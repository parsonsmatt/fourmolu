{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Printing combinators.

module Ormolu.Printer.Combinators
  ( -- * The 'R' monad
    R
  , runR
    -- * Combinators
    -- ** Basic
  , txt
  , atom
  , newline
  , inci
  , relaxComments
  , located
  , locatedVia
  , located'
  , switchLayout
  , velt
  , velt'
  , vlayout
  , breakpoint
  , withSep
  , spaceSep
  , newlineSep
    -- ** Wrapping
  , line
  , backticks
  , braces
  , brackets
  , bracketsPar
  , parens
  , parensHash
    -- ** Literals
  , comma
  , space
  )
where

import Data.Bool (bool)
import Data.Data (Data)
import Data.List (intersperse)
import Data.Text (Text)
import Language.Haskell.GHC.ExactPrint.Types
import Ormolu.Printer.Comments
import Ormolu.Printer.Internal
import Outputable (Outputable (..), showSDocUnsafe)
import SrcLoc
import qualified Data.Text as T

----------------------------------------------------------------------------
-- Basic

-- | Output a fixed 'Text' fragment. The argument may not contain any line
-- breaks or tab characters.

txt :: Text -> R ()
txt t = ensureIndent >> spit t

-- | Output 'Outputable' fragment of AST.

atom :: Outputable a => a -> R ()
atom = txt . T.pack . showSDocUnsafe . ppr

-- | Enter a 'Located' entity. This primitive handles outputting comments
-- that may be associated with the primitive and sets corresponding layout
-- for the inner computation.

located
  :: Data a
  => Located a                  -- ^ Thing to enter
  -> (a -> R ())                -- ^ How to render inner value
  -> R ()
located loc@(L l _) = locatedVia (Just l) loc

-- | A special version of 'located' that allows to control layout using
-- externally provided span. 'Nothing' means that layout won't be changed.

locatedVia
  :: Data a
  => Maybe SrcSpan              -- ^ Span that controls layout selection
  -> Located a                  -- ^ Thing to enter
  -> (a -> R ())                -- ^ How to render inner value
  -> R ()
locatedVia ml loc@(L l a) f = do
  mann <- lookupAnn loc
  relaxed <- relaxedComments
  let m = case ml of
       Nothing -> f a
       Just l' -> switchLayout l' (f a)
  case mann of
    Nothing -> m
    Just Ann {..} -> bool sitcc id relaxed $ do
      -- There are three things in 'Ann' which contain comments:

      let cmode =
            if annGetConstr a == CN "HsModule"
              then Module
              else Other
          (before, after) = partitionDPs cmode l annsDP

      -- 'annPriorComments' contains comments that were directly placed
      -- before entities such as comments (in both styles) before function
      -- definitions and inline comments before smaller things like types
      -- and literals.

      spitComments (addDecoration cmode Before l <$> annPriorComments)

      -- Comments inside 'annsDP' marked with 'AnnComment' are trickier,
      -- they seem to contain everything that goes after the thing they
      -- are attached to and in some cases (e.g. for modules) they contain
      -- comments that go before things. Exact location can only be
      -- deduced by analyzing the associated span.

      spitComments before
      m
      spitComments after

      -- I wasn't able to find any case when 'annFollowingComments' is
      -- populated, so we'll ignore that one for now and fix it when we
      -- have an example of source code where it matters.

-- | A version of 'located' with arguments flipped.

located'
  :: Data a
  => (a -> R ())                -- ^ How to render inner value
  -> Located a                  -- ^ Thing to enter
  -> R ()
located' = flip located

-- | Set layout according to given 'SrcSpan' for a given computation. Use
-- this only when you need to set layout based on e.g. combined span of
-- several elements when there is no corresponding 'Located' wrapper
-- provided by GHC AST.

switchLayout
  :: SrcSpan                    -- ^ Span that controls layout
  -> R ()                       -- ^ Computation to run with changed layout
  -> R ()
switchLayout spn = enterLayout
  (if isOneLineSpan spn
    then SingleLine
    else MultiLine)

-- | Element of variable layout. This means that the sub-components may be
-- rendered either on single line or each on its own line depending on
-- current layout.
--
-- This version does not make subsequent element (second and later) align
-- with the first automatically and does not insert spaces between elements
-- when layout is single line.

velt :: [R ()] -> R ()
velt xs = sequence_ (intersperse sep (sitcc <$> xs))
  where
    sep = vlayout (pure ()) newline

-- | Like 'velt', but all sub-elements start at the same indentation level
-- as first element, additionally spaces are inserted when layout is single
-- line.

velt' :: [R ()] -> R ()
velt' xs = sitcc $ sequence_ (intersperse sep (sitcc <$> xs))
  where
    sep = vlayout (spit " ") newline

-- | Insert a space if enclosing layout is single-line, or newline if it's
-- multiline.

breakpoint :: R ()
breakpoint = vlayout space newline

-- | Put separator between renderings of items of a list.

withSep
  :: R ()                       -- ^ Separator
  -> (a -> R ())                -- ^ How to render list items
  -> [a]                        -- ^ List to render
  -> [R ()]                     -- ^ List of printing actions
withSep sep f = \case
  [] -> []
  (x:xs) ->
    let g a = sep >> f a
    in f x : fmap g xs

-- | Render space-separated elements.

spaceSep
  :: (a -> R ())                -- ^ How to render list items
  -> [a]                        -- ^ List to render
  -> R ()
spaceSep f = sequence_ . withSep space f

-- | Render newline-separated elements.

newlineSep
  :: (a -> R ())                -- ^ How to render list items
  -> [a]                        -- ^ List to render
  -> R ()
newlineSep f = sequence_ . withSep newline f

----------------------------------------------------------------------------
-- Wrapping

-- | Finish given entity by a 'newline'.

line :: R () -> R ()
line m = do
  m
  newline

-- | Surround given entity by backticks.

backticks :: R () -> R ()
backticks m = txt "`" >> m >> txt "`"

-- | Surround given entity by curly braces.

braces :: R () -> R ()
braces m = sitcc $ do
  txt "{"
  ospaces m
  txt "}"

-- | Surround given entity by square brackets.

brackets :: R () -> R ()
brackets m = sitcc $ do
  txt "["
  ospaces m
  txt "]"

-- | Surround given entity by parallel array brackets @[:@ ond @:]@.

bracketsPar :: R () -> R ()
bracketsPar m = sitcc $ do
  txt "[:"
  m
  txt ":]"

-- | Surround given entity by parentheses.

parens :: R () -> R ()
parens m = sitcc $ do
  txt "("
  ospaces m
  txt ")"

-- | Surround given entity by @(# @ and @ #)@.

parensHash :: R () -> R ()
parensHash m = sitcc $ do
  txt "(# "
  m
  txt " #)"

-- | Surround given entity by optional space before and a newline after, iff
-- current layout is multiline.

ospaces :: R () -> R ()
ospaces m = vlayout m (txt " " >> m >> newline)

----------------------------------------------------------------------------
-- Literals

-- | Print @,@ followed by a space.

comma :: R ()
comma = txt ", "

-- | Print single space.

space :: R ()
space = txt " "
