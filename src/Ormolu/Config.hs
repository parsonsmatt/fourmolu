{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Configuration options used by the tool.
module Ormolu.Config
  ( Config (..),
    RegionIndices (..),
    RegionDeltas (..),
    defaultConfig,
    PrinterOpts (..),
    PrinterOptsPartial,
    PrinterOptsTotal,
    defaultPrinterOpts,
    loadConfigFile,
    configFileName,
    ConfigFileLoadResult (..),
    fillMissingPrinterOpts,
    CommaStyle (..),
    HaddockPrintStyle (..),
    regionIndicesToDeltas,
    DynOption (..),
    dynOptionToLocatedStr,
  )
where

import Data.Aeson
  ( FromJSON (..),
    camelTo2,
    constructorTagModifier,
    defaultOptions,
    fieldLabelModifier,
    genericParseJSON,
  )
import qualified Data.ByteString.Lazy as BS
import Data.Char (isLower)
import Data.Functor.Identity (Identity (..))
import Data.YAML (Pos)
import Data.YAML.Aeson (decode1)
import GHC.Generics (Generic)
import qualified SrcLoc as GHC
import System.Directory
  ( XdgDirectory (XdgConfig),
    findFile,
    getXdgDirectory,
    makeAbsolute,
  )
import System.FilePath (splitPath, (</>))

-- | Ormolu configuration.
data Config region = Config
  { -- | Dynamic options to pass to GHC parser
    cfgDynOptions :: ![DynOption],
    -- | Do formatting faster but without automatic detection of defects
    cfgUnsafe :: !Bool,
    -- | Output information useful for debugging
    cfgDebug :: !Bool,
    -- | Checks if re-formatting the result is idempotent
    cfgCheckIdempotence :: !Bool,
    -- | Region selection
    cfgRegion :: !region,
    cfgPrinterOpts :: !PrinterOptsTotal
  }
  deriving (Eq, Show, Functor)

-- | Region selection as the combination of start and end line numbers.
data RegionIndices = RegionIndices
  { -- | Start line of the region to format
    regionStartLine :: !(Maybe Int),
    -- | End line of the region to format
    regionEndLine :: !(Maybe Int)
  }
  deriving (Eq, Show)

-- | Region selection as the length of the literal prefix and the literal
-- suffix.
data RegionDeltas = RegionDeltas
  { -- | Prefix length in number of lines
    regionPrefixLength :: !Int,
    -- | Suffix length in number of lines
    regionSuffixLength :: !Int
  }
  deriving (Eq, Show)

-- | Default @'Config' 'RegionIndices'@.
defaultConfig :: Config RegionIndices
defaultConfig =
  Config
    { cfgDynOptions = [],
      cfgUnsafe = False,
      cfgDebug = False,
      cfgCheckIdempotence = False,
      cfgRegion =
        RegionIndices
          { regionStartLine = Nothing,
            regionEndLine = Nothing
          },
      cfgPrinterOpts = defaultPrinterOpts
    }

-- | Options controlling formatting output.
data PrinterOpts f = PrinterOpts
  { -- | Number of spaces to use for indentation
    poIndentation :: f Int,
    -- | Whether to place commas at start or end of lines
    poCommaStyle :: f CommaStyle,
    -- | Whether to indent `where` blocks
    poIndentWheres :: f Bool,
    -- | Break after a `let`
    poLetNewline :: f Bool,
    -- | Leave space before opening record brace
    poRecordBraceSpace :: f Bool,
    -- | Trailing commas with parentheses on separate lines
    poDiffFriendlyImportExport :: f Bool,
    -- | Be less opinionated about spaces/newlines etc.
    poRespectful :: f Bool,
    -- | How to print doc comments
    poHaddockStyle :: f HaddockPrintStyle,
    -- | Number of newlines between top-level decls
    poNewlinesBetweenDecls :: f Int
  }
  deriving (Generic)

-- | A version of 'PrinterOpts' where any field can be empty.
-- This corresponds to the information in a config file or in CLI options.
type PrinterOptsPartial = PrinterOpts Maybe

deriving instance Eq PrinterOptsPartial

deriving instance Show PrinterOptsPartial

instance Semigroup PrinterOptsPartial where
  (<>) = fillMissingPrinterOpts

instance Monoid PrinterOptsPartial where
  mempty = PrinterOpts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | A version of 'PrinterOpts' without empty fields.
type PrinterOptsTotal = PrinterOpts Identity

deriving instance Eq PrinterOptsTotal

deriving instance Show PrinterOptsTotal

defaultPrinterOpts :: PrinterOptsTotal
defaultPrinterOpts =
  PrinterOpts
    { poIndentation = pure 4,
      poCommaStyle = pure Leading,
      poIndentWheres = pure False,
      poLetNewline = pure False,
      poRecordBraceSpace = pure False,
      poDiffFriendlyImportExport = pure True,
      poRespectful = pure True,
      poHaddockStyle = pure HaddockMultiLine,
      poNewlinesBetweenDecls = pure 1
    }

-- | Fill the field values that are 'Nothing' in the first argument
-- with the values of the corresponding fields of the second argument.
fillMissingPrinterOpts ::
  forall f.
  Applicative f =>
  PrinterOptsPartial ->
  PrinterOpts f ->
  PrinterOpts f
fillMissingPrinterOpts p1 p2 =
  PrinterOpts
    { poIndentation = fillField poIndentation,
      poCommaStyle = fillField poCommaStyle,
      poIndentWheres = fillField poIndentWheres,
      poLetNewline = fillField poLetNewline,
      poRecordBraceSpace = fillField poRecordBraceSpace,
      poDiffFriendlyImportExport = fillField poDiffFriendlyImportExport,
      poRespectful = fillField poRespectful,
      poHaddockStyle = fillField poHaddockStyle,
      poNewlinesBetweenDecls = fillField poNewlinesBetweenDecls
    }
  where
    fillField :: (forall g. PrinterOpts g -> g a) -> f a
    fillField f = maybe (f p2) pure $ f p1

data CommaStyle
  = Leading
  | Trailing
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

instance FromJSON CommaStyle where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = camelTo2 '-'
        }

data HaddockPrintStyle
  = HaddockSingleLine
  | HaddockMultiLine
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

instance FromJSON HaddockPrintStyle where
  parseJSON =
    genericParseJSON
      defaultOptions
        { constructorTagModifier = drop (length "haddock-") . camelTo2 '-'
        }

-- | Convert 'RegionIndices' into 'RegionDeltas'.
regionIndicesToDeltas ::
  -- | Total number of lines in the input
  Int ->
  -- | Region indices
  RegionIndices ->
  -- | Region deltas
  RegionDeltas
regionIndicesToDeltas total RegionIndices {..} =
  RegionDeltas
    { regionPrefixLength = maybe 0 (subtract 1) regionStartLine,
      regionSuffixLength = maybe 0 (total -) regionEndLine
    }

-- | A wrapper for dynamic options.
newtype DynOption = DynOption
  { unDynOption :: String
  }
  deriving (Eq, Ord, Show)

-- | Convert 'DynOption' to @'GHC.Located' 'String'@.
dynOptionToLocatedStr :: DynOption -> GHC.Located String
dynOptionToLocatedStr (DynOption o) = GHC.L GHC.noSrcSpan o

instance FromJSON PrinterOptsPartial where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '-' . dropWhile isLower
        }

-- | Read options from a config file, if found.
-- Looks recursively in parent folders, then in 'XdgConfig',
-- for a file named /fourmolu.yaml/.
loadConfigFile :: FilePath -> IO ConfigFileLoadResult
loadConfigFile path = do
  root <- makeAbsolute path
  xdg <- getXdgDirectory XdgConfig ""
  let dirs = reverse $ xdg : scanl1 (</>) (splitPath root)
  findFile dirs configFileName >>= \case
    Nothing -> return $ ConfigNotFound dirs
    Just file ->
      either (ConfigParseError file) (ConfigLoaded file)
        . decode1
        <$> BS.readFile file

-- | The result of calling 'loadConfigFile'.
data ConfigFileLoadResult
  = ConfigLoaded FilePath PrinterOptsPartial
  | ConfigParseError FilePath (Pos, String)
  | ConfigNotFound [FilePath]
  deriving (Eq, Show)

-- | Expected file name for YAML config.
configFileName :: FilePath
configFileName = "fourmolu.yaml"
