{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad
import Data.Bool (bool)
import Data.Either (lefts)
import Data.Functor.Identity (Identity (..))
import Data.List (intercalate, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Development.GitRev
import Options.Applicative
import Ormolu
import Ormolu.Config
import Ormolu.Parser (manualExts)
import Ormolu.Utils (showOutputable)
import Paths_fourmolu (version)
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

-- | Entry point of the program.
main :: IO ()
main = withPrettyOrmoluExceptions $ do
  opts@Opts {..} <- execParser optsParserInfo
  let formatStdIn = do
        cur <- getCurrentDirectory
        cfg <- mkConfig cur opts
        formatOne optMode cfg Nothing
  case optInputFiles of
    [] -> formatStdIn
    ["-"] -> formatStdIn
    [x] -> flip (formatOne optMode) (Just x) =<< mkConfig x opts
    xs@(x : _) -> do
      cfg <- mkConfig x opts
      -- It is possible to get IOException, error's and 'OrmoluException's
      -- from 'formatOne', so we just catch everything.
      errs <-
        lefts
          <$> mapM
            (try @SomeException . formatOne optMode cfg . Just)
            (sort xs)
      unless (null errs) $ do
        mapM_ (hPutStrLn stderr . displayException) errs
        exitWith (ExitFailure 102)

-- | Format a single input.
formatOne ::
  -- | Mode of operation
  Mode ->
  -- | Configuration
  Config RegionIndices ->
  -- | File to format or stdin as 'Nothing'
  Maybe FilePath ->
  IO ()
formatOne mode config = \case
  Nothing -> do
    r <- ormoluStdin config
    case mode of
      Stdout -> TIO.putStr r
      _ -> do
        hPutStrLn
          stderr
          "This feature is not supported when input comes from stdin."
        -- 101 is different from all the other exit codes we already use.
        exitWith (ExitFailure 101)
  Just inputFile -> do
    originalInput <- TIO.readFile inputFile
    formattedInput <- ormoluFile config inputFile
    case mode of
      Stdout ->
        TIO.putStr formattedInput
      InPlace -> do
        -- Only write when the contents have changed, in order to avoid
        -- updating the modified timestamp if the file was already correctly
        -- formatted.
        when (formattedInput /= originalInput) $
          TIO.writeFile inputFile formattedInput
      Check -> do
        when (formattedInput /= originalInput) $
          -- 100 is different to all the other exit code that are emitted
          -- either from an 'OrmoluException' or from 'error' and
          -- 'notImplemented'.
          exitWith (ExitFailure 100)

----------------------------------------------------------------------------
-- Command line options parsing.

data Opts = Opts
  { -- | Mode of operation
    optMode :: !Mode,
    -- | Ormolu 'Config'
    optConfig :: !(Config RegionIndices),
    -- | Fourmolu-specific options
    optPrinterOpts :: !PrinterOptsPartial,
    -- | Haskell source files to format or stdin (when the list is empty)
    optInputFiles :: ![FilePath]
  }

-- | Mode of operation.
data Mode
  = -- | Output formatted source code to stdout
    Stdout
  | -- | Overwrite original file
    InPlace
  | -- | Exit with non-zero status code if
    -- source is not already formatted
    Check
  deriving (Eq, Show, Bounded, Enum)

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> ver <*> exts <*> optsParser) . mconcat $
    [ fullDesc,
      progDesc "",
      header ""
    ]
  where
    ver :: Parser (a -> a)
    ver =
      infoOption verStr . mconcat $
        [ long "version",
          short 'v',
          help "Print version of the program"
        ]
    verStr =
      intercalate
        "\n"
        [ unwords
            [ "fourmolu",
              showVersion version,
              $gitBranch,
              $gitHash
            ],
          "using ghc-lib-parser " ++ VERSION_ghc_lib_parser
        ]
    exts :: Parser (a -> a)
    exts =
      infoOption displayExts . mconcat $
        [ long "manual-exts",
          help "Display extensions that need to be enabled manually"
        ]
    displayExts = unlines $ sort (showOutputable <$> manualExts)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> ( (fmap (bool Stdout InPlace) . switch . mconcat)
            [ short 'i',
              help "A shortcut for --mode inplace"
            ]
            <|> (option parseBoundedEnum . mconcat)
              [ long "mode",
                short 'm',
                metavar "MODE",
                value Stdout,
                help "Mode of operation: 'stdout' (default), 'inplace', or 'check'"
              ]
        )
    <*> configParser
    <*> printerOptsParser
    <*> (many . strArgument . mconcat)
      [ metavar "FILE",
        help "Haskell source files to format or stdin (default)"
      ]

configParser :: Parser (Config RegionIndices)
configParser =
  Config
    <$> (fmap (fmap DynOption) . many . strOption . mconcat)
      [ long "ghc-opt",
        short 'o',
        metavar "OPT",
        help "GHC options to enable (e.g. language extensions)"
      ]
    <*> (switch . mconcat)
      [ long "unsafe",
        short 'u',
        help "Do formatting faster but without automatic detection of defects"
      ]
    <*> (switch . mconcat)
      [ long "debug",
        short 'd',
        help "Output information useful for debugging"
      ]
    <*> (switch . mconcat)
      [ long "check-idempotence",
        short 'c',
        help "Fail if formatting is not idempotent"
      ]
    <*> ( RegionIndices
            <$> (optional . option auto . mconcat)
              [ long "start-line",
                metavar "START",
                help "Start line of the region to format (starts from 1)"
              ]
            <*> (optional . option auto . mconcat)
              [ long "end-line",
                metavar "END",
                help "End line of the region to format (inclusive)"
              ]
        )
    <*> pure defaultPrinterOpts

printerOptsParser :: Parser PrinterOptsPartial
printerOptsParser = do
  poIndentation <-
    (optional . option auto . mconcat)
      [ long "indentation",
        metavar "WIDTH",
        help $
          "Number of spaces per indentation step"
            <> showDefaultValue poIndentation
      ]
  poCommaStyle <-
    (optional . option parseBoundedEnum . mconcat)
      [ long "comma-style",
        metavar "STYLE",
        help $
          "How to place commas in multi-line lists, records etc: "
            <> showAllValues @CommaStyle
            <> showDefaultValue poCommaStyle
      ]
  poIndentWheres <-
    (optional . option parseBoundedEnum . mconcat)
      [ long "indent-wheres",
        metavar "BOOL",
        help $
          "Whether to indent 'where' bindings past the preceding body"
            <> " (rather than half-indenting the 'where' keyword)"
            <> showDefaultValue poIndentWheres
      ]
  poRecordBraceSpace <-
    (optional . option parseBoundedEnum . mconcat)
      [ long "record-brace-space",
        metavar "BOOL",
        help $
          "Whether to leave a space before an opening record brace"
            <> showDefaultValue poRecordBraceSpace
      ]
  poDiffFriendlyImportExport <-
    (optional . option parseBoundedEnum . mconcat)
      [ long "diff-friendly-import-export",
        metavar "BOOL",
        help $
          "Whether to make use of extra commas in import/export lists"
            <> " (as opposed to Ormolu's style)"
            <> showDefaultValue poDiffFriendlyImportExport
      ]
  poRespectful <-
    (optional . option parseBoundedEnum . mconcat)
      [ long "respectful",
        metavar "BOOL",
        help $
          "Give the programmer more choice on where to insert blank lines"
            <> showDefaultValue poRespectful
      ]
  poHaddockStyle <-
    (optional . option parseBoundedEnum . mconcat)
      [ long "haddock-style",
        metavar "STYLE",
        help $
          "How to print Haddock comments: "
            <> showAllValues @HaddockPrintStyle
            <> showDefaultValue poHaddockStyle
      ]
  poNewlinesBetweenDecls <-
    (optional . option auto . mconcat)
      [ long "newlines-between-decls",
        metavar "HEIGHT",
        help $
          "Number of spaces between top-level declarations"
            <> showDefaultValue poNewlinesBetweenDecls
      ]
  pure PrinterOpts {..}

----------------------------------------------------------------------------
-- Helpers

-- | A standard parser of CLI option arguments, applicable to arguments that
-- have a finite (preferably small) number of possible values. (Basically an
-- inverse of 'toCLIArgument'.)
parseBoundedEnum ::
  forall a.
  (Enum a, Bounded a, ToCLIArgument a) =>
  ReadM a
parseBoundedEnum =
  eitherReader
    ( \s ->
        case lookup s argumentToValue of
          Just v -> Right v
          Nothing ->
            Left $
              "unknown value: '"
                <> s
                <> "'\nValid values are: "
                <> showAllValues @a
                <> "."
    )
  where
    argumentToValue = map (\x -> (toCLIArgument x, x)) [minBound ..]

-- | Values that appear as arguments of CLI options and thus have
-- a corresponding textual representation.
class ToCLIArgument a where
  -- | Convert a value to its representation as a CLI option argument.
  toCLIArgument :: a -> String

  -- | Convert a value to its representation as a CLI option argument wrapped
  -- in apostrophes.
  toCLIArgument' :: a -> String
  toCLIArgument' x = "'" <> toCLIArgument x <> "'"

instance ToCLIArgument Bool where
  toCLIArgument True = "true"
  toCLIArgument False = "false"

instance ToCLIArgument CommaStyle where
  toCLIArgument Leading = "leading"
  toCLIArgument Trailing = "trailing"

instance ToCLIArgument Int where
  toCLIArgument = show

instance ToCLIArgument HaddockPrintStyle where
  toCLIArgument HaddockSingleLine = "single-line"
  toCLIArgument HaddockMultiLine = "multi-line"

instance ToCLIArgument Mode where
  toCLIArgument Stdout = "stdout"
  toCLIArgument InPlace = "inplace"
  toCLIArgument Check = "check"

showAllValues :: forall a. (Enum a, Bounded a, ToCLIArgument a) => String
showAllValues = format (map toCLIArgument' [(minBound :: a) ..])
  where
    format [] = []
    format [x] = x
    format [x1, x2] = x1 <> " or " <> x2
    format (x : xs) = x <> ", " <> format xs

-- | CLI representation of the default value of an option, formatted for
-- inclusion in the help text.
showDefaultValue ::
  ToCLIArgument a =>
  (PrinterOptsTotal -> Identity a) ->
  String
showDefaultValue =
  (" (default " <>)
    . (<> ")")
    . toCLIArgument'
    . runIdentity
    . ($ defaultPrinterOpts)

-- | Build the full config, by adding 'PrinterOpts' from a file, if found.
mkConfig :: FilePath -> Opts -> IO (Config RegionIndices)
mkConfig path Opts {..} = do
  filePrinterOpts <-
    loadConfigFile path >>= \case
      ConfigLoaded f po -> do
        printDebug $ "Loaded config from: " <> f
        printDebug $ show po
        return $ Just po
      ConfigParseError f (_pos, err) -> do
        -- we ignore '_pos' due to the note on 'Data.YAML.Aeson.decode1'
        hPutStrLn stderr $
          unlines
            [ "Failed to load " <> f <> ":",
              "  " <> err
            ]
        exitWith $ ExitFailure 400
      ConfigNotFound searchDirs -> do
        printDebug
          . unlines
          $ ("No " ++ show configFileName ++ " found in any of:") :
          map ("  " ++) searchDirs
        return Nothing
  return $
    optConfig
      { cfgPrinterOpts =
          fillMissingPrinterOpts
            (optPrinterOpts <> fromMaybe mempty filePrinterOpts)
            (cfgPrinterOpts optConfig)
      }
  where
    printDebug = when (cfgDebug optConfig) . hPutStrLn stderr
