{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module IO.Config where

import ClassyPrelude

import qualified Data.Text.IO       as T (readFile)
import           System.Directory   (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                                     getHomeDirectory)
import           System.Environment (lookupEnv)

import Brick           (AttrMap)
import Brick.Themes    (loadCustomizations, themeToAttrMap)
import Data.FileEmbed  (embedFile)
import Data.Ini.Config

import IO.Keyboard        (addMissing, badMapping, defaultBindings)
import IO.Keyboard.Parser (bindings)
import IO.Keyboard.Types  (Bindings)
import UI.Theme           (defaultTheme)

import qualified IO.Config.General  as General
import qualified IO.Config.GitHub   as GitHub
import qualified IO.Config.Layout   as Layout
import qualified IO.Config.Markdown as Markdown

data Config = Config
    { general  :: General.Config
    , layout   :: Layout.Config
    , markdown :: Markdown.Config
    , github   :: GitHub.Config
    }

defaultConfig :: Config
defaultConfig =
    Config
        General.defaultConfig
        Layout.defaultConfig
        Markdown.defaultConfig
        GitHub.defaultConfig

directoryName :: FilePath
directoryName = "taskell"

legacyConfigPath :: IO FilePath
legacyConfigPath = (</> "." <> directoryName) <$> getHomeDirectory

xdgDefaultConfig :: IO FilePath
xdgDefaultConfig = (</> ".config") <$> getHomeDirectory

xdgConfigPath :: IO FilePath
xdgConfigPath =
    (</> directoryName) <$> (fromMaybe <$> xdgDefaultConfig <*> lookupEnv "XDG_CONFIG_HOME")

getDir :: IO FilePath
getDir = legacyConfigPath >>= doesDirectoryExist >>= bool xdgConfigPath legacyConfigPath

themePath :: FilePath -> FilePath
themePath = (</> "theme.ini")

configPath :: FilePath -> FilePath
configPath = (</> "config.ini")

bindingsPath :: FilePath -> FilePath
bindingsPath = (</> "bindings.ini")

setup :: IO Config
setup
    -- create config dir
 = do
    dir <- getDir
    createDirectoryIfMissing True dir
    -- create config files
    create (configPath dir) $(embedFile "templates/config.ini")
    create (themePath dir) $(embedFile "templates/theme.ini")
    create (bindingsPath dir) $(embedFile "templates/bindings.ini")
    -- get config
    getConfig

create :: FilePath -> ByteString -> IO ()
create path contents = doesFileExist path >>= flip unless (writeFile path contents)

configParser :: IniParser Config
configParser =
    Config <$> General.parser <*> Layout.parser <*> Markdown.parser <*> GitHub.parser

getConfig :: IO Config
getConfig = do
    content <- T.readFile =<< (configPath <$> getDir)
    case parseIniFile content configParser of
        Right config -> pure config
        Left s       -> putStrLn (pack $ "config.ini: " <> s) $> defaultConfig

getBindings :: IO Bindings
getBindings = do
    bnds <- bindings <$> (T.readFile =<< (bindingsPath <$> getDir))
    case addMissing <$> (badMapping =<< bnds) of
        Right b -> pure b
        Left err ->
            putStrLn ("bindings.ini: " <> err <> " - using default bindings") $> defaultBindings

-- generate theme
generateAttrMap :: IO AttrMap
generateAttrMap = do
    dir <- getDir
    customizedTheme <- loadCustomizations (themePath dir) defaultTheme
    case customizedTheme of
        Right theme -> pure $ themeToAttrMap theme
        Left s      -> putStrLn (pack $ "theme.ini: " <> s) $> themeToAttrMap defaultTheme
