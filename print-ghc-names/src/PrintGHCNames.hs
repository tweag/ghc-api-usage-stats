module PrintGHCNames (plugin) where

import Control.Monad.IO.Class
import Data.Data (Data, gmapQr)
import Data.Generics (extQ)
import Data.List
import GHC
import GHC.Driver.Session
import GHC.Driver.Plugins
import GHC.Tc.Types
import GHC.Types.Name
import GHC.Unit.Types

plugin :: Plugin
plugin = defaultPlugin
    { renamedResultAction = printGHCNames
    , pluginRecompile = purePlugin
    }
  where
    printGHCNames
      :: [CommandLineOption]
      -> TcGblEnv
      -> HsGroup GhcRn
      -> TcM (TcGblEnv, HsGroup GhcRn)
    printGHCNames _ tcg g = do
      dflags <- getDynFlags
      liftIO $ printNames dflags $ collectGHCNames g
      return (tcg, g)

collectGHCNames :: HsGroup GhcRn -> [Name]
collectGHCNames = filter isGHCName . collectNames
  where
    isGHCName n = case nameModule_maybe n of
      Nothing -> False
      Just m ->
        let u = unitIdString (moduleUnitId m)
         in u == "ghc" ||
            "ghc-9" `isPrefixOf` u ||
            "ghc-lib-parser-" `isPrefixOf` u

collectNames :: Data a => a -> [Name]
collectNames = gmapQr (++) [] collectNames `extQ` (:[])

-- | Prints the given names after removing duplicates.
--
-- At the moment this function is called once per 'HsGroup',
-- which means that the same name can be still printed more
-- than once if it appears in different groups.
printNames :: DynFlags -> [Name] -> IO ()
printNames dflags names =
    mapM_ putStrLn $
      concat $
      map (take 1) $
      group $
      sort $
      map (("print-ghc-names:" ++) . showName dflags) names

showName :: DynFlags -> Name -> String
showName dflags n = case nameModule_maybe n of
    Nothing -> getOccString n
    Just m -> unitIdString (homeUnitId_ dflags) ++":"++ moduleNameString (moduleName m) ++ "." ++ getOccString n
