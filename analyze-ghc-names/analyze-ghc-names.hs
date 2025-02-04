-- This program collects a few statistics from the files in the data folder.
--
-- It prints the list of most commonly used modules from the GHC API, and the
-- list of most commonly used identifiers.
--
-- Run with
--
-- > runghc analyze-ghc-names.hs | less
--

import Data.Function
import Data.List
import System.Directory
import System.FilePath

data Row = Row
    { rowUnit :: String
    , rowModule :: String
    , rowName :: String
    }

main = do
    let dataDir = "data"
    fileNames <- listDirectory dataDir
    let txts = filter (".txt" `isSuffixOf`) fileNames
    contents <- mapM (readFile . (dataDir </>))  txts :: IO [String]
    let rows = map parseRow $ concatMap lines contents
        rowsPerUnit =
          groupBy ((==) `on` rowUnit) $
          sortOn rowUnit rows
        moduleRows =
          -- Let modules appear at most once per unit
          sort $ concatMap (nub . map rowModule) rowsPerUnit
        nameRowsPerModule =
          -- Let names appear at most once per unit
          sort $ concatMap (map rowName) rowsPerUnit
        nameRowsPerUnit =
          -- Let names appear at most once per unit
          sort $ concatMap (nub . map rowName) rowsPerUnit
        -- Count the # of units a modules appears in
        moduleStatsPerUnit =
          sortBy
            (flip compare `on` snd)
            [ (head g, length g) | g <- group moduleRows ]
        -- Count the # of units a name appears in
        nameStatsPerUnit =
          sortBy
            (flip compare `on` snd)
            [ (head g, length g) | g <- group nameRowsPerUnit ]
        -- Count the # of units a name appears in
        nameStatsPerModule =
          sortBy
            (flip compare `on` snd)
            [ (head g, length g) | g <- group nameRowsPerModule ]
        units = map head $ group $ sort $ map rowUnit rows
        nunits = length units
        p20 = div nunits 5
        nmodules = length moduleStatsPerUnit
        m20 = div nmodules 5
        nnames = length nameStatsPerUnit

    putStrLn $ unwords
      [show nunits, "analyzed units:"]
    mapM_ putStrLn units

    putStrLn ""
    putStrLn $ unwords
      ["Most commonly used GHC modules (of a total of", show nmodules, "modules)"]
    putStrLn "(module name, # of units in which it appears)"
    mapM_ print $ takeWhile ((> p20) . snd) moduleStatsPerUnit

    putStrLn ""
    putStrLn $ unwords
      ["Most commonly used GHC names (of a total of", show nnames, "names)"]
    putStrLn "(GHC name, # of units in which it appears)"
    mapM_ print $ takeWhile ((>p20) . snd) nameStatsPerUnit

    putStrLn ""
    putStrLn $ unwords
      ["Most commonly used GHC names (of a total of", show nnames, "names)"]
    putStrLn "(GHC name, # of modules in which it appears)"
    mapM_ print $ takeWhile ((>m20) . snd) nameStatsPerModule

parseRow s0 =
    let (_, _:s1) = break (':'==) s0
        (p, _:s2) = break (':'==) s1
        (_, _:s3) = break ('.'==) (reverse s2)
     in Row
         { rowUnit = p
         , rowModule = reverse s3
         , rowName = s2
         }
