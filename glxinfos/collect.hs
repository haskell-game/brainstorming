import Control.Arrow      (first)
import Control.Monad      (forM_)
import Data.Char          (isAlphaNum)
import Data.List          (isPrefixOf, nub, sortBy)
import Data.Tuple         (swap)
import System.Environment (getArgs)

import qualified Data.Map as M

main :: IO ()
main = do
    (options, filenames) <- span ("--" `isPrefixOf`) `fmap` getArgs
    bodies <- mapM readFile filenames
    let extss = map extract bodies
        pcts  = calcPcts extss
    if "--lcd" `elem` options
      then forM_ (takeWhile ((100 ==) . fst) pcts) $ \(_, ext) ->
             putStrLn ext
      else forM_ pcts $ \(pct, ext) ->
             putStrLn $ show pct ++ "% " ++ ext

extract :: String -> [String]
extract =
    nub . map clean . filter interesting . words
  where
    clean         = takeWhile (\c -> isAlphaNum c || c == '_')
    interesting w = (`isPrefixOf` w) `any` extPrefixes
    extPrefixes   = ["GL_", "GLU_", "GLX_", "WGL_"]

calcPcts :: [[String]] -> [(Integer, String)]
calcPcts extss =
    let m     = foldl insert M.empty extss
        freqs = map swap $ M.toList m
        pcts  = map (first toPct) freqs
    in sortBy pctDesc pcts
  where
    insert =
        foldl (\m e -> M.insertWith (+) e one m)
      where
        one = 1 :: Integer

    pctDesc (pct0, ext0) (pct1, ext1)
      | pct0 > pct1 = LT
      | pct0 < pct1 = GT
      | otherwise = compare ext0 ext1

    toPct freq =
        floor $ oneHundred * fromIntegral freq / num
      where
        oneHundred = 100 :: Double
        num = fromIntegral $ length extss
