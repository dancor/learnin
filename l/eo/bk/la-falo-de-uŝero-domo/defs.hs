import Control.Applicative
import Data.Maybe
import FUtil
import HSH
import System.Environment

tryIndex n [] = Nothing
tryIndex 0 (x:_) = Just x
tryIndex n (_:xs) = tryIndex (n - 1) xs

-- fixme: use fmap?
onJust f (Just x) = Just $ f x
onJust _ Nothing = Nothing

main :: IO ()
main = do
  args <- getArgs
  let [f] = args
  --l <- take 50 . lines . fromUtf <$> readFile f
  l <- lines . fromUtf <$> readFile f
  let wds = map (takeWhile (/= ',') . (!! 1) . words) l
  wdDefs <- mapM 
    (\ wd -> onJust ((,) wd . dropWhile (== ' ') . fromUtf) . tryIndex 1 <$> 
      run ("eo", ["-e", toUtf wd])) 
    wds
  let strs = map (\ (w, d) -> w ++ "|" ++ d) $ catMaybes wdDefs
  putStr . toUtf $ unlines strs
