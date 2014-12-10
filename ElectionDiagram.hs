{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main :: IO ()
main = mainWith . pad 1.1 $ senate
     
senate :: Diagram B R2
senate = mconcat . map (uncurry seatRow) 
                 $ zip [23, 26, 28, 31, 34, 36, 39, 41, 44, 47]
                       [1,1+1/9..]

seatRow :: Int -> Double -> Diagram B R2
seatRow n r = centerX $ decorateTrail (seatRowTrail n r) (repeat node) 

node :: Diagram B R2
node = circle (2/45) # fc green

seatRowTrail :: (TrailLike t, V t ~ R2) => Int -> Double -> t
seatRowTrail n r = polygon (with & polyType .~ PolyPolar (repeat angle)
                                                         (replicate n r))
    where angle = (1/(2*fromIntegral n) @@ turn)
