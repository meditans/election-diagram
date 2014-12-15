{-# LANGUAGE TypeFamilies #-}

import Data.List
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
                  

------------------------------ GALLIANI ------------------------------

-- Dati iniziali
type Seats = Int
type Rows  = Int
data Colore = Red | Yellow | Blue | Green

nArchi :: Rows
nArchi = 13

nPalline :: Seats
nPalline = 630
           
partiti :: [(Seats,Colore)] 
partiti = [(200, Red), (200, Yellow), (130, Blue), (100, Green)]

-- Raggi delle circonferenze
radii :: [Double]
radii = [1, 1+1/(fromIntegral nArchi-1) .. 2]

inverses :: [Double]
inverses = map (\a -> 1 / fromIntegral a) [1 .. nPalline `div` nArchi]

-- Funzioni utili -- Non so come scriverle
cumula :: [Seats]->[Seats]
cumula = tail . map sum . inits

decumula :: [Seats]->[Seats]
decumula xs = head xs : zipWith (-) (tail xs) xs

--Decumulare lista (l'inversa)
-- Esempio
--Cumula . Map first $ partiti = [200,400,530,630]

pallineCumulate = cumula . map fst $ partiti

-- Metodo d'Hondt (equivalente a Hagenbach-Bischoff)
-- Trasfromo la matrice in lista, e ordino in decrescente
listaCoeff :: [Double]
listaCoeff = reverse . sort $ [i*j | i <- radii, j <- inverses]
-- Cerca il coefficiente per ogni partito
hbCoefficients :: [Double]
hbCoefficients = map (\a -> listaCoeff!!(a-1)) pallineCumulate
-- Calcola le palline cumulate per ogni partito per ogni arco
listaPallineArcoCum :: [(Double,[Seats])]
listaPallineArcoCum = map (\a -> (a, map (floor . (a/)) hbCoefficients)) radii 
-- Decumula le palline
listaPallineArco :: [(Double,[Seats])]
listaPallineArco = map (\(a,b) -> (a,decumula b)) listaPallineArcoCum 

-- Abbiamo una lista ad esempio di [(1,[20,20,13,10])..] dobbiamo passare a quella dei colori con 20 Rossi, 20 Gialli, 13 Blue e 10 Verdi

-- Estrae il numero di palline complessive da un arco
pallinePerArco :: [(Double,[Seats])] -> [(Double,Seats)]
pallinePerArco = map $ \(a,ls) -> (a, last ls)

-- Estrae la distanza tra due palline dello stesso arco
distPerArco :: [(Double,Seats)]->[Double]
distPerArco = map (\(r,p) -> 2*r*sin (pi/(fromIntegral p - 1)/2))
-- Determino la distanza sui due Raggi principali
distRadiale = 1/(fromIntegral nArchi-1)
-- Determino i raggi delle palline, sugli archi tra due pallide ci deve essere una distanza radiale mentre sui Raggi principali almeno 0.01
raggio = minimum $ (map (/3) . distPerArco . pallinePerArco $ listaPallineArcoCum) ++ [distRadiale/2 - 0.01]
