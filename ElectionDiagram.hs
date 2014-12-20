{-# LANGUAGE TypeFamilies #-}

import Data.List
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.SRGB
import Data.Either


------------ Dati Iniziali ----------
type Seats = Int
type Rows  = Int

nArchi :: Rows
nArchi = 9

colourList :: [Colour Double]
colourList = [(sRGB (0.5) (0.5) (0.5)), (sRGB (0.5+171/255/2) (0.5+205/255/2) (0.5+239/255/2)), (sRGB (0.5+30/255/2) (0.5+144/255/2) (255/255)),(sRGB (0.5+3/255/2) (0.5+192/255/2) (0.5+60/255/2)), (sRGB 0.7 0.7 0.7), (sRGB 0.7 0.7 0.7), (sRGB 0.7 0.7 0.7), (sRGB 1 1 (220/255)), (sRGB 1 0.5 0.5), (sRGB 1 0.5 0.5), (sRGB (0.5+123/255/2) (0.5+27/255/2) (0.5+2/255/2))]

nSeats :: [Seats]
nSeats = [16,12,127,28,5,4,15,10,1,70,12]

---------- Main ed Elementi Grafici ----------

main :: IO ()
main = do
    logo <- loadImageEmb "Logo.png"
    mainWith . pad 1.1 $ (titolo <> sigle <> house <> filigrana <> image (head $ rights [logo]) # sized (Dims 0.8 0.8) # translateY (1/8) <> sottotitolo)

titolo :: Diagram B R2
titolo = text "Greek Presidential Election 2014" # scale (1/5) # translateY (2.3)

sottotitolo :: Diagram B R2
sottotitolo = text "First Vote 17 Dic 2014" # scale (1/8) # translateY 2.1

sigle :: Diagram B R2
sigle = text "MS" # scale ((radiusSeat nArchi (cumula nSeats))/2) # translateX 2

filigrana :: Diagram B R2
filigrana = text "scenaripolitici.com" # scale (1/6) # translateY (5/8) # opacity 0.7

house :: Diagram B R2
house = mconcat . map seatRow . addColourToSeat $ listaPallineArco nArchi (cumula nSeats)


---------- Funzioni Grafiche per i Seggi del Parlamento -----------

--Disegna una singola riga
seatRow :: (Double,[(Seats, Colour Double)]) -> Diagram B R2
seatRow (r,xs) = centerX $ decorateTrail (seatRowTrail (sum . map fst $ xs) r) (seatsList xs)

--Assegna ai seggi di un determinato partito il suo colore
addColourToSeat :: [(Double,[Seats])] -> [(Double,[(Seats,Colour Double)])]
addColourToSeat ls = map (\(a,xs) -> (a,zip xs colourList)) ls

--Crea la lista di tutti i seggi
seatsList :: [(Seats,Colour Double)] -> [Diagram B R2]
seatsList xs = mconcat . map (\(n,c)-> drawSeats (radiusSeat nArchi (cumula nSeats)) (take n (repeat c))) $ xs

--Crea la primitiva dei seggi per ogni lista di colori
drawSeats :: Double -> [Colour Double] -> [Diagram B R2]
drawSeats r xc = map ( \a -> circle r # fc a) xc

-- Calcola le primitive angolari per ogni riga del parlamento
seatRowTrail :: (TrailLike t, V t ~ R2) => Int -> Double -> t
seatRowTrail n r = polygon (with & polyType .~ PolyPolar (repeat angle)
                                                         (replicate n r))
    where angle = (1/(2*fromIntegral n) @@ turn)
                  
--------------------- Funzioni Utili -----------------


-- Funzione per cumulare una lista
cumula :: [Seats]->[Seats]
cumula = tail . map sum . inits

-- Funzione per decumulare una lista
decumula :: [Seats]->[Seats]
decumula xs = head xs : zipWith (-) (tail xs) xs


--------------------- Algoritmo per distribuire i seggi nelle varie file -------------------

-- Raggi delle circonferenze
radii :: Rows -> [Double]
radii r = [1.0001, 1.0001+1/(fromIntegral r-1) .. 2.0001]

-- Metodo d'Hondt (equivalente a Hagenbach-Bischoff)
-- Calcolo la lista di tutti i coefficienti per la distribuzione
listaCoeff :: Rows -> Seats -> [Double]
listaCoeff r s = reverse . sort $ [i/(fromIntegral j) | i <- (radii r), j <- [1 .. 2 * s `div` r]]

-- Cerca il coefficiente per ogni partito
hbCoefficients :: Rows -> [Seats] -> [Double]
hbCoefficients r xs = map (\a -> (listaCoeff r (last xs))!!(a-1)) xs

-- Calcola le palline cumulate per ogni partito per ogni arco
listaPallineArcoCum :: Rows -> [Seats] -> [(Double,[Seats])]
listaPallineArcoCum r xs = map (\a -> (a, map (floor . (a/)) (hbCoefficients r xs))) (radii r)

-- Decumula le palline
listaPallineArco :: Rows -> [Seats] -> [(Double,[Seats])]
listaPallineArco r xs = map (\(a,b) -> (a,decumula b)) (listaPallineArcoCum r xs)

-- Estrae il numero di palline complessive da un arco
pallinePerArco :: [(Double,[Seats])] -> [(Double,Seats)]
pallinePerArco = map $ \(a,ls) -> (a, last ls)

-- Estrae la distanza tra due palline dello stesso arco
distPerArco :: [(Double,Seats)] -> [Double]
distPerArco = map (\(r,p) -> 2*r*sin (pi/(fromIntegral p - 1)/2))

-- Determino la distanza sui due Raggi principali
distRadiale :: Rows -> Double
distRadiale r = 1/(fromIntegral r-1)

-- Determino i raggi delle palline, sugli archi tra due pallide ci deve essere una distanza radiale mentre sui Raggi principali almeno 0.01
radiusSeat :: Rows -> [Seats] -> Double
radiusSeat r xs = minimum $ (map (/3) . distPerArco . pallinePerArco $ (listaPallineArcoCum r xs)) ++ [distRadiale r/2 - 0.01]
