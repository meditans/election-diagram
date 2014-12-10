Appunti:

non serve calcolare le coordinate polari: basta decidere quanti seggi
stanno in ogni fila e questo consente di distribuirli equamente nella
loro semicirconferenza. Basta quindi sapere quante file vogliamo, e
quanti posti ci devono essere in ogni fila.
Ti serve almeno il Raggio di ogni semicirconferenza e il raggio dei seggi?
Ovviamente dal m di seggi per arco alle cordinanate polari è un attimo mi sarei stupito se non l'avesse fatto direttamente lui...

Vuoi che il numero di seggi sia variabile nella rappresentazione?
Ovviamente si, voglio fare un programma generico... Non voglio rifarlo quando passo dalla Camera ITA al quella Svedese e così via...

In che formato dovrebbe prendere i dati?
Pensavo a Num di seggi totali e poi una lista (un dato per riga) di {nSeggiPartito, colorePartito} ovviamente il colore me lo devi dire tu se vuoi in RGB o cos'altro; stando così nel più generico possibile

Impostazioni grafiche secondarie:
I seggi andrebbero nel settore circolare di raggio R e 2R
Nel Cerchio di Raggio R/2 io metterei il monogramma della Camera
In alto un titolo (facoltativo)
E in filigrana il nome del sito in cui si pubblica l'immagine; firmare le proprie cose non fa mai male...


Algoritmo per determinare il tutto:
-- Dati iniziali
nArchi = 13
nPalline = 630
partiti = [(200, Red), (200, Yellow), (130, Blue), (100, Green)]

-- Raggi delle circonferenze
raggiSemiCirc = [1, 1/(nArchi-1) .. 2]
listaInversi = 1 / [1 .. 2 nPalline/nArchi]

-- Funzioni utili -- Non so come scriverle
Cumula :: [Palline]->[Palline]
Decumula :: [Palline]->[Palline]
Decumulare lista (l'inversa)
-- Esempio
Cumula . Map first $ partiti = [200,400,530,630]

-- Metodo d'Hondt (equivalente a Hagenbach-Bischoff)
matriceCoeff :: [[Floor]]
matriceCoeff = raggiSemiCirc X listaInversi
-- Trasfromo la matrice in lista, e ordino in decrescente
listaCoeff :: [Floor]
listaCoeff = Sort Union matriceCoeff
-- Cerca il coefficiente per ogni partito
coeffHBPartiti :: [Floor]
coeffHBPartiti = Map (\a -> listaCoeff(a)) pallineCumulate
-- Calcola le palline cumulate per ogni partito per ogni arco
listaPallineArcoCum :: [(Raggio,[Palline])]
listaPallineArcoCum = Map (\a -> (a, Floor a/coeffHBPartiti)) raggiSemiCirc 
-- Decumula le palline
listaPallineArcoCum :: [(Raggio,[Palline])]
listaPallineArco = Map (\(a,b) -> (a,Decumula b)) listaPallineArcoCum 

-- Abbiamo una lista ad esempio di [(1,[20,20,13,10])..] dobbiamo passare a quella dei colori con 20 Rossi, 20 Gialli, 13 Blue e 10 Verdi

-- Estrae il numero di palline complessive da un arco
pallinePerArco :: [(Raggio,[Palline])] -> [(Raggio,Palline)]
pallinePerArco [(_,lsPalline)] = [(_, last lsPalline)]

-- Estrae la distanza tra due palline dello stesso arco
distPerArco :: [(Raggio,Palline)]->[Floor]
distPerArco [r,p] = [2 r Sin (Pi/(p - 1)/2)];
-- Determino la distanza sui due Raggi principali
distRadiale = 1/(nArchi-1)
-- Determino i raggi delle palline, sugli archi tra due pallide ci deve essere una distanza radiale mentre sui Raggi principali almeno 0.01
raggio = Min[(distPerArco . pallinePerArco $ listaPallineArcoCum)/3, distRadiale/2 - 0.01]