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

Allora per fare il Riksdag i parametri sono:
10 archi,
Il primo dista dall'origine 1
I vari archi distano tra loro 1/9, quindi l'ultimo è a 2
Il raggio dei seggi è 2/45
I seggi per i vari archi sono: {23, 26, 28, 31, 34, 36, 39, 41, 44, 47}

Algoritmo per determinare il tutto: (scritto in Mathematica, ma dovresti importarlo easy)
-- Dati iniziali
archi = 13;
seggi = 630;
-- Raggi delle circonferenze
raggiSemiCirc = 1 + Range[0, archi - 1]/(archi - 1);
-- Determino il vero divisore D'Hondt (http://en.wikipedia.org/wiki/D'Hondt_method#Example)
-- Determino tutti i numeri del tipo raggi/nSeggi con raggi in raggiSemiCirc e nSeggi in {1 .. 2 seggi/archi}
-- Riduco la matrice ad una lista (Flatten) e ordino in maniera decrescente
-- Prendo il seggi-esimo termine
div = Sort[Flatten@Map[raggiSemiCirc/# &, Range[2 seggi/archi]], Greater][[seggi]];
-- Calcolo quanti seggi vanno per arco, parte intera di raggio/div; controllo che torni con il totale
seggiPerArco = Floor[raggiSemiCirc/div]
seggi == Total[seggiPerArco]
-- Determino la distanza euclidea (in R^2) tra i vari centri per ogni circonferenza
distPerArco = 2 raggiSemiCirc Sin[\[Pi]/(seggiPerArco - 1)/2];
-- Determino la distanza sui due Raggi principali
distRadiale = 1/(archi-1)
-- Determino i raggi dei seggi, sugli archi tra due seggi ci deve essere una distanza radiale mentre sui Raggi principali almeno 0.01
raggio = Min[distPerArco/3, distRadiale/2 - 0.01]