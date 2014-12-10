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