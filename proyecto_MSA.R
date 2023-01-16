# Primero cargamos las librerias que ocuparemos en el script.
library(Biostrings)
library(msa)
library(ggmsa)

# Para leer las secuencias se uso la funcion readDNAStringSet de la libreria
# Biostrings, cada una se guardo en un objeto diferente.
Mpudica <- readDNAStringSet("secuencias/Mimosa_pudica_AQP.fna")
Palba <- readDNAStringSet("secuencias/Prosopis_alba_AQP.fna")
Dmusc <- readDNAStringSet("secuencias/Dionaea_muscipula_AQP.fna")

# En el caso de la secuencia de Chamaecrista nictitans la secuencia solo estaba
# disponible como aminoacidos, por lo que se utilizo la funcion readAAStringSet.
Cnic <- readAAStringSet("secuencias/Chamaecrista_nictitans_aqp.fasta")

# Luego guardamos en un vector las 3 secuencias de nucleotidos.
secuencias_nct <- c(Mpudica, Palba, Dmusc)

# Tambien creamos un vector con los nombres para sustituirlos en las secuencias.
spp <- c("Mimosa_pudica", "Prosopis_alba", "Dionaea_muscipula")
# Y le cambiamos el nombre a cada una para que sea facil identificarlas
# con la funcion names, como argumento tiebe el objeto con las secuencias
# y con el caracter de asignacion indicamos que le coloque los nombres que estan
# dentro del vector.
names(secuencias_nct) <- spp

# Despues realizamos el msa, como argumentos tenemos el objeto con las secuencias
# y el metodo es Muscle, el resto de los argumentos se omitieron porque permanecen
# los valores predeterminados.
fabaceas_msa <- msa (secuencias_nct, "Muscle")
print (fabaceas_msa, show = "alignment")

# Las secuencias fueron traducidas a proteina con la funcion translate de la
# libreria Biostrings.
secuenciAAs <- translate (secuencias_nct)
# Y agrupamos las secuencias traducidad con C. nictitans.
secuenciAAs2 <- c(secuenciAAs, Cnic)

# Repetimos la linea de codigo anterior para asignarle nombre a las secuencias de
# aminoacidos.
spp2 <- c("Mimosa_pudica", "Prosopis_alba", "Dionaea_muscipula", "Chamaecrista_nictitans")
names(secuenciAAs2) <- spp2

# Se realizo el alineamiento multiple de las secuencias de aminoacidos,
# el metodo fue el predeterminado.
fabaceas_msAA <- msa (secuenciAAs2)

#########################################################

fabuloso <- ggmsa (secuencias_nct, 1, 100, font = "DroidSansMono", color = "Chemistry_AA",
                   char_width = 0.6, by_conservation = T, seq_name = T)

fabuloso2 <- ggmsa(secuenciAAs2, 1, 100, font = "DroidSansMono", color = "Chemistry_AA",
                   char_width = 0.6, by_conservation = T, seq_name = T)


##########################################################

# Las secuencias se convirtieron en un objeto con formato para hacer un arbol
# filogenetico. Se utilizo la funcion msaConvert, como argumento tenia el alineamiento
# multiple y se indica que el tipo de archivo es un alineamiento.

fabaceas_ncl <- msaConvert (fabaceas_msa, type="seqinr::alignment")
fabaceas_aa <- msaConvert (fabaceas_msAA, type="seqinr::alignment")

# Para el arbol cargamos dos librerias mas, seqinr y ape.
library (seqinr)
library (ape)

# Se calcularon las distancias en ambos alineamientos con la funcion dist.alignment,
# la matriz que se obtenga sera una matriz identidad, ya que esta opcion
# sirve para secuencias de ADN y de aminoacidos.

dist1 <- dist.alignment(fabaceas_ncl, "identity")
dist2 <- dist.alignment(fabaceas_aa, "identity")

# Indicamos que el objeto anterior debe ser leido como una matriz.
matrizdist <- as.matrix(dist1)
matrizdistAA <- as.matrix(dist2)

# Para hacer el arbol se uso el metodo de inferencia por dsitancia Neighbour Joining,
# y se grafico.
faboarbol <- nj (matrizdist)
plot (faboarbol, main = "Relacion aquaporinas (nucleotidos)")

# Se repitio el paso anterior pero con la secuencia de aminoacidos, con el fin
# de observar diferencias o similitudes entre las relaciones inferidas.
faboarbol2 <- nj (matrizdistAA)
plot (faboarbol2, main = "Relacion aquaporinas (aminoacidos)")
