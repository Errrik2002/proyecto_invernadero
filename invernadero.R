install.packages("serial")
install.packages("plotly")
install.packages("magrittr")
library(tidyverse)
library(serial)
library(ggplot2)
library(magrittr)#####Debemos instalar estos paquetes y cargarlos para 
library(plotly)####permitir la concexión entre Arduino y R
library(stringtr)

entrada <- listPorts()    #la funcion listPorts nos permite conocer los puertos que estan siendo usados en EL MOMENTO
#Por lo que el aurduino debe ya estar conectado
entrada #lo asignamos a un objeto para que pueda conocer el puerto de cada computadora diferente




myArduino <-  serialConnection(
  port = entrada,
  mode = "9600,n,8,1" ,
  buffering = "none",
  newline = TRUE,   #realizamos el objeto myArduino, que nos pemrite la conexion de R y Arduino y nos permite ver los datos 
  eof = "",         #en port, colocamos nuestro objeto entrada, para espcificar la entrada que estamos ocupando
  translation = "cr", #de esta manera cuando conectemos arduino a otra computadora el puerto podra acoplarse al objeto
  handshake = "none",
  buffersize = 4096
)





##############################################################HUMEDAD Y TEMPERSTURA
close(myArduino)
open(myArduino)          #con esto podemos abrir y cerrar el arduino, solo cierra y abrem, este cerrado o abierto, 
                         #Arduino seguira leyendo las cosas, solo que al abrirlo, R ya puede verlo

isOpen(myArduino) #con esto solo comprobamos si si esta abierto, por que puede fallar si el Serial Port (En arduino) esta abierto



read.serialConnection(myArduino) ##Lee los datos, pero lee los datos de manera fea, por que no se entiende
#debemos aocmodar los datos


datos_sensor <- read.serialConnection(myArduino) %>%
  str_replace_all("[^[:digit:]]+", " ") %>%       #eliminamos todas los caracteres no numericos y los que tenemos los especificamos como numericos
  str_split(" ", simplify = TRUE) %>%
  as.numeric() 

datos_sensor

datos_sensor1 <- na.omit(datos_sensor) #eliminamos NA que salen el inicio y al final de las mediciones

matrixsenso <- as.matrix(datos_sensor1) #lo pasamos a una matrix para ordenar los datos

matrixsenso
sensorchido <- matrix(matrixsenso, nrow= 10) #acomodamos los datos de esta forma, por que es como lo lee arduino
####
nombres <- c("humedad", ".", "temperatura C", ".", "temperatura F", ".", ".",".",".",".")
####
rownames(sensorchido) <- nombres  #asignamos los nombres a los datos, para poder visualizarlos mejor
sensorchido

sensorH_TC_TF <- sensorchido[c("humedad","temperatura C", "temperatura F" ),]
sensorH_TC_TF








###################FUNCION HUMEDAD

ArduSenHT <- function(){
  entrada <- listPorts()
  entrada
  
  myArduino <-  serialConnection(
    port = entrada,
    mode = "9600,n,8,1" ,
    buffering = "none",
    newline = TRUE,
    eof = "",
    translation = "cr",
    handshake = "none",
    buffersize = 4096
  )
  
  
  open(myArduino)
  
  datos_sensor <- read.serialConnection(myArduino) %>%
    str_replace_all("[^[:digit:]]+", " ") %>%
    str_split(" ", simplify = TRUE) %>%
    as.numeric()
  
  datos_sensor1 <- na.omit(datos_sensor)
  
  matrixsenso <- as.matrix(datos_sensor1)
  
  sensorchido <- matrix(matrixsenso, nrow= 10)
 
  nombres <- c("humedad", ".", "temperatura C", ".", "temperatura F", ".", ".",".",".",".")

  rownames(sensorchido) <- nombres

  
  sensorH_TC_TF <- sensorchido[c("humedad","temperatura C", "temperatura F" ),] ####todo lo que hacemos anteriormente lo asignamos a una
  #funcion para que lo haga en automatico, 
  
  
  basenorm <- t(sensorH_TC_TF) #el cambio es que transposicionamos la matrix, para que en cada renglon, sea horas y cada columna
  #sea un dato que medimos
print(basenorm)
    
}

cerrarsensor()










###################PRUEBA SENSOR HT############################################################################################
ArduSenHT()


#realizamos otra funcion para poder asignar el tiempo que queremos que lea el sensor

Humedad_Tempera <- function(){
  
tutiempo <- readline(prompt = "cuantos segundos quieres dejarlo? (error de +2 mediciones) : ") 
tutiempo <- as.numeric(tutiempo)
tiempoaco <- (tutiempo +2) #le preguntamos a la persona el tiempo que quiere dejar correr el sensor
#este caso sons egundos pero pueden ser días u horas

fecha <- Sys.time()
print(fecha) #imprime la hora en que inicia

medida <- ArduSenHT()
print("hacer caso omiso a estas mediciones") #al momento de abrir el sensor imprime datos pero erroneos, como tipo cargando o dapatarse

Sys.sleep(tiempoaco) #dejamos que lea los datos el tiempo que nosotros le asignamos

##########agregar o especificar que es esa hora que termina
print(Sys.time())

ArduSenHT() #imprime los datos del tiempo que le asiganmos

}



base <- Humedad_Tempera() #asiganmos a un objeto

base #imprime los ultimos datos que lee, no incluye los de prueba/erroneos
########Acabando tus mediciones recuerda cerrar el sensor
cerrarsensor() 

##############################################################################################

##

##
















































############FOTORESISTOR


open(myArduino)

isOpen(myArduino)



read.serialConnection(myArduino)


luz <- read.serialConnection(myArduino) %>%
  str_replace_all("[^[:digit:]]+", " ") %>%
  str_split(" ", simplify = TRUE) %>%
  as.numeric()
luz


luz

luz_no_na <- na.omit(luz)

matrixluz <- as.matrix(luz_no_na)

matrixluz
fotoresistor <- matrix(matrixluz)   #realizamos los mismos tratamiento de la información cruda para el sensor de luz
fotoresistor
####

colnames(fotoresistor) <- "luminosidad"
fotoresistor


close(myArduino)
######FUNCION DE LUZ#########

ArduSenL <- function(){
  entrada <- listPorts()
  entrada
  
  myArduino <-  serialConnection(
    port = entrada,
    mode = "9600,n,8,1" ,
    buffering = "none",
    newline = TRUE,
    eof = "",
    translation = "cr",
    handshake = "none",
    buffersize = 4096
  )
  
  
  open(myArduino)
  
  luz <- read.serialConnection(myArduino) %>%
    str_replace_all("[^[:digit:]]+", " ") %>%
    str_split(" ", simplify = TRUE) %>%
    as.numeric()
  
  luz_no_na <- na.omit(luz)
  
  matrixluz <- as.matrix(luz_no_na)
  

  print(matrixluz)
  
 
  
}#y lo agregamos a una funcion, para solmante correr y pueda transformarnos la información más procesada

##########PRUEBA##################

ArduSenL()

cerrarsensor <- function()
  {close(myArduino)  #hacemos la fucnion de cerrar sensor para facilitar el proceso, ya que se debe de hacer de manera recurrente
} 

cerrarsensor()

#########################

luz_fotoresistor<- function(){
  
  tutiempo <- readline(prompt = "cuantos segundos quieres dejarlo? (error de +2 mediciones) : ") 
  tutiempo <- as.numeric(tutiempo)
  tiempoaco <- (tutiempo +2)
  
  fecha <- Sys.time()
  print(fecha)
  
  medida <- ArduSenL()
  print("hacer caso omiso a estas mediciones") #realizamos función similar a la de humedad y temperatura, dond enos pregunte el tiempo
  #y respecto al tiempo nos diga los valores que midio
  
  Sys.sleep(tiempoaco)
  

  print(Sys.time())
  
  ArduSenL()
  
}


medidasluz <- luz_fotoresistor()
medidasluz #######Lo asignamos a un objeto e igual nos da los ultimos datos


medidasluz 


#######sensores

ArduSenHT() #estas son las dos funciones del sensor, que debemos imprimir de manera manual 
ArduSenL()  #debemoos correrlo de manera manual para desde que empiece y temrine la medicion

cerrarsensor()


luz_fotoresistor()

Humedad_Tempera() #estas funcione spermite ya ingresar un numero de tiempo para no tener que correr de manera manual 
#nuestras funciones





base

humedad <- c(base[,1])
humedad

temperaturaC <- c(base[, 2])
temperaturaC

temperaturaF <- c(base[, 3])
temperaturaF

horas <- length(temperaturaC)
horas


revisiones <- function() { #se uso la funcion de function
  contrasena <- readline (prompt = "Buen dia, por favor introduce la contraseña: ") #la contraseña es un objeto que contiene un readline que interactua con el usuario
if(contrasena == "proyecto") { #si el objeto contrasena es igual a proyecto entonces se va a ejecutar lo siguiente
    for ( i in 1:length(horas) ) { #la variable i va a cambiar segun el tamaño del horas
  if (humedad[i] < 20) { #si la humedad correspondiente a cada objeto de las variables "i" es menor a 20 entonces imprime el siguiente mensaje
    print (paste ("PRECAUCION; a los", horas [i], "horas. la humedad fue MUY baja: ", humedad [i], "% de humedad.")) }
      #así se fue con todas variales que se toman definimos un rango en el cual queremos que nos avise si las condiciones son anormales
  if (humedad [i] > 30)
    print (paste ("PRECAUCION; a las", horas[i], "horas, la humedad fue MUY ALTA: ", humedad[i], "% de humedad.")) }
  if (temperaturaC[i] < 20) {
  print (paste ("PRECAUCION; a las ", horas[i], "horas, la temperatura BAJÓ a ", temperaturaC [i], "°C.")) }
  if (temperaturaC[i] > 30) {
    print (paste ("PRECAUCION; a las ", horas[i], "horas, la temperatura fue MUY ALTA: ", temperaturaC[i], "°C.")) }
  if (temperaturaF[i] < 68) {
    print (paste ("PRECAUCION; a las: ", horas[i], "horas, la temperatura fue muy baja: ", temperaturaF[i], "°F.")) }
  if (temperaturaF[i] > 86 ) {
    print (paste ("PRECAUCION; a las ", horas[i], "horas, la temperatura fue MUY ALTA: ", temperaturaF[i], "°F.")) }
    }
}

revisiones()



##


