#### TENDENCIAS DE TWITTER

### Directorio de trabajo
setwd("C:/Users/david/Documents/6_Master/3_Master/000_TFM/1_Minerva/datos/A_news")
getwd()

### Carga de librerías
library(twitteR)
library(rtweet)
library(wordcloud)
library(tidyverse)
library(tidyr)
library(tm)
library(dplyr)
library(stringr)
library(readr)

### Carga de credenciales
source('twitterAuth.R')

## Acceso a Twitter 
create_token(
  app             = twitter_app,
  consumer_key    = api_key,
  consumer_secret = api_secret,
  access_token    = access_token,
  access_secret   = access_token_secret)

### Tendencias (rtweet)

get_trends()

# Del paquete rtweet
tendencias_locales <- trends_available()
head(tendencias_locales)

# Tabla por paises
table(tendencias_locales$countryCode)

# Para España
tendencias_locales[tendencias_locales$countryCode=='ES', ]

# El woeid 23424950 corresponde a España
tendencias_Espana <- get_trends(woeid = '23424950')


### Importación del dataset de noticias diarias generado en Python
# (notebook: Scraping completo + clientes v2.ipynb)
data <- read.csv("news_completo_31072020.csv", header = 1, sep = ";", encoding = "UTF-8")

## Extracción titular e id_new del dataset diario
data_txt <- data$Título
head(data_txt, 10)

## Limpieza del dataset: Eliminación puntuaciones, acentos y caracteres especiales, alfanuméricos
#                        minúsculas, tabulaciones, espacios, stopwords, así como palabras irrelavantes manualmente.

# Puntuaciones
data_txt <- gsub("[[:punct:]]", "", data_txt)
# Acentos
acentos.text <- function(x="Texto Nulo"){
  x <- gsub("á", "a", x)
  x <- gsub("é", "e", x)
  x <- gsub("í", "i", x)
  x <- gsub("ó", "o", x)
  x <- gsub("ú", "u", x)
  x <- gsub("ñ", "n", x)
}
data_txt <- acentos.text(data_txt)

# Carácteres alfanuméricos
data_txt <- gsub("[^a-zA-Z0-9 ]", "", data_txt)
head(data_txt, 10)

# NAs
data_txt <- data_txt[!is.na(data_txt)]
head(data_txt, 10)

# Texto a minúsculas
data_txt <- tolower(data_txt)
head(data_txt, 10)

# Espacios y tabulaciones
data_txt <- gsub("[ \t]{2,}", "", data_txt)
data_txt <- gsub("^\\s+|\\s+$", "", data_txt)
head(data_txt, 10)

# Eliminación de Stopwords
data_txt <- removeWords(data_txt, stopwords('es'))
head(data_txt, 10)

# Eliminación de palabras manualmente
data_txt <- gsub("tras", "", data_txt)
data_txt <- gsub("si", "", data_txt)
data_txt <- gsub(c("mas", "ser", "estar", "dos", "primera", "vez"), "", data_txt)
head(data_txt, 10)

## Una vez depurado data_txt (títular de las noticias diarias), se crea el Corpus
#  para lematización y realización de operaciones de minería de textos (tm package).

# Creación del corpus
corpus <- Corpus(VectorSource(data_txt), readerControl = list(language = "es"))
inspect(corpus[1:10])

# Generación de Wordcloud preliminar de todos los titulares (wordcloud package)
wordcloud(corpus, min.freq = 10, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# Generación DTM (Document-Term Matrix) para identificar la frecuencia de palabras gestionadas. (tm package)
data_dtm <- DocumentTermMatrix(corpus, control = list(minWordLength = 1, stopwords = TRUE))
inspect(data_dtm)

### Terminos más frecuentes incluyendo una limitación de una frecuencia mínima de 10 (tm package)
top_terminos <- findFreqTerms(data_dtm, lowfreq=15)
head(top_terminos, 40) # Muestra los 40 primeros

# Tras identificación de los términos más frecuentes en el dataset de noticias diarias se procede
# a identificar en la base de noticias diarias (data) un campo "Matches" que marque las noticias que 
# incluyen uno o más términos frecuentes del día para posteriormente acoplarle los tweets relacionados

regex = paste(top_terminos, collapse="|")
data$Matches = sapply(str_extract_all(data$Título, regex), function(x) paste(x, collapse=";"))

# Una vez generado el campo Matches, el cual incluye en cada noticia los términos frecuentes que le impactan,
# se procede a la separación de estos términos para poder cruzarlos con el dataset que contiene el stock
# de Top5 tweets por término frecuente. Este proceso implica que el campo Matches pasa a 5 campos (Top_term1...5),
# de modo que cada noticia puede contener hasta un límite de 5 términos frecuentes. (tidyr package)

data = data %>% separate(Matches, c("Top_term_1", "Top_term_2", 'Top_term_3', 'Top_term_4', 'Top_term_5'), 
                         sep= ';', extra = "drop", fill = "right")

# Mediante un bucle, se procede a recorrer top_terminos para generar un dataframe acumulando para cada término 
# frecuente los 5 tweets relacionados en español, creando así top_terminos_acumulados (stock total de tweets). (dplyr package)
top_terminos_acumulados <- data.frame()
for(i in top_terminos) { 
  temp  = search_tweets(i, n = 5 , include_rts = FALSE, lang = "es")
  indice_top_terminos = (i)
  temp2  = cbind(temp, indice_top_terminos)
  top_terminos_acumulados <- bind_rows(top_terminos_acumulados, temp2)
}

# El data frame "data" contiene los campos con los hasta 5 top terms. Para ello se procede a realizar
# cruces entre cada uno de estos 5 campos y el data frame top_terminos_acumulados con los tweets.
# Posteriormete, se acomplan los data frames en uno único y se eliminan las filas nulas, ya que hay que considerar
# que no todas las noticias tienen hasta 5 terminos frecuentes (dplyr package)
data_with_tweets_1 = data %>% left_join(top_terminos_acumulados, by = c("Top_term_1" = "indice_top_terminos"))
data_with_tweets_2 = data %>% left_join(top_terminos_acumulados, by = c("Top_term_2" = "indice_top_terminos"))
data_with_tweets_3 = data %>% left_join(top_terminos_acumulados, by = c("Top_term_3" = "indice_top_terminos"))
data_with_tweets_4 = data %>% left_join(top_terminos_acumulados, by = c("Top_term_4" = "indice_top_terminos"))
data_with_tweets_5 = data %>% left_join(top_terminos_acumulados, by = c("Top_term_5" = "indice_top_terminos"))

data_with_total_tweets = rbind(data_with_tweets_1, data_with_tweets_2, data_with_tweets_3,
                               data_with_tweets_4, data_with_tweets_5)

data_with_total_tweets<- data_with_total_tweets[-which(is.na(data_with_total_tweets$user_id)), ]

# Se verifica para una noticia específica cuales son los 5 tweets relacionados.
Test_gen1 = data_with_total_tweets %>% filter(new_id == 'gen_1')

# Con el objeto de ampliar las funcionalidades del proyecto, se genera una tabla y exporta para poder ser utilizada
# en futuras extensiones del cuadro de mando.
data_with_total_tweets_res = data_with_total_tweets %>% select(2:30)

utils::write.table(data_with_total_tweets_res, 
            file = "C:/Users/david/Documents/6_Master/3_Master/000_TFM/1_Minerva/visualizacion_shinyapps/noticias_con_tweets.csv", 
            sep = "|",
            col.names = TRUE,row.names = FALSE)


