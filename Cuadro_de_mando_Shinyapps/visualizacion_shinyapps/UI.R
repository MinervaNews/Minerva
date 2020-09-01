# Selección ruta de trabajo
#setwd("C:/Users/david/Documents/6_Master/3_Master/000_TFM/1_Minerva/visualizacion_shinyapps")

# Carga de librerías
library(shiny)
library(tidyverse)
library(dplyr)  
library(lubridate)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(ggthemes)


# Reseteo global environment
rm(list=ls())
gc()

########### Importación de datos generados en proceso Python ####

# carga datos procedentes de la salida de Python para generar info de tablas
main = read.csv(file="main.csv", sep=";", header=TRUE, encoding="UTF-8") #, ,fileEncoding = "utf-8" , encondig='UTF-8'
base_clientes = read.csv(file="base_clientes.csv", sep=";", header=TRUE, encoding="UTF-8") #, ,fileEncoding = "utf-8" , encondig='UTF-8'

# selección campos generales a utilizar de las bases generadas en Python.
dataset <- main[, colnames(main)[c(2:11)]]
dataset_clientes <-base_clientes[, colnames(base_clientes)[c(2:7)]]

cond <- {dataset_clientes$Web != "Total"}
dataset_clientes_sin_total <- dataset_clientes[cond, ]

########### Base noticias #########################

# modificación del nombre de algunos campos: Género y Suscripción.
dataset = dataset %>% rename(Genero = tipo_suscripcion)
dataset_clientes = dataset_clientes %>% rename(Suscripción = suscripcion)
dataset_clientes_sin_total = dataset_clientes_sin_total %>% rename(Suscripción = suscripcion)

# modificación de noticias identificadas en las extracciones iniciales como "new" por "gen" de genérica
dataset$Genero[dataset$Genero=='new'] = "gen" 

# modificación formato fecha. 
# NOTA: Dado que nuestro último dato es del 31-7-2020 fijamos esta fecha como "current date". En modo producción lo modificaríamos por Sys.Date() para que tomase siempre la fecha del día en curso
dataset = dataset %>% mutate(Fecha = ymd(dataset$Fecha))
dataset = dataset %>% mutate(Mes_noticia = month(dataset$Fecha))
dataset = dataset %>% mutate(current_date = as.Date('2020-07-25')) #Sys.Date())
dataset = dataset %>% mutate(dias = current_date - Fecha) 
dataset$periodo_notici        = ifelse(dataset$dias <=2, "ult_24h", ifelse(dataset$dias <=7, "ult_semana", "antiguo"))
dataset$ult_24h               = ifelse(dataset$dias <=2, "SI", "NO")
dataset$ultima_semana         = ifelse(dataset$dias <=7, "SI", "NO")

# Creación dataset para agrupar noticias por Genero
dataset_web_genero = dataset  %>% group_by(Web, Genero) %>% summarize(
  rating   = mean(rating_medio),
  lecturas = sum(total_lecturas)/1000000)

# Tabla Tops noticias semanales y últimas 48 horas
dataset_ult_semana = dataset %>% filter(dias <=7)
dataset_ult_24h = dataset %>% filter(dias <=2)

dataset_top10_rc_ult_semana = dataset_ult_semana %>% top_n(15, rating_medio) %>% arrange(desc(rating_medio))
dataset_top10_rc_ult_24h    = dataset_ult_24h %>% top_n(15, rating_medio) %>% arrange(desc(rating_medio))
dataset_top10_lc_ult_semana = dataset_ult_semana %>% top_n(15, total_lecturas) %>% arrange(desc(total_lecturas))
dataset_top10_lc_ult_24h    = dataset_ult_24h %>% top_n(15, total_lecturas) %>% arrange(desc(total_lecturas))

data_para_plot_rc_ult7 <- dataset_top10_rc_ult_semana
data_para_plot_lc_ult7 <- dataset_top10_lc_ult_semana
data_para_plot_rc_ult24h <- dataset_top10_rc_ult_24h
data_para_plot_lc_ult24h <- dataset_top10_lc_ult_24h

########### Base clientes #########################

# clientes suscripción tipo 1
dataset_cli_suscripcion   = dataset_clientes %>% group_by(Suscripción, Web) %>% summarize(
  noticias_leidas_mill    = sum(noticias_leidas)/1000000,
  noticias_puntuadas_mill = sum(noticias_puntuadas)/1000000,
  puntuacion_media        = mean(puntuacion_media))

# clientes suscripción tipo 2 (sin totales)
dataset_cli_suscripcion2  = dataset_clientes_sin_total %>% group_by(Suscripción, Web) %>% summarize(
  noticias_leidas_mill    = sum(noticias_leidas)/1000000,
  noticias_puntuadas_mill = sum(noticias_puntuadas)/1000000,
  puntuacion_media        = mean(puntuacion_media))

# Información por periódicos del tipo de suscripción a través de la cual leen sus noticias
dataset_cli_suscripcion3  = dataset_clientes_sin_total %>% group_by(Web, Suscripción) %>% summarize(
  noticias_leidas_mill    = sum(noticias_leidas)/1000000,
  noticias_puntuadas_mill = sum(noticias_puntuadas)/1000000,
  puntuacion_media        = mean(puntuacion_media))

########### Distinguimos variables continuas y categóricas para ggplot) #########

# CONTINUAS:

# Base de noticias
data_para_plot <- dataset
# Base de clientes
data_para_plot_cli_susc <- dataset_cli_suscripcion

## Datos generales
nums <- sapply(data_para_plot, is.numeric)
continuas <- names(data_para_plot)[nums]
## rating ult7
nums_rc_ult7 <- sapply(data_para_plot_rc_ult7, is.numeric)
continuas_rc_ult7 <- names(data_para_plot_rc_ult7)[nums_rc_ult7]
## lecturas ult7
nums_lc_ult7 <- sapply(data_para_plot_lc_ult7, is.numeric)
continuas_lc_ult7 <- names(data_para_plot_lc_ult7)[nums_lc_ult7]
## rating ult24h
nums_rc_ult24h <- sapply(data_para_plot_rc_ult24h, is.numeric)
continuas_rc_ult24h <- names(data_para_plot_rc_ult24h)[nums_rc_ult24h]
## lecturas ult24h
nums_lc_ult24h <- sapply(data_para_plot_lc_ult24h, is.numeric)
continuas_lc_ult24h <- names(data_para_plot_lc_ult24h)[nums_lc_ult24h]
## web_genero
nums_web_genero <- sapply(dataset_web_genero, is.numeric)
continuas_web_genero <- names(dataset_web_genero)[nums_web_genero]
## base clientes: suscripciones
nums_cli_susc <- sapply(dataset_cli_suscripcion, is.numeric)
continuas_cli_susc <- names(dataset_cli_suscripcion)[nums_cli_susc]
nums_cli_susc2 <- sapply(dataset_cli_suscripcion2, is.numeric)
continuas_cli_susc2 <- names(dataset_cli_suscripcion2)[nums_cli_susc]
nums_cli_susc3 <- sapply(dataset_cli_suscripcion3, is.numeric)
continuas_cli_susc3 <- names(dataset_cli_suscripcion3)[nums_cli_susc]


# CATEGORICAS:

## Datos generales
cats <- sapply(data_para_plot, is.factor)
categoricas <- names(data_para_plot)[cats]
## rating ult7
cats_rc_ult7 <- sapply(data_para_plot_rc_ult7, is.factor)
categoricas_rc_ult7 <- names(data_para_plot_rc_ult7)[cats_rc_ult7]
## lecturas ult7
cats_lc_ult7 <- sapply(data_para_plot_lc_ult7, is.factor)
categoricas_lc_ult7 <- names(data_para_plot_lc_ult7)[cats_lc_ult7]
## rating ult24h
cats_rc_ult24h <- sapply(data_para_plot_rc_ult24h, is.factor)
categoricas_rc_ult24h <- names(data_para_plot_rc_ult24h)[cats_rc_ult24h]
## lecturas ult24h
cats_lc_ult24h <- sapply(data_para_plot_lc_ult24h, is.factor)
categoricas_lc_ult24h <- names(data_para_plot_lc_ult24h)[cats_lc_ult24h]
## web_genero
cats_web_genero <- sapply(dataset_web_genero, is.factor)
categoricas_dataset_web_genero <- names(dataset_web_genero)[cats_web_genero]
## clientes: suscripcion
cats_cli_susc <- sapply(dataset_cli_suscripcion, is.factor)
categoricas_cli_susc <- names(dataset_cli_suscripcion)[cats_cli_susc]
cats_cli_susc2 <- sapply(dataset_cli_suscripcion2, is.factor)
categoricas_cli_susc2 <- names(dataset_cli_suscripcion2)[cats_cli_susc]
cats_cli_susc3 <- sapply(dataset_cli_suscripcion3, is.factor)
categoricas_cli_susc3 <- names(dataset_cli_suscripcion3)[cats_cli_susc]

########### ShinyUI ####

# Guardamos global environment
save.image(".RData")
#savehistory()

### shinyUI. NAVBARPAGE. Diseño de la estructura visual del cuadro de mando en shinyapps

shinyUI(  
  navbarPage("Minerva", windowTitle="Minerva News",  # Página con barra de navegación superior donde incluir apartados
                  # Tema utilizado
                  theme = shinytheme("spacelab"), 
                  # Creación con navbarPage de la barra de navegación principal con los apartados: noticias y clientes.
                  tabPanel("Introducción",
                      tags$img(src='IMG_9071_50_60.jpg', align = "left", width=180, height=130),
                      mainPanel(
                        h1("Cuadro de mando", align = "left"),
                        h4("Nota de contenido:", align = "left"),
                        hr(),
                        br(),
                        h4("Dentro del proyecto Minerva, se ha desarrollado un cuadro de mando interno para controlar", 
                        strong("el número de usuarios, las noticias que más leen, las mejor valoradas y a qué medios 
                        corresponden."), "Adicionalmente, en la fase inicial también se busca controlar el", 
                        strong("tipo de suscripción"), "para conocer en qué medios y tipo de noticias están realmente 
                        interesados los lectores para poder mejorar continuamente su experiencia y optimizar nuestros 
                        recursos.", style="text-align: justify"), 
                        br(),
                        br(),
                        h4("En posibles extensiones del proyecto se añadirían secciones relativas a", strong("pricing,"), 
                        "con información desglosada de ingresos no sólo a nivel suscripción o periódico, sino a nivel 
                        noticia. Así mismo, se incluiría información sobre", strong("freelances"), "e incorporaría al cuadro 
                        la gestión del", strong("pago a los medios por noticias"), "para generar datos de rentabilidad", 
                        style="text-align: justify"),
                        br()
                        )),
                  # En noticias, incorporamos una nueva barra de navegación con datos de webs, top ratings, más leidas...
                  tabPanel("Noticias", navbarPage("", 
                               tabPanel("Datos Webs",
                                        mainPanel(
                                          h2("Datos de lectura y puntuación por periódicos", align = "center")),
                                          tags$img(src="IMG_9071_50_60.jpg", height="65", align="rigth"),
                                        sidebarPanel(
                                          radioButtons(inputId = 'y2', 'Seleccion:', # Con radioButtons creamos botones de selección.
                                                      c('Rating'  = continuas_web_genero[1],
                                                        'Lecturas (MM)'= continuas_web_genero[2]),  # Variables a considerar
                                                      continuas_web_genero[[2]]),), # Variable a mostrar por defecto
                                        mainPanel(
                                          plotOutput(outputId = 'plot2',
                                                     height = 300)), # píxeles.
                                        h4("Datos distribuidos por periódicos con información sobre el total de lecturas (en millones) 
                                           y rating medios. Podemos monitorizar los periódicos más leidos por los suscriptores, las puntuaciones
                                           medias de las noticias de los mismos, así como el género que más se está leyendo.", style="text-align: justify")
                                        ),
                               tabPanel("Top rating semanal",
                                        mainPanel(
                                          h2("Mejor puntuación en la última semana", align = "center")),
                                          tags$img(src="IMG_9071_50_60.jpg", height="65", align="rigth"),
                                        sidebarPanel(
                                          radioButtons(inputId = 'y3', 'Seleccion:', 
                                                      c('Rating medio'   = continuas_rc_ult7[3],
                                                        'Lecturas'  = continuas_rc_ult7[4]), 
                                                      continuas_rc_ult7[[3]]), 
                                        ),
                                        mainPanel(
                                          plotOutput(outputId = 'plot3',
                                                     height = 300)), 
                                        h4("Información sobre el Top15 de noticias mejor puntuadas durante la última semana. Adicionalmente 
                                           se cuenta con la información del número de lecturas de estas noticias. El cuadro ofrece información de los 
                                          del periódico al que corresponden las noticias para poder ver de un vistazo que medios son los que 
                                           ofrecen las noticias más leidas", style="text-align: justify"),
                                        ),
                               tabPanel("+ leídas semana",
                                        mainPanel(
                                          h2("Mayor número de lecturas en la última semana", align = "center")),
                                          tags$img(src="IMG_9071_50_60.jpg", height="65", align="rigth"),
                                        sidebarPanel(
                                          
                                          radioButtons(inputId = 'y4', 'Seleccion:', 
                                                       c('Rating medio'   = continuas_rc_ult7[3],
                                                         'Lecturas'  = continuas_rc_ult7[4]), 
                                                      continuas_rc_ult7[[4]]), 
                                        ),
                                        mainPanel(
                                          plotOutput(outputId = 'plot4',
                                                     height = 300)), 
                                        h4("Información sobre el Top15 de noticias más leidas durante la última semana. Adicionalmente 
                                           se cuenta con la puntuación media para estas noticias. El cuadro ofrece información de los 
                                          del periódico al que corresponden las noticias para poder ver de un vistazo que medios son los que 
                                           ofrecen las noticias más leidas", style="text-align: justify")
                                        ),
                               tabPanel("Top rating 48h",
                                        mainPanel(
                                          h2("Mejor puntuación en las últimas 48 horas", align = "center")),
                                          tags$img(src="IMG_9071_50_60.jpg", height="65", align="rigth"),
                                        sidebarPanel(
                                        radioButtons(inputId = 'y5', 'Seleccion:', 
                                                     c('Rating medio'   = continuas_rc_ult24h[3],
                                                       'Lecturas'  = continuas_rc_ult24h[4]),  
                                                     continuas_rc_ult24h[[3]]), 
                                        ),
                                        mainPanel(
                                          plotOutput(outputId = 'plot5',
                                                     height = 300)), 
                                        h4("Al igual que el el apartado anterior, se muestran las noticias con mejor puntuación, pero
                                           en este caso en las últimas 48 horas.", style="text-align: justify")
                                        ), 
                               tabPanel("+ leídas últimas 48h",
                                        mainPanel(
                                          h2("Mayor número de lecturas en las últimas 48 horas", align = "center")),
                                          tags$img(src="IMG_9071_50_60.jpg", height="65", align="rigth"),
                                        sidebarPanel(
                                        radioButtons(inputId = 'y6', 'Últimas noticias', 
                                                     c('Rating medio'   = continuas_rc_ult24h[3],
                                                       'Lecturas'  = continuas_rc_ult24h[4]),  
                                                     continuas_lc_ult24h[[4]]), 
                                        ),
                                        mainPanel(
                                        plotOutput(outputId = 'plot6',
                                                     height = 300)), 
                                        h4("Detalle de las 15 noticias más leidas en las últimas 48 horas. Se indica el periódico al que 
                                           corresponde cada noticia para visualmente poder controlar qué medios están funcionando mejor
                                           en nuestra plataforma", style="text-align: justify")
                                        ))),
                                        
                    # En clientes incluimos otra barra de navegación con datos sobre suscripciones
                    tabPanel("Clientes", navbarPage("", 
                                                  tabPanel("Info suscripciones",
                                                           mainPanel(
                                                             h2("Lecturas y puntuaciones totales por suscripción", align = "center")),
                                                             tags$img(src="IMG_9071_50_60.jpg", height="65", align="rigth"),
                                                           sidebarPanel(
                                                           radioButtons(inputId = 'y7', 'Seleccion:', 
                                                                        c('Noticias leidas (MM)'    = continuas_cli_susc[1],
                                                                          'Noticias puntuadas (MM)' = continuas_cli_susc[2],
                                                                          'Puntuación media'        = continuas_cli_susc[3]),  
                                                                        continuas_cli_susc[[2]]), 
                                                           ),
                                                           mainPanel(
                                                             plotOutput(outputId = 'plot7',
                                                                        height = 300)),
                                                           h4("Datos relativos a noticias leidas, puntuadas y puntuación media 
                                                              en función del tipo de suscripción. Se incluye detalle a nivel 
                                                              periódico.", style="text-align: justify")
                                                           ),
                                                  tabPanel("Suscripciones agregadas",
                                                           mainPanel(
                                                             h2("Distribución datos por suscripción según periódicos", align = "center")),
                                                             tags$img(src="IMG_9071_50_60.jpg", height="65", align="rigth"),
                                                           sidebarPanel(
                                                           radioButtons(inputId = 'y8', 'Seleccion:', 
                                                                        c('Noticias leidas (MM)'    = continuas_cli_susc2[1],
                                                                          'Noticias puntuadas (MM)' = continuas_cli_susc2[2]), 
                                                                          continuas_cli_susc2[[2]]), 
                                                           ),
                                                           mainPanel(
                                                           plotOutput(outputId = 'plot8',
                                                                        height = 300)), 
                                                           h4("Datos de lectura y puntuación según el tipo de suscripción.
                                                              Las barras reflejan la proporción de los datos en función de 
                                                              los distintos periódicos, lo cual permite ver la magnitud de cada medio
                                                              en función de cada suscripción", style="text-align: justify")
                                                           ),
                                                  tabPanel("Periódicos según tipo suscripción",
                                                           mainPanel(
                                                             h2("Periódicos y tipo de suscripción asociada", align = "center")),
                                                             tags$img(src="IMG_9071_50_60.jpg", height="65", align="rigth"),
                                                           sidebarPanel(
                                                             radioButtons(inputId = 'y9', 'Seleccion:', 
                                                                          c('Noticias leidas (MM)'    = continuas_cli_susc3[1],
                                                                            'Noticias puntuadas (MM)' = continuas_cli_susc3[2], 
                                                                            'Puntuación media'        = continuas_cli_susc[3]),
                                                                          continuas_cli_susc3[[2]]), 
                                                           ),
                                                           mainPanel(
                                                             plotOutput(outputId = 'plot9',
                                                                        height = 300)),
                                                             h4("Datos a nivel periódico de noticias leidas, puntuadas y puntuación media. 
                                                              Esta información se distribuye para indicar en qué medida los usuarios que están 
                                                              leyendo y valorando las noticias acceden en función de una determinada suscripción", style="text-align: justify")
                                                           
                                                           )))))
                                                  
                                                  

