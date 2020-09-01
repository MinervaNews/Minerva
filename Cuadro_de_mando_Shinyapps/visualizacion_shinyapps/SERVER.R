# Carga de librerías
library(shiny)
library(ggplot2)

# ShinyServer: Se incorpora la información de los gráficos a generar con la información de 
# UI.R. (tipo de gráficos, temas, variables a utilizar, etc...)

shinyServer(function(input, output) {
  
  # Datos de lectura y puntuación por periódicos
  output$plot2 <- renderPlot({
    p2 <- ggplot(dataset_web_genero) # Indicamos el dataset con la información a graficar.
    if (input$y2 == 'rating')        # Indicamos mediante condiciones qué mostrar en función de la variable seleccionada.
      p2 <- p2 + aes(Web, rating, fill = Genero)
    if (input$y2 == 'lecturas')
      p2 <- p2 + aes(Web, lecturas, fill = Genero)
    p2 <- p2 + geom_bar(position = "dodge",stat = "identity") +   # Indicamos características del gráfico según las funcionalidades que nos ofrece ggplot2
      coord_flip() +theme_minimal() +                             # En este caso, el tipo gráfico (dodge: Pone los elementos a lado de cada uno), trasposición del gráfico (coord_flip) y temáticas
      scale_fill_brewer(palette = "Blues") +                      # Finalmente para una mayor simplicidad visual eliminamos el nombre de ejes ya que por la información de la pantalla se extrae correctamente.
      labs(x="", y=".")
    print(p2)})
  
  # Top rating semanal. Mejor puntuación en la última semana
  output$plot3 <- renderPlot({
    p3 <- ggplot(data_para_plot_rc_ult7)
    if (input$y3 == 'rating_medio') 
      p3 <- p3 + aes(Título, rating_medio, fill = Web)
    if (input$y3 == 'total_lecturas')
      p3 <- p3 + aes(Título, total_lecturas, fill = Web)
    p3 <- p3 + geom_bar(position = "dodge", stat = "identity") +
      coord_flip()+theme_minimal() +
      scale_fill_brewer(palette = "Blues") + 
      labs(x="", y="")
    print(p3)})
  
  # + Leidas semana. Mayor número de lecturas en la última semana
  output$plot4 <- renderPlot({
    p4 <- ggplot(data_para_plot_lc_ult7)
    if (input$y4 == 'rating_medio') 
      p4 <- p4 + aes(Título, rating_medio, fill = Web)
    if (input$y4 == 'total_lecturas')
      p4 <- p4 + aes(Título, total_lecturas, fill = Web)
    p4 <- p4 + geom_bar(position = "dodge", stat = "identity") + 
      coord_flip()+theme_minimal() + 
      scale_fill_brewer(palette = "Blues") +
      labs(x="", y="")
    print(p4)})

  # Mejor puntuación en las últimas 48 horas
  output$plot5 <- renderPlot({
    p5 <- ggplot(data_para_plot_rc_ult24h)
    if (input$y5 == 'rating_medio') 
      p5 <- p5 + aes(Título, rating_medio, fill = Web)
    if (input$y5 == 'total_lecturas')
      p5 <- p5 + aes(Título, total_lecturas, fill = Web)
    p5 <- p5 + geom_bar(position = "dodge", stat = "identity") + 
      coord_flip()+theme_minimal() + 
      scale_fill_brewer(palette = "Blues") +
      labs(x="", y="")
    print(p5)})
  
  # Mayor número de lecturas en las últimas 48 horas
  output$plot6 <- renderPlot({
    p6 <- ggplot(data_para_plot_lc_ult24h)
    if (input$y6 == 'rating_medio') 
      p6 <- p6 + aes(Título, rating_medio, fill = Web)
    if (input$y6 == 'total_lecturas')
      p6 <- p6 + aes(Título, total_lecturas, fill = Web)
    p6 <- p6 + geom_bar(position = "dodge", stat = "identity") + 
      coord_flip() +theme_minimal() + 
      scale_fill_brewer(palette = "Blues") +
      labs(x="", y="")
    print(p6)})
  
  # Lecturas y puntuaciones totales por suscripción
  output$plot7 <- renderPlot({
    p7 <- ggplot(dataset_cli_suscripcion)
    if (input$y7 == 'noticias_leidas_mill') 
      p7 <- p7 + aes(Suscripción, noticias_leidas_mill, fill = Web)
    if (input$y7 == 'noticias_puntuadas_mill')
      p7 <- p7 + aes(Suscripción, noticias_puntuadas_mill, fill = Web)
    if (input$y7 == 'puntuacion_media')
      p7 <- p7 + aes(Suscripción, puntuacion_media, fill = Web)
    p7 <- p7 + geom_bar(position = "dodge", stat = "identity") + 
      coord_flip() +theme_minimal() + 
      scale_fill_brewer(palette = "RdYlBu") +
      labs(x="", y="")
    print(p7)})
  
  # Distribución datos por suscripción según periódicos
  output$plot8 <- renderPlot({
    p8 <- ggplot(dataset_cli_suscripcion2)
    if (input$y8 == 'noticias_leidas_mill') 
      p8 <- p8 + aes(Suscripción, noticias_leidas_mill, fill = Web)
    if (input$y8 == 'noticias_puntuadas_mill')
      p8 <- p8 + aes(Suscripción, noticias_puntuadas_mill, fill = Web)
    p8 <- p8 + geom_bar(position = "stack", stat = "identity") +        # En este gráfico, modificamos la posición dodge por stack 
      theme_minimal() +                                                 # de modo que pasa de poner los elementos al lado a acoplarlos uno encima de otro. 
      scale_fill_brewer(palette = "RdYlBu") +                           # Adicionalmente eliminamos el volteo de coordenadas.
      labs(x="", y="")
    print(p8)})
  
  # Periódicos y tipo de suscripción asociada
  output$plot9 <- renderPlot({
    p9 <- ggplot(dataset_cli_suscripcion3)
    if (input$y9 == 'noticias_leidas_mill') 
      p9 <- p9 + aes(Web, noticias_leidas_mill, fill = Suscripción)
    if (input$y9 == 'noticias_puntuadas_mill')
      p9 <- p9 + aes(Web, noticias_puntuadas_mill, fill = Suscripción)
    if (input$y9 == 'puntuacion_media')
      p9 <- p9 + aes(Web, puntuacion_media, fill = Suscripción)
    p9 <- p9 + geom_bar(position = "dodge", stat = "identity") +        # En este gráfico volvemos a posición dodge pero sin volteado para ofrecer una mayor claridad en los datos reflejados.
      theme_minimal() + 
      scale_fill_brewer(palette = "RdYlBu") +
      labs(x="", y="")
    print(p9)})
  
})


