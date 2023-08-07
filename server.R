server <-  function(input, output) { 
  shinyjs::onclick("situation0",
                   shinyjs::toggle(id = "scenario0", anim = TRUE),
                   
                   )
  shinyjs::onclick("situationcc",
                   shinyjs::toggle(id = "scenario1", anim = TRUE),
                   
                   )
  shinyjs::onclick("strat1",
                   shinyjs::toggle(id = "scenario2", anim = TRUE),
                   
  )
  
  # -------------------------------------------------------------------------Home
  output$home_img <- renderImage({
    
    list(src = "C:/Users/Youness/Desktop/R project/stage/shiny/withjs/www/imgaccueil.jpg",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  # -------------------------------------------------------------------------Scénario 0
  # paramètres d'entrés
  # Vis 1
  observeEvent(input$item_choice, {
    
    hcpxy <- highchartProxy("animated")
    
    if(input$item_choice == "parliment") {
      hcpxy %>%
        hcpxy_update_series(
          id = "serieid",
          center = list('50%', '88%'),
          size = '170%',
          startAngle = -100,
          endAngle = 100
        )
    } else if (input$item_choice == "rectangle") {
      hcpxy %>%
        hcpxy_update_series(
          id = "serieid",
          startAngle = NULL,
          endAngle = NULL
        )
    } else if (input$item_choice == "circle") {
      hcpxy %>%
        hcpxy_update_series(
          id = "serieid",
          center = list('50%', '50%'),
          size = '100%',
          startAngle = 0,
          endAngle = 360
        )
    }
    
  })
  
  output$animated <- renderHighchart ({
    # Sys.sleep(1.5)
    hchart(
      data2,
      "item", 
      hcaes(name = parameters, y = value),
      name = "Nombre ",
      id = "serieid"
    )%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
  })
  
  # Vis 2
  output$ressource_surface <- renderHighchart({
    # Sys.sleep(1.5)
    hc <- highchart() %>%
      hc_add_series(type="column",name = "<b>Par type de ressource et par type d'usage</b>", data = plot_man$taille )%>%
      hc_add_series( type = 'scatter',name = " <b>Rendement</b>", data = rendement$valeur, yAxis = 1 )%>%
      hc_xAxis(categories = plot_man$Surface)%>%
      hc_yAxis_multiples(
        list(lineWidth = 0,
             title = list(text = "Par type de ressource et par type d'usage")),
        list(showLastLabel = FALSE, opposite = TRUE,
             title = list(text = "rendement"))
      ) %>%
      
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    
    hc
  })
  
  # Vis 3
  output$stade_physio <- renderDT({
    # Sys.sleep(1.5)
    datatable(
      stadephysio,
      caption = 'Need according to physiological stages ',
      options = list(pageLength = 8, dom = 'tip'))%>%
      formatStyle('value',  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')
    
    
  })
  # Paramètres de sortie 
  # Vis a
  bins <- reactive({
    input$radio
    if (input$radio == 1) {
      dataa <- mois(data_radio)
    } else if ((input$radio == 2)) {
      dataa <- saison(data_radio)
    } else if ((input$radio == 3)) {
      dataa <- quinzine(data_radio)
    } else {
      dataa <- data_radio
    }
    
  })
  
  output$plot <- renderHighchart({
    # Sys.sleep(1.5)
    h1 <- highchart() %>% 
      hc_xAxis(categories = bins()$Date) %>% 
      hc_add_series(name = "Herd_Requirement_mean", 
                    data = bins()$mean) %>%
      hc_add_series(name = "Herd_Requirement_min",
                    data = bins()$min) %>%
      hc_add_series(name = "Herd_Requirement_max",
                    data = bins()$max) %>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    # Afficher le graphique
    h1
  })
  
  # Vis b
  output$ressour_consom <- renderHighchart({
    # Sys.sleep(1.5)
    hc <- data3_radio %>% 
      hchart('areaspline', hcaes(x = 'Date', y = 'points', group = "year"), stacking = "normal") %>%
      hc_add_series(data_radio, type = 'line', hcaes(x = 'Date', y = 'mean'))%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
  })
  
  # Indicateur de performance
  # Vis c
  output$forage_ingestt <- renderInfoBox({
    
    infoBox(
      "", forage_ingest_par_fememlle , " Fourrage consommé par chaque femelle ", 
      icon = tags$i(class = "fa-solid fa-wheat-awn"),
      color = "light-blue"
    )
  })
  
  # Vis d
  output$grain_ingestt <- renderInfoBox({
    
    infoBox(
      "", grain_ingest_par_fememlle , " Grain consommé par chaque femelle ", 
      icon = tags$i(class = "fa-solid fa-wheat-awn-circle-exclamation"),
      color = "light-blue"
    )
  })
  
  # Vis e
  output$ressour_consom_produit <-  renderHighchart({
    #Sys.sleep(1.5)
    hc <- data_ressource %>% 
      hchart('column', hcaes(x = 'type', y = 'Qte', group = 'Nature')) %>%
      hc_colors(c("#0073C2FF", "#EFC000FF"))
    
    
    hc
  })
  
  # Vis f
  output$gauge1 <- renderHighchart({
    a <- round(taux_paturage*100)
    col_stops <- data.frame(
      q = c(0.15, 0.4, .8),
      c = c('#55BF3B', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    
    
    hc <- 
      highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        number = list(suffix = "%"),
        data = a,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>% 
      hc_size(height = 400)%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
    
  })
  
  # Vis g
  
  output$gauge2 <- renderHighchart({
    a <- round(auto_four*100)
    col_stops <- data.frame(
      q = c(0.15, 0.4, .8),
      c = c('#55BF3B', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    hc <- 
      highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data = a,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>% 
      hc_size(height = 400)%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
    
  })
  
  # Vis h
  
  output$gauge3 <- renderHighchart({
    a <- round(auto_concen*100)
    col_stops <- data.frame(
      q = c(0.15, 0.4, .8),
      c = c('#55BF3B', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    hc <- 
      highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = TRUE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data = a,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>% 
      hc_size(height = 400)%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
    
  })
  
  
  
  # -------------------------------------------------------------------------Scénario 1
  # paramètres d'entrés
  # Vis 1
  observeEvent(input$item_choice_s1, {
    
    hcpxy <- highchartProxy("animated")
    
    if(input$item_choice_s1 == "parliment") {
      hcpxy %>%
        hcpxy_update_series(
          id = "serieid",
          center = list('50%', '88%'),
          size = '170%',
          startAngle = -100,
          endAngle = 100
        )
    } else if (input$item_choice_s1 == "rectangle") {
      hcpxy %>%
        hcpxy_update_series(
          id = "serieid",
          startAngle = NULL,
          endAngle = NULL
        )
    } else if (input$item_choice_s1 == "circle") {
      hcpxy %>%
        hcpxy_update_series(
          id = "serieid",
          center = list('50%', '50%'),
          size = '100%',
          startAngle = 0,
          endAngle = 360
        )
    }
    
  })
  
  output$animated_s1 <- renderHighchart ({
    # Sys.sleep(1.5)
    hchart(
      data2_s1,
      "item", 
      hcaes(name = parameters, y = value),
      name = "Nombre ",
      id = "serieid"
    )%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
  })
  
  # Vis 2
  output$ressource_surface_s1 <- renderHighchart({
    # Sys.sleep(1.5)
    hc <- highchart() %>%
      hc_add_series(type="column",name = "<b>Par type de ressource et par type d'usage</b>", data = plot_man_s1$taille )%>%
      hc_add_series( type = 'scatter',name = " <b>Rendement</b>", data = rendement_s1$valeur, yAxis = 1 )%>%
      hc_xAxis(categories = plot_man_s1$Surface)%>%
      hc_yAxis_multiples(
        list(lineWidth = 0,
             title = list(text = "Par type de ressource et par type d'usage")),
        list(showLastLabel = FALSE, opposite = TRUE,
             title = list(text = "rendement"))
      ) %>%
      
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    
    hc
  })
  
  # Vis 3
  output$stade_physio_s1 <- renderHighchart({
    # Sys.sleep(1.5)
    hc <- stadephysio_s1 %>%
      hchart(
        'bar', hcaes(x = parameters, y = value),
        color = "lightgray", borderColor = "black"
      )%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
  })
  # Paramètres de sortie 
  # Vis a
  bins_s1 <- reactive({
    input$radio
    if (input$radio == 1) {
      dataa_s1 <- mois(data_radio_s1)
    } else if ((input$radio == 2)) {
      dataa_s1 <- saison(data_radio_s1)
    } else if ((input$radio == 3)) {
      dataa_s1 <- quinzine(data_radio_s1)
    } else {
      dataa_s1 <- data_radio_s1
    }
    
  })
  
  output$plot_s1 <- renderHighchart({
    # Sys.sleep(1.5)
    h1 <- highchart() %>% 
      hc_xAxis(categories = bins_s1()$Date) %>% 
      hc_add_series(name = "Herd_Requirement_mean", 
                    data = bins_s1()$mean) %>%
      hc_add_series(name = "Herd_Requirement_min",
                    data = bins_s1()$min) %>%
      hc_add_series(name = "Herd_Requirement_max",
                    data = bins_s1()$max) %>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    # Afficher le graphique
    h1
  })
  
  # Vis b
  output$ressour_consom_s1 <- renderHighchart({
    # Sys.sleep(1.5)
    hc <- data3_radio_s1 %>% 
      hchart('areaspline', hcaes(x = 'Date', y = 'points', group = "year"), stacking = "normal") %>%
      hc_add_series(data_radio_s1, type = 'line', hcaes(x = 'Date', y = 'mean'))%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
  })
  
  # Indicateur de performance
  # Vis c
  output$forage_ingestt_s1 <- renderInfoBox({
    
    infoBox(
      "", forage_ingest_par_fememlle_s1 , " Fourrage consommé par chaque femelle ", 
      icon = tags$i(class = "fa-solid fa-wheat-awn"),
      color = "light-blue"
    )
  })
  
  # Vis d
  output$grain_ingestt_s1 <- renderInfoBox({
    
    infoBox(
      "", grain_ingest_par_fememlle_s1 , " Grain consommé par chaque femelle ", 
      icon = tags$i(class = "fa-solid fa-wheat-awn-circle-exclamation"),
      color = "light-blue"
    )
  })
  
  # Vis e
  output$ressour_consom_produit_s1 <-  renderHighchart({
    #Sys.sleep(1.5)
    hc <- data_ressource_s1 %>% 
      hchart('column', hcaes(x = 'type', y = 'Qte', group = 'Nature')) %>%
      hc_colors(c("#0073C2FF", "#EFC000FF"))
    
    
    hc
  })
  
  # Vis f
  output$gauge1_s1 <- renderHighchart({
    a <- round(taux_paturage_s1*100)
    col_stops <- data.frame(
      q = c(0.15, 0.4, .8),
      c = c('#55BF3B', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    
    
    hc <- 
      highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        number = list(suffix = "%"),
        data = a,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>% 
      hc_size(height = 400)%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
    
  })
  
  # Vis g
  
  output$gauge2_s1 <- renderHighchart({
    a <- round(auto_four_s1*100)
    col_stops <- data.frame(
      q = c(0.15, 0.4, .8),
      c = c('#55BF3B', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    hc <- 
      highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data = a,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>% 
      hc_size(height = 400)%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
    
  })
  
  # Vis h
  
  output$gauge3_s1 <- renderHighchart({
    a <- round(auto_concen_s1*100)
    col_stops <- data.frame(
      q = c(0.15, 0.4, .8),
      c = c('#55BF3B', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    hc <- 
      highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = TRUE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data = a,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>% 
      hc_size(height = 400)%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
    
  })
  
  # -------------------------------------------------------------------------Scénario 2
  # paramètres d'entrés
  # Vis 1
  observeEvent(input$item_choice_s2, {
    
    hcpxy <- highchartProxy("animated_s2")
    
    if(input$item_choice_s2 == "parliment") {
      hcpxy %>%
        hcpxy_update_series(
          id = "serieid",
          center = list('50%', '88%'),
          size = '170%',
          startAngle = -100,
          endAngle = 100
        )
    } else if (input$item_choice_s2 == "rectangle") {
      hcpxy %>%
        hcpxy_update_series(
          id = "serieid",
          startAngle = NULL,
          endAngle = NULL
        )
    } else if (input$item_choice_s2 == "circle") {
      hcpxy %>%
        hcpxy_update_series(
          id = "serieid",
          center = list('50%', '50%'),
          size = '100%',
          startAngle = 0,
          endAngle = 360
        )
    }
    
  })
  
  output$animated_s2 <- renderHighchart ({
    # Sys.sleep(1.5)
    hchart(
      data2_s2,
      "item", 
      hcaes(name = parameters, y = value),
      name = "Nombre ",
      id = "serieid"
    )%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
  })
  
  # Vis 2
  output$ressource_surface_s2 <- renderHighchart({
    # Sys.sleep(1.5)
    hc <- highchart() %>%
      hc_add_series(type="column",name = "<b>Par type de ressource et par type d'usage</b>", data = plot_man_s2$taille )%>%
      hc_add_series( type = 'scatter',name = " <b>Rendement</b>", data = rendement_s2$valeur, yAxis = 1 )%>%
      hc_xAxis(categories = plot_man_s2$Surface)%>%
      hc_yAxis_multiples(
        list(lineWidth = 0,
             title = list(text = "Par type de ressource et par type d'usage")),
        list(showLastLabel = FALSE, opposite = TRUE,
             title = list(text = "rendement"))
      ) %>%
      
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    
    hc
  })
  
  # Vis 3
  output$stade_physio_s2 <- renderHighchart({
    # Sys.sleep(1.5)
    hc <- stadephysio_s2 %>%
      hchart(
        'bar', hcaes(x = parameters, y = value),
        color = "lightgray", borderColor = "black"
      )%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
  })
  # Paramètres de sortie 
  # Vis a
  bins_s2 <- reactive({
    input$radio
    if (input$radio == 1) {
      dataa_s2 <- mois(data_radio_s2)
    } else if ((input$radio == 2)) {
      dataa_s2 <- saison(data_radio_s2)
    } else if ((input$radio == 3)) {
      dataa_s2 <- quinzine(data_radio_s2)
    } else {
      dataa_s2 <- data_radio_s2
    }
    
  })
  
  output$plot_s2 <- renderHighchart({
    # Sys.sleep(1.5)
    h1 <- highchart() %>% 
      hc_xAxis(categories = bins_s2()$Date) %>% 
      hc_add_series(name = "Herd_Requirement_mean", 
                    data = bins_s2()$mean) %>%
      hc_add_series(name = "Herd_Requirement_min",
                    data = bins_s2()$min) %>%
      hc_add_series(name = "Herd_Requirement_max",
                    data = bins_s2()$max) %>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    # Afficher le graphique
    h1
  })
  
  # Vis b
  output$ressour_consom_s2 <- renderHighchart({
    # Sys.sleep(1.5)
    hc <- data3_radio_s2 %>% 
      hchart('areaspline', hcaes(x = 'Date', y = 'points', group = "year"), stacking = "normal") %>%
      hc_add_series(data_radio_s2, type = 'line', hcaes(x = 'Date', y = 'mean'))%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
  })
  
  # Indicateur de performance
  # Vis c
  output$forage_ingestt_s2 <- renderInfoBox({
    
    infoBox(
      "", forage_ingest_par_fememlle_s2 , " Fourrage consommé par chaque femelle ", 
      icon = tags$i(class = "fa-solid fa-wheat-awn"),
      color = "light-blue"
    )
  })
  
  # Vis d
  output$grain_ingestt_s2 <- renderInfoBox({
    
    infoBox(
      "", grain_ingest_par_fememlle_s2 , " Grain consommé par chaque femelle ", 
      icon = tags$i(class = "fa-solid fa-wheat-awn-circle-exclamation"),
      color = "light-blue"
    )
  })
  
  # Vis e
  output$ressour_consom_produit_s2 <-  renderHighchart({
    #Sys.sleep(1.5)
    hc <- data_ressource_s2 %>% 
      hchart('column', hcaes(x = 'type', y = 'Qte', group = 'Nature')) %>%
      hc_colors(c("#0073C2FF", "#EFC000FF"))
    
    
    hc
  })
  
  # Vis f
  output$gauge1_s2 <- renderHighchart({
    a <- round(taux_paturage_s2*100)
    col_stops <- data.frame(
      q = c(0.15, 0.4, .8),
      c = c('#55BF3B', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    
    
    hc <- 
      highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        number = list(suffix = "%"),
        data = a,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>% 
      hc_size(height = 400)%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
    
  })
  
  # Vis g
  
  output$gauge2_s2 <- renderHighchart({
    a <- round(auto_four_s2*100)
    col_stops <- data.frame(
      q = c(0.15, 0.4, .8),
      c = c('#55BF3B', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    hc <- 
      highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data = a,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>% 
      hc_size(height = 400)%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
    
  })
  
  # Vis h
  
  output$gauge3_s2 <- renderHighchart({
    a <- round(auto_concen_s2*100)
    col_stops <- data.frame(
      q = c(0.15, 0.4, .8),
      c = c('#55BF3B', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    hc <- 
      highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = TRUE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data = a,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>% 
      hc_size(height = 400)%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
    
  })
  
  # -------------------------------------------------------------------------Scénario 3
  # paramètres d'entrés
  # Vis 1
  observeEvent(input$item_choice_s3, {
    
    hcpxy <- highchartProxy("animated")
    
    if(input$item_choice_s3 == "parliment") {
      hcpxy %>%
        hcpxy_update_series(
          id = "serieid",
          center = list('50%', '88%'),
          size = '170%',
          startAngle = -100,
          endAngle = 100
        )
    } else if (input$item_choice_s3 == "rectangle") {
      hcpxy %>%
        hcpxy_update_series(
          id = "serieid",
          startAngle = NULL,
          endAngle = NULL
        )
    } else if (input$item_choice_s3 == "circle") {
      hcpxy %>%
        hcpxy_update_series(
          id = "serieid",
          center = list('50%', '50%'),
          size = '100%',
          startAngle = 0,
          endAngle = 360
        )
    }
    
  })
  
  output$animated_s3 <- renderHighchart ({
    # Sys.sleep(1.5)
    hchart(
      data2_s3,
      "item", 
      hcaes(name = parameters, y = value),
      name = "Nombre ",
      id = "serieid"
    )%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
  })
  
  # Vis 2
  output$ressource_surface_s3 <- renderHighchart({
    # Sys.sleep(1.5)
    hc <- highchart() %>%
      hc_add_series(type="column",name = "<b>Par type de ressource et par type d'usage</b>", data = plot_man_s3$taille )%>%
      hc_add_series( type = 'scatter',name = " <b>Rendement</b>", data = rendement_s3$valeur, yAxis = 1 )%>%
      hc_xAxis(categories = plot_man_s3$Surface)%>%
      hc_yAxis_multiples(
        list(lineWidth = 0,
             title = list(text = "Par type de ressource et par type d'usage")),
        list(showLastLabel = FALSE, opposite = TRUE,
             title = list(text = "rendement"))
      ) %>%
      
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    
    hc
  })
  
  # Vis 3
  output$stade_physio_s3 <- renderHighchart({
    # Sys.sleep(1.5)
    hc <- stadephysio_s3 %>%
      hchart(
        'bar', hcaes(x = parameters, y = value),
        color = "lightgray", borderColor = "black"
      )%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
  })
  # Paramètres de sortie 
  # Vis a
  bins_s3 <- reactive({
    input$radio
    if (input$radio == 1) {
      dataa_s3 <- mois(data_radio_s3)
    } else if ((input$radio == 2)) {
      dataa_s3 <- saison(data_radio_s3)
    } else if ((input$radio == 3)) {
      dataa_s3 <- quinzine(data_radio_s3)
    } else {
      dataa_s3 <- data_radio_s3
    }
    
  })
  
  output$plot_s3 <- renderHighchart({
    # Sys.sleep(1.5)
    h1 <- highchart() %>% 
      hc_xAxis(categories = bins_s3()$Date) %>% 
      hc_add_series(name = "Herd_Requirement_mean", 
                    data = bins_s3()$mean) %>%
      hc_add_series(name = "Herd_Requirement_min",
                    data = bins_s3()$min) %>%
      hc_add_series(name = "Herd_Requirement_max",
                    data = bins_s3()$max) %>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    # Afficher le graphique
    h1
  })
  
  # Vis b
  output$ressour_consom_s3 <- renderHighchart({
    # Sys.sleep(1.5)
    hc <- data3_radio_s3 %>% 
      hchart('areaspline', hcaes(x = 'Date', y = 'points', group = "year"), stacking = "normal") %>%
      hc_add_series(data_radio_s3, type = 'line', hcaes(x = 'Date', y = 'mean'))%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
  })
  
  # Indicateur de performance
  # Vis c
  output$forage_ingestt_s3 <- renderInfoBox({
    
    infoBox(
      "", forage_ingest_par_fememlle_s3 , " Fourrage consommé par chaque femelle ", 
      icon = tags$i(class = "fa-solid fa-wheat-awn"),
      color = "light-blue"
    )
  })
  
  # Vis d
  output$grain_ingestt_s3 <- renderInfoBox({
    
    infoBox(
      "", grain_ingest_par_fememlle_s3 , " Grain consommé par chaque femelle ", 
      icon = tags$i(class = "fa-solid fa-wheat-awn-circle-exclamation"),
      color = "light-blue"
    )
  })
  
  # Vis e
  output$ressour_consom_produit_s3 <-  renderHighchart({
    #Sys.sleep(1.5)
    hc <- data_ressource_s3 %>% 
      hchart('column', hcaes(x = 'type', y = 'Qte', group = 'Nature')) %>%
      hc_colors(c("#0073C2FF", "#EFC000FF"))
    
    
    hc
  })
  
  # Vis f
  output$gauge1_s3 <- renderHighchart({
    a <- round(taux_paturage_s3*100)
    col_stops <- data.frame(
      q = c(0.15, 0.4, .8),
      c = c('#55BF3B', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    
    
    hc <- 
      highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        number = list(suffix = "%"),
        data = a,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>% 
      hc_size(height = 400)%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
    
  })
  
  # Vis g
  
  output$gauge2_s3 <- renderHighchart({
    a <- round(auto_four_s3*100)
    col_stops <- data.frame(
      q = c(0.15, 0.4, .8),
      c = c('#55BF3B', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    hc <- 
      highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data = a,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>% 
      hc_size(height = 400)%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
    
  })
  
  # Vis h
  
  output$gauge3_s3 <- renderHighchart({
    a <- round(auto_concen_s3*100)
    col_stops <- data.frame(
      q = c(0.15, 0.4, .8),
      c = c('#55BF3B', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    hc <- 
      highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = TRUE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data = a,
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          style = list(fontSize = "40px")
        )
      ) %>% 
      hc_size(height = 400)%>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      )
    hc
    
    
  })
  
}
shinyApp(ui, server)
