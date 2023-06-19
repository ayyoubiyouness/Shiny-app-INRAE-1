library(markdown)
library(shiny)
library(ggplot2)
library(readxl)
library(ggcorrplot)
library(tidyverse)


setwd("C:/Users/Youness/Desktop/R project/stage/shiny/inraeshiny")


Outputs_requirements <- read_excel("Outputs_requirements_cleaned.xlsx", col_names = TRUE )
Feed_Available <- read_excel("Feed_Available_cleaned.xlsx", col_names = TRUE )


Forage_ingest <-as.data.frame(by(Outputs_requirements$'Forage ingested', list(Outputs_requirements$cycle), mean)) 
Grain_ingest <- as.data.frame(by(Outputs_requirements$'Grain ingested', list(Outputs_requirements$cycle), mean))
Grassland_ingest <- as.data.frame( by(Outputs_requirements$'Grassland ingested', list(Outputs_requirements$cycle), mean))
Rangeland_ingest  <- as.data.frame(by(Outputs_requirements$'Rangeland ingested', list(Outputs_requirements$cycle), mean))
cycle <- as.data.frame(by(Outputs_requirements$'cycle', list(Outputs_requirements$cycle), mean)) 

Forage_available <-as.data.frame(by(Feed_Available$'Forage_available', list(Feed_Available$cycle), mean))
Grain_available <- as.data.frame(by(Feed_Available$'Grain_Available', list(Feed_Available$cycle), mean))
Grassland_available <- as.data.frame( by(Feed_Available$'Grasslands_Available', list(Feed_Available$cycle), mean))
Rangeland_available  <- as.data.frame(by(Feed_Available$'Rangelands_Available', list(Feed_Available$cycle), mean))

data <- cbind(Forage_ingest, Grain_ingest, Grassland_ingest, Rangeland_ingest,
              Forage_available, Grain_available, Grassland_available, Rangeland_available, cycle )

colnames(data) <- c(
  "Forage_ingest",
  "Grain_ingest",
  "Grassland_ingest",
  "Rangeland_ingest",
  "Forage_available",
  "Grain_available",
  "Grassland_available",
  "Rangeland_available",
  "cycle"
)

a <- c("Forage_available", "Grain_available", "Grassland_available", "Rangeland_available",
       "Rangeland_ingest", "Grassland_ingest", "Grain_ingest", "Forage_ingest", "cycle"  )



ui <- navbarPage("INRAE : Interface de visualisation ",
                 tabPanel("Courbe",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Choix des paramètres", align = "center"),
                              
                              # Select variable for y-axis
                              p("Choisir l'axe des ordonnées"),
                              selectInput(
                                inputId = "xcol",
                                label = "Y-axis:",
                                choices = a,
                                selected = "Forage_available"
                              ),
                              p("Choisir l'axe des abscisses"),
                              selectInput(
                                inputId = "ycol",
                                label = "X-axis:",
                                choices = a,
                                selected = "Forage_ingest"
                              ),
                              numericInput('clusters', 'Pour répartir les données', 2, min = 1, max = 9),
                              br(),
                              p("Pour Afficher la matrice de corrélation, cliquez sur la boutton"),
                              br(),
                              actionButton("go", "Correlation")
                              
                            ),
                            mainPanel(
                              h4("Représentation des graphes", align ="center"),
                              h4("Nuage de points", style = "color:blue"),
                              plotOutput('plot1'),
                              h4("Matrice de corrélation", style = "color:blue"),
                              plotOutput(outputId = "correlation")
    
    
                            )
                          )
                 ),
                 tabPanel("Résumé",
                          verbatimTextOutput("summary")
                 ),
                 navbarMenu("Indicateur",
                            tabPanel("table",
                                     DT::dataTableOutput("table")
                            ),
                            tabPanel("Indicateurs de disponibilité des aliments ",
                                     sidebarLayout(
                                       sidebarPanel("sidebar panel"),
                                       mainPanel("main panel")
                                     )
                                     ),
                            tabPanel("Indicateurs de consommation d'aliments  ",
                                     sidebarLayout(
                                       sidebarPanel("sidebar panel"),
                                       mainPanel("main panel")
                                     )
                            ),
                            tabPanel("Indicateurs de production de la ferme ",
                                     sidebarLayout(
                                       sidebarPanel("sidebar panel"),
                                       mainPanel("main panel")
                                     )
                            ),
                            
                            tabPanel("Indicateurs de performance du troupeau  ",
                                     sidebarLayout(
                                       sidebarPanel("sidebar panel"),
                                       mainPanel("main panel")
                                     )
                            ),
                            
                            tabPanel("younes",
                                     sidebarLayout(
                                       sidebarPanel("sidebar panel"),
                                       mainPanel("main panel")
                                     ))
                 )
)



server <- function(input, output, session) {
  
  selectedData <- reactive({
    data[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  randomVals <- eventReactive(input$go, {
    r <- cor(data)
    round(r, 2)
  })
  
  output$correlation <- renderPlot({
    ggcorrplot(randomVals(), lab = TRUE)
  })
  
  
  
  output$summary <- renderPrint({
    summary(data)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(data)
  })
}

shinyApp(ui = ui, server = server)


