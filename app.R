# Load packages 
library(shiny)
library(ggplot2)
library(readxl)
library(ggcorrplot)


# Get the data
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

# Define UI 

ui <- fluidPage(
  titlePanel("Interface et Visualisation de données de simulation : "),
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      # img( src = "www/inraeimage.png" , height = 40, width = 40),
      
      h4("Choix des paramètres", align = "center"),
      
      # Select variable for y-axis
      p("Choisir l'axe des ordonnées"),
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = a,
        selected = "Forage_available"
      ),
      # Select variable for x-axis
      p("Choisir l'axe des abscisses"),
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = a,
        selected = "Forage_ingest"
      ),
      br(),
      p("Pour Afficher la matrice de corrélation, cliquez sur la boutton"),
      br(),
      actionButton("go", "Correlation"),
      
    ),
    
    # Output: Show scatterplot
    mainPanel(
      h4("Représentation des graphes", align ="center"),
      
      h4("Nuage de points", style = "color:blue"),
      plotOutput(outputId = "scatterplot"),
      
      h4("Matrice de corrélation", style = "color:blue"),
      plotOutput(outputId = "correlation")
      
    )
  )
)



# Define server 

server <- function(input, output, session) {
  output$scatterplot <- renderPlot({
    ggplot(data = data, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  
  randomVals <- eventReactive(input$go, {
    r <- cor(data)
    round(r, 2)
  })
  output$correlation <- renderPlot({
    ggcorrplot(randomVals(), lab = TRUE )
  })
  
}

# Create a Shiny app object 

shinyApp(ui = ui, server = server)

# randomVals <- eventReactive(input$go, {
#   runif(input$n)
# })
# 
# output$plot <- renderPlot({
#   hist(randomVals())
# })


