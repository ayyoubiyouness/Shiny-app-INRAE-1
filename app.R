library(shiny)
fg <- tibble(Scenario = rep("situation 1"), data2)
fa <- tibble(Scenario = rep("situation 2"), data2_s1)
fn <- rbind(fg, fa)
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("ContinentSelect", "Select which continents to include in the cluster analysis:", 
                         choices = c("situation 1", "situation 2")),
      
    ),
    
    mainPanel(
      
      highchartOutput("youness")
      
    )
  )
)

server <- function(input, output) {
  
  filtered_gap <- reactive({ #If no continents are selected
    if (is.null(input$ContinentSelect)) {
      return(NULL)
    }    
    
    fn %>%
      filter(Scenarion ==input$ContinentSelect)
  })
  output$youness <-  renderHighchart({
    hc <- filtered_gap() %>% 
      hchart('column', hcaes(x = 'Scenario', y = 'value', group = 'parameters')) %>%
      hc_colors(c("#0073C2FF", "#EFC000FF", "red"))
    
    
    hc
    
  })
  
}

shinyApp(ui, server)






fg <- tibble(Scenario = rep("situation 1"), data2)
fa <- tibble(Scenario = rep("situation 2"), data2_s1)
fn <- rbind(fg, fa)
hc <- fn %>% 
  hchart('column', hcaes(x = 'Scenario', y = 'value', group = 'parameters')) %>%
  hc_colors(c("#0073C2FF", "#EFC000FF", "red"))


hc
