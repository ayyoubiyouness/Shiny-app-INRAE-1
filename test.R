fig <- plotly::plot_ly(
  type = "indicator",
  mode = "gauge+number",
  number = list(suffix = "%"),  
  value = 60,
  #title = list(text = "Uptake first dose %", font = list(size = 24)),
  #delta = list(reference = 70, increasing = list(color = "gray")),
  gauge = list(
    axis = list(range = list(NULL, 100), tickwidth = 1, tickcolor = "lightgreen"),
    bar = list(color = "lightgreen"),
    bgcolor = "white",
    borderwidth = 1,
    bordercolor = "gray",
    steps = list(
      #list(range = c(0, 50), color = "lightgreen"),
      list(range = c(0, 100), color = "gray")))
    # threshold = list(
    #   line = list(color = "black", width = 4),
    #   thickness = 0.75,
    #   value = 70)
    
  )
fig <- fig %>%
  plotly::layout(
    margin = list(l=20,r=30),
    paper_bgcolor = "lavender",
    font = list(color = "darkblue", family = "Arial"),
    annotations = list(x = 0.1, y = 0.3, text = "phase 1",
                       hovertext = "Your Text",
                       showarrow = FALSE))

fig 
