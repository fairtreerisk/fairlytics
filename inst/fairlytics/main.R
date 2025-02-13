library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    html, body { 
      overflow: hidden !important; 
      height: 100%; 
      margin: 0; 
    }
    
    iframe { 
      width: 100%; 
      height: 100vh; 
      border: none; 
      overflow: auto; 
    }"
      )
    )
  ),
  
 tags$iframe(src = "index.html",
             width = "100%",
             height = "900px",
             style = "border:none; overflow: auto;"
             )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
