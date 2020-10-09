# load packages
library("shiny")
library("shinythemes")

# user interface
ui <- fluidPage(
   theme = shinytheme(theme = "cosmo") 
)

# server
server <- function(input, output, session) {
    
}

# app
shinyApp(ui, server)