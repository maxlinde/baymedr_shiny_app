# module user interface
inferRawUi <- function(id) {
    
}

# module server
inferRawServer <- function(id) {
    
}

# module app
inferRawApp <- function() {
    ui <- fluidPage(
        inferRawUi(
            id = "inferRawUi"
        )
    )
    server <- function(input, output, session) {
        inferRawServer(
            id = "inferRawServer"
        )
    }
    shinyApp(ui, server)
}
