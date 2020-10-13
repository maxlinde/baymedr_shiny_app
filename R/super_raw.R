# module user interface
superRawUi <- function(id) {
    
}

# module server
superRawServer <- function(id) {
    
}

# module app
superRawApp <- function() {
    ui <- fluidPage(
        superRawUi(
            id = "superRawUi"
        )
    )
    server <- function(input, output, session) {
        superRawServer(
            id = "superRawServer"
        )
    }
    shinyApp(ui, server)
}
