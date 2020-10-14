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
            id = "superRaw"
        )
    )
    server <- function(input, output, session) {
        superRawServer(
            id = "superRaw"
        )
    }
    shinyApp(ui, server)
}
