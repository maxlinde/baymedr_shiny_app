# module user interface
equivRawUi <- function(id) {
    
}

# module server
equivRawServer <- function(id) {
    
}

# module app
equivRawApp <- function() {
    ui <- fluidPage(
        equivRawUi(
            id = "equivRawUi"
        )
    )
    server <- function(input, output, session) {
        equivRawServer(
            id = "equivRawServer"
        )
    }
    shinyApp(ui, server)
}
