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
            id = "equivRaw"
        )
    )
    server <- function(input, output, session) {
        equivRawServer(
            id = "equivRaw"
        )
    }
    shinyApp(ui, server)
}
