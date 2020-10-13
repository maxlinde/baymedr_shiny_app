equivRawUi <- function(id) {
    
}

equivRawServer <- function(id) {
    
}

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
