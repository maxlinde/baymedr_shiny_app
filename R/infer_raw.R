inferRawUi <- function(id) {
    
}

inferRawServer <- function(id) {
    
}

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
