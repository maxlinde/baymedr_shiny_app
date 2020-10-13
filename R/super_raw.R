superRawUi <- function(id) {
    
}

superRawServer <- function(id) {
    
}

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
