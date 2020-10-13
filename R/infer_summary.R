inferSummaryUi <- function(id) {
    
}

inferSummaryServer <- function(id) {
    
}

inferSummaryApp <- function() {
    ui <- fluidPage(
        inferSummaryUi(
            id = "inferSummaryUi"
        )
    )
    server <- function(input, output, session) {
        inferSummaryServer(
            id = "inferSummaryServer"
        )
    }
    shinyApp(ui, server)
}
