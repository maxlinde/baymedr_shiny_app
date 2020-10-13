equivSummaryUi <- function(id) {
    
}

equivSummaryServer <- function(id) {
    
}

equivSummaryApp <- function() {
    ui <- fluidPage(
        equivSummaryUi(
            id = "equivSummaryUi"
        )
    )
    server <- function(input, output, session) {
        equivRawServer(
            id = "equivSummaryServer"
        )
    }
    shinyApp(ui, server)
}
