# module user interface
equivSummaryUi <- function(id) {
    
}

# module server
equivSummaryServer <- function(id) {
    
}

# module app
equivSummaryApp <- function() {
    ui <- fluidPage(
        equivSummaryUi(
            id = "equivSummary"
        )
    )
    server <- function(input, output, session) {
        equivSummaryServer(
            id = "equivSummary"
        )
    }
    shinyApp(ui, server)
}
