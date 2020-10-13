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
