# module user interface
superSummaryUi <- function(id) {
    
}

# module server
superSummaryServer <- function(id) {
    
}

# module app
superSummaryApp <- function() {
    ui <- fluidPage(
        superSummaryUi(
            id = "superSummary"
        )
    )
    server <- function(input, output, session) {
        superSummaryServer(
            id = "superSummary"
        )
    }
    shinyApp(ui, server)
}
