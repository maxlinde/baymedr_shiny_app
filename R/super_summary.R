superSummaryUi <- function(id) {
    
}

superSummaryServer <- function(id) {
    
}

superSummaryApp <- function() {
    ui <- fluidPage(
        superSummaryUi(
            id = "superSummaryUi"
        )
    )
    server <- function(input, output, session) {
        superSummaryServer(
            id = "superSummaryServer"
        )
    }
    shinyApp(ui, server)
}
