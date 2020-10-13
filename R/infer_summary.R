# module user interface
inferSummaryUi <- function(id) {
    
}

# module server
inferSummaryServer <- function(id) {
    
}

# module app
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
