# module user interface
inferSummaryUi <- function(id) {
    
}

# module server
inferSummaryServer <- function(id) {
    moduleServer(
        id = id,
        module = function(input, output, session) {
            
        }
    )
}

# module app
inferSummaryApp <- function() {
    ui <- fluidPage(
        inferSummaryUi(
            id = "inferSummary"
        )
    )
    server <- function(input, output, session) {
        inferSummaryServer(
            id = "inferSummary"
        )
    }
    shinyApp(ui, server)
}
