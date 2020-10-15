# module user interface
superSummaryUi <- function(id) {
    
}

# module server
superSummaryServer <- function(id) {
    moduleServer(
        id = id,
        module = function(input, output, session) {
            
        }
    )
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
