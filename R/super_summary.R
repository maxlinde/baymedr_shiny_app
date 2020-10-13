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
