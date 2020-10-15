# module user interface
equivSummaryUi <- function(id) {
    ns <- NS(
        namespace = id
    )
    list(
        fluidRow(
            descriptives(id)
        ),
        fluidRow(
            column(
                width = 4,
                general_ui(ns("prior_scale"))
            ),
            column(
                width = 4,
                equiv_ui(ns("interval_low")),
                equiv_ui(ns("interval_high"))
            ),
            column(
                width = 4,
                equiv_ui(ns("interval_std"))
            )
        ),
        fluidRow(
            general_ui(ns("submit"))
        )
    )
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
