# module user interface
equivRawUi <- function(id) {
    ns <- NS(
        namespace = id
    )
    list(
        fluidRow(
            dataUploadUi(
                id = ns("dataUpload")
            )
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
        ),
        fluidRow(
            column(
                width = 6,
                results_ui(ns("results_show"))
            )
        )
    )
}

# module server
equivRawServer <- function(id) {
    moduleServer(
        id = id,
        module = function(input, output, session) {
            dat <- dataUploadServer(
                id = "dataUpload"
            )
            results <- eventReactive(
                eventExpr = {
                    input$submit
                },
                valueExpr = {
                    results_calculate(
                        id = id,
                        x = dat$data()[[dat$control()]],
                        y = dat$data()[[dat$experimental()]],
                        interval_low = input$interval_low,
                        interval_high = input$interval_high,
                        interval_std = input$interval_std,
                        prior_scale = input$prior_scale
                    )
                }
            )
            show <- eventReactive(
                eventExpr = {
                    input$submit
                },
                valueExpr = {
                    results_show(
                        id = id,
                        results()
                    )
                }
            )
            output$results_show <- renderPrint(
                expr = {
                    show()
                }
            )
        }
    )
}

# module app
equivRawApp <- function() {
    ui <- fluidPage(
        equivRawUi(
            id = "equivRaw"
        )
    )
    server <- function(input, output, session) {
        equivRawServer(
            id = "equivRaw"
        )
    }
    shinyApp(ui, server)
}
