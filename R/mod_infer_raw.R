# module user interface
inferRawUi <- function(id) {
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
                infer_ui(ns("ni_margin")),
                infer_ui(ns("ni_margin_std"))
            ),
            column(
                width = 4,
                infer_ui(ns("direction"))
            )
        ),
        fluidRow(
            general_ui(ns("submit"))
        ),
        fluidRow(
            column(
                width = 6,
                results_ui(ns("results_show"))
            ),
            column(
                width = 6,
                results_ui(ns("results_plot"))
            )
        )
    )
}

# module server
inferRawServer <- function(id) {
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
                        ni_margin = input$ni_margin,
                        ni_margin_std = input$ni_margin_std,
                        prior_scale = input$prior_scale,
                        direction = input$direction
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
            plot <- eventReactive(
                eventExpr = {
                    input$submit
                },
                valueExpr = {
                    results_plot(
                        id = id,
                        results()
                    )
                }
            )
            output$results_plot <- renderPlot(
                expr = {
                    plot()
                },
                height = 700
            )
        }
    )
}

# module app
inferRawApp <- function() {
    ui <- fluidPage(
        inferRawUi(
            id = "inferRaw"
        )
    )
    server <- function(input, output, session) {
        inferRawServer(
            id = "inferRaw"
        )
    }
    shinyApp(ui, server)
}
