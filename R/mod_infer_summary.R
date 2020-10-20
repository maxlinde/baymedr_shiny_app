# module user interface
inferSummaryUi <- function(id) {
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
inferSummaryServer <- function(id) {
    moduleServer(
        id = id,
        module = function(input, output, session) {
            results <- eventReactive(
                eventExpr = {
                    input$submit
                },
                valueExpr = {
                    results_calculate(
                        id = id,
                        n_x = input$n_x,
                        n_y = input$n_y,
                        mean_x = input$mean_x,
                        mean_y = input$mean_y,
                        sd_x = input$sd_x,
                        sd_y = input$sd_y,
                        ci_margin = input$ci_margin,
                        ci_level = input$ci_level,
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
