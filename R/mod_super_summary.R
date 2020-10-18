# module user interface
superSummaryUi <- function(id) {
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
                super_ui(ns("direction"))
            ),
            column(
                width = 4,
                super_ui(ns("alternative"))
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
superSummaryServer <- function(id) {
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
                        prior_scale = input$prior_scale,
                        direction = input$direction,
                        alternative = input$alternative
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
                }
            )
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
