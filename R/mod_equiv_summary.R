# module user interface
equivSummaryUi <- function(id) {
    ns <- NS(
        namespace = id
    )
    list(
        fluidRow(
            descriptives(id)
        ),
        tags$head(
            tags$style(
                HTML("
                    hr {
                        border: 1px solid #000000;
                    }
                ")
            )
        ),
        tags$hr(),
        fluidRow(
            column(
                width = 4,
                general_ui(ns("prior_scale")),
                plotOutput(ns("prior_scale_plot"))
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
        tags$hr(),
        fluidRow(
            general_ui(ns("submit"))
        ),
        tags$hr(),
        fluidRow(
            column(
                width = 6,
                results_ui(ns("results_show")),
                results_ui(ns("results_form"))
            ),
            column(
                width = 6,
                results_ui(ns("results_plot"))
            )
        )
    )
}

# module server
equivSummaryServer <- function(id) {
    moduleServer(
        id = id,
        module = function(input, output, session) {
            output$prior_scale_plot <- renderPlot(
                expr = {
                    prior_scale_plot(
                        prior_scale = input$prior_scale
                    )
                }
            )
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
            form <- eventReactive(
                eventExpr = {
                    input$submit
                },
                valueExpr = {
                    results_form(
                        id = id,
                        results()
                    )
                }
            )
            output$results_form <- renderUI(
                expr = {
                    form()
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
                height = 350
            )
        }
    )
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
