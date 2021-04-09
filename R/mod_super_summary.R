# module user interface
superSummaryUi <- function(id) {
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
                width = 6,
                general_ui(ns("prior_scale")),
                plotOutput(ns("prior_scale_plot"))
            ),
            column(
                width = 6,
                super_ui(ns("direction"))
            )
        ),
        tags$hr(),
        fluidRow(
            align = "center",
            general_ui(ns("submit"))
        ),
        tags$hr(),
        fluidRow(
            column(
                width = 6,
                results_ui(ns("results_show")),
                hidden(
                    div(
                        id = ns("freq_button_expand"),
                        actionButton(
                            inputId = ns("show_freq_expand"),
                            label = "Show frequentist results",
                            icon = icon("chevron-up"),
                            class = "btn btn-link btn-sm"
                        )
                    )
                ),
                hidden(
                    div(
                        id = ns("freq_button_collapse"),
                        actionButton(
                            inputId = ns("show_freq_collapse"),
                            label = "Hide frequentist results",
                            icon = icon("chevron-down"),
                            class = "btn btn-link btn-sm"
                        )
                    )
                ),
                hidden(
                    div(
                        id = ns("results_freq"),
                        verbatimTextOutput(
                            outputId = ns("res_freq")
                        )
                    )
                ),
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
superSummaryServer <- function(id) {
    moduleServer(
        id = id,
        module = function(input, output, session) {
            output$prior_scale_plot <- renderPlot(
                expr = {
                    prior_scale <- eval(
                        parse(
                            text = input$prior_scale
                        )
                    )
                    validate(need(
                        is.numeric(prior_scale) & prior_scale > 0,
                        str_c(
                            "The prior scale must evaluate to a positive ",
                            "numeric value."
                        )
                    ))
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
                    feedback <- feedback(
                        id = id,
                        n_x = input$n_x,
                        n_y = input$n_y,
                        sd_x = input$sd_x,
                        sd_y = input$sd_y,
                        ci_margin = input$ci_margin,
                        ci_level = input$ci_level
                    )
                    req(feedback)
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
                        choose_sd_ci = input$choose_sd_ci,
                        prior_scale = input$prior_scale,
                        direction = input$direction
                    )
                }
            )
            results_freq <- eventReactive(
                eventExpr = {
                    input$submit
                },
                valueExpr = {
                    feedback <- feedback(
                        id = id,
                        n_x = input$n_x,
                        n_y = input$n_y,
                        sd_x = input$sd_x,
                        sd_y = input$sd_y,
                        ci_margin = input$ci_margin,
                        ci_level = input$ci_level
                    )
                    req(feedback)
                    results_calculate_freq(
                        id = id,
                        n_x = input$n_x,
                        n_y = input$n_y,
                        mean_x = input$mean_x,
                        mean_y = input$mean_y,
                        sd_x = input$sd_x,
                        sd_y = input$sd_y,
                        ci_margin = input$ci_margin,
                        ci_level = input$ci_level,
                        choose_sd_ci = input$choose_sd_ci,
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
            observeEvent(
                eventExpr = {
                    input$submit
                },
                handlerExpr = {
                    shinyjs::show(
                        id = "freq_button_expand"
                    )
                    shinyjs::hide(
                        id = "freq_button_collapse"
                    )
                    shinyjs::hide(
                        id = "results_freq",
                        anim = TRUE
                    )
                }
            )
            observeEvent(
                eventExpr = {
                    input$show_freq_expand
                },
                handlerExpr = {
                    shinyjs::show(
                        id = "results_freq",
                        anim = TRUE
                    )
                    shinyjs::hide(
                        id = "freq_button_expand"
                    )
                    shinyjs::show(
                        id = "freq_button_collapse"
                    )
                }
            )
            observeEvent(
                eventExpr = {
                    input$show_freq_collapse
                },
                handlerExpr = {
                    shinyjs::hide(
                        id = "results_freq",
                        anim = TRUE
                    )
                    shinyjs::hide(
                        id = "freq_button_collapse"
                    )
                    shinyjs::show(
                        id = "freq_button_expand"
                    )
                }
            )
            show_freq <- eventReactive(
                eventExpr = {
                    input$submit
                },
                valueExpr = {
                    results_show_freq(
                        id = id,
                        results_freq()
                    )
                }
            )
            output$res_freq <- renderPrint(
                expr = {
                    show_freq()
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
