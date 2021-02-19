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
                infer_ui(ns("ni_margin")),
                infer_ui(ns("ni_margin_std")),
                infer_ui(ns("direction"))
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
inferRawServer <- function(id) {
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
            dat <- dataUploadServer(
                id = "dataUpload"
            )
            results <- eventReactive(
                eventExpr = {
                    input$submit
                },
                valueExpr = {
                    ni_margin_feed <- input$ni_margin >= 0
                    feedbackWarning(
                        inputId = "ni_margin",
                        show = !ni_margin_feed,
                        text = "The non-inferiority margin must not be lower than 0."
                    )
                    req(ni_margin_feed)
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
            results_freq <- eventReactive(
                eventExpr = {
                    input$submit
                },
                valueExpr = {
                    ni_margin_feed <- input$ni_margin >= 0
                    feedbackWarning(
                        inputId = "ni_margin",
                        show = !ni_margin_feed,
                        text = "The non-inferiority margin must not be lower than 0."
                    )
                    req(ni_margin_feed)
                    results_calculate_freq(
                        id = id,
                        x = dat$data()[[dat$control()]],
                        y = dat$data()[[dat$experimental()]],
                        ni_margin = input$ni_margin,
                        ni_margin_std = input$ni_margin_std,
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
