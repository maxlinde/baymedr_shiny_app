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
                equiv_ui(ns("interval_low")),
                equiv_ui(ns("interval_high")),
                equiv_ui(ns("interval_std"))
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
                results_ui(ns("results_explain")),
                results_ui(ns("results_plot"))
            )
        )
    )
}

# module server
equivRawServer <- function(id) {
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
            dat <- dataUploadServer(
                id = "dataUpload"
            )
            results <- eventReactive(
                eventExpr = {
                    input$submit
                },
                valueExpr = {
                    validate(need(
                        is.numeric(dat$control()) & is.numeric(dat$experimental()),
                        "Data for the dependent variable must be numeric."
                    ))
                    validate(need(
                        all(!is.na(dat$control())) & all(!is.na(dat$experimental())),
                        "Data must not contain missing values."
                    ))
                    validate(need(
                        all(!is.infinite(dat$control())) & all(!is.infinite(dat$experimental())),
                        "Data must only contain finite values."
                    ))
                    validate(need(
                        input$interval_low <= input$interval_high,
                        str_c(
                            "The lower boundary of the equivalence interval ",
                            "must not be higher than the upper boundary of ",
                            "the equivalence interval."
                        )
                    ))
                    results_calculate(
                        id = id,
                        x = dat$control(),
                        y = dat$experimental(),
                        interval_low = input$interval_low,
                        interval_high = input$interval_high,
                        interval_std = input$interval_std,
                        prior_scale = input$prior_scale
                    )
                }
            )
            results_freq <- eventReactive(
                eventExpr = {
                    input$submit
                },
                valueExpr = {
                    validate(need(
                        is.numeric(dat$control()) & is.numeric(dat$experimental()),
                        "Data for the dependent variable must be numeric."
                    ))
                    validate(need(
                        all(!is.na(dat$control())) & all(!is.na(dat$experimental())),
                        "Data must not contain missing values."
                    ))
                    validate(need(
                        all(!is.infinite(dat$control())) & all(!is.infinite(dat$experimental())),
                        "Data must only contain finite values."
                    ))
                    validate(need(
                        input$interval_low <= input$interval_high,
                        str_c(
                            "The lower boundary of the equivalence interval ",
                            "must not be higher than the upper boundary of ",
                            "the equivalence interval."
                        )
                    ))
                    results_calculate_freq(
                        id = id,
                        x = dat$control(),
                        y = dat$experimental(),
                        interval_low = input$interval_low,
                        interval_high = input$interval_high,
                        interval_std = input$interval_std
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
            explain <- eventReactive(
                eventExpr = {
                    input$submit
                },
                valueExpr = {
                    results_explain(
                        id = id,
                        results()
                    )
                }
            )
            output$results_explain <- renderText(
                expr = {
                    explain()
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
