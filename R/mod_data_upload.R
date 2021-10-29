# module user interface
dataUploadUi <- function(id) {
    ns <- NS(
        namespace = id
    )
    list(
        sidebarLayout(
            sidebarPanel(
                fileInput(
                    inputId = ns("upload"),
                    label = "Upload data file"
                ) %>%
                    helper(
                        icon = "question-circle",
                        colour = "green",
                        type = "markdown",
                        content = "upload"
                    ),
                selectInput(
                    inputId = ns("name_dv"),
                    label = "Column name for the dependent variable",
                    choices = NULL
                ),
                selectInput(
                    inputId = ns("name_iv"),
                    label = "Column name for the independent (grouping) variable",
                    choices = NULL
                ),
                selectInput(
                    inputId = ns("name_control"),
                    label = "Name of the control group",
                    choices = NULL
                ),
                selectInput(
                    inputId = ns("name_experimental"),
                    label = "Name of the experimental group",
                    choices = NULL
                )
            ),
            mainPanel(
                tableOutput(
                    outputId = ns("data_table")
                )
            )
        )
    )
}

# module server
dataUploadServer <- function(id) {
    moduleServer(
        id = id,
        module = function(input, output, session) {
            data <- reactive(
                x = {
                    req(input$upload)
                    df <- vroom(
                        file = input$upload$datapath
                    )
                }
            )
            observeEvent(
                eventExpr = {
                    data()
                },
                handlerExpr = {
                    updateSelectInput(
                        session = session,
                        inputId = "name_dv",
                        choices = names(data()),
                        selected = ""
                    )
                    updateSelectInput(
                        session = session,
                        inputId = "name_iv",
                        choices = names(data()),
                        selected = ""
                    )
                }
            )
            observeEvent(
                eventExpr = {
                    input$name_iv
                },
                handlerExpr = {
                    updateSelectInput(
                        session = session,
                        inputId = "name_control",
                        choices = unique(data()[[input$name_iv]]),
                        selected = ""
                    )
                    updateSelectInput(
                        session = session,
                        inputId = "name_experimental",
                        choices = unique(data()[[input$name_iv]]),
                        selected = ""
                    )
                }
            )
            output$data_table <- function() {
                kable(x = data()) %>%
                    kable_styling(bootstrap_options = c("striped", "hover")) %>%
                    scroll_box(width = "100%", height = "400px")
            }
            return(
                list(
                    control = reactive(
                        x = {
                            data()[data()[input$name_iv] == input$name_control, ][[input$name_dv]]
                        }
                    ),
                    experimental = reactive(
                        x = {
                            data()[data()[input$name_iv] == input$name_experimental, ][[input$name_dv]]
                        }
                    )
                )
            )
        }
    )
}

# module app
dataUploadApp <- function() {
    ui <- fluidPage(
        dataUploadUi(
            id = "dataUpload"
        )
    )
    server <- function(input, output, session) {
        dataUploadServer(
            id = "dataUpload"
        )
    }
    shinyApp(ui, server)
}
