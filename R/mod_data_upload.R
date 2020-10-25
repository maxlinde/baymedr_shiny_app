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
                        icon = "question",
                        colour = "red",
                        type = "markdown",
                        content = "upload"
                    ),
                selectInput(
                    inputId = ns("name_control"),
                    label = "Column name of the control condition",
                    choices = NULL
                ),
                selectInput(
                    inputId = ns("name_experimental"),
                    label = "Column name of the experimental condition",
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
                        inputId = "name_control",
                        choices = names(data()),
                        selected = ""
                    )
                    updateSelectInput(
                        session = session,
                        inputId = "name_experimental",
                        choices = names(data()),
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
                    data = data,
                    control = reactive(
                        x = {
                            input$name_control
                        }
                    ),
                    experimental = reactive(
                        x = {
                            input$name_experimental
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
