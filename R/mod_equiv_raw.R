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
