# module user interface
equivRawUi <- function(id) {
    ns <- NS(
        namespace = id
    )
    list(
        fluidRow(
            dataUploadUi(
                id = "dataUpload"
            )
        ),
        fluidRow(
            column(
                width = 4,
                prior_scale(ns("prior_scale"))
            )
        )
    )
}

# module server
equivRawServer <- function(id) {
    dataUploadServer(
        id = "dataUpload"
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
