# module user interface
inferRawUi <- function(id) {
    
}

# module server
inferRawServer <- function(id) {
    moduleServer(
        id = id,
        module = function(input, output, session) {
            
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
