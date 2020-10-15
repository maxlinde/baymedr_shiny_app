# module user interface
superRawUi <- function(id) {
    
}

# module server
superRawServer <- function(id) {
    moduleServer(
        id = id,
        module = function(input, output, session) {
            
        }
    )
}

# module app
superRawApp <- function() {
    ui <- fluidPage(
        superRawUi(
            id = "superRaw"
        )
    )
    server <- function(input, output, session) {
        superRawServer(
            id = "superRaw"
        )
    }
    shinyApp(ui, server)
}
