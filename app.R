# load packages
library("shiny")
library("shinythemes")

# user interface
ui <- fluidPage(
    theme = shinytheme(
        theme = "cosmo"
    ),
    navbarPage(
        title = paste0("baymedr | v", packageVersion(pkg = "baymedr")),
        tabPanel(
            title = "Welcome",
            uiOutput("welcome")
        ),
        navbarMenu(
            title = "Superiority",
            tabPanel(
                title = "Raw data"
            ),
            tabPanel(
                title = "Summary data"
            )
        ),
        navbarMenu(
            title = "Non-inferiority",
            tabPanel(
                title = "Raw data"
            ),
            tabPanel(
                title = "Summary data"
            )
        ),
        navbarMenu(
            title = "Equivalence",
            tabPanel(
                title = "Raw data"
            ),
            tabPanel(
                title = "Summary data"
            )
        )
    )
)

# server
server <- function(input, output, session) {
    output$welcome <- renderUI(
        expr = {
            includeMarkdown(
                path = "welcome.md"
            )
        }
    )
}

# app
shinyApp(ui, server)