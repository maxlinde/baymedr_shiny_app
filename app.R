# load packages
library("shiny")
library("shinythemes")
library("reactlog")

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
                title = "Raw data",
                superRawUi("superRaw")
            ),
            tabPanel(
                title = "Summary data",
                superSummaryUi("superSummary")
            )
        ),
        navbarMenu(
            title = "Non-inferiority",
            tabPanel(
                title = "Raw data",
                inferRawUi("inferRaw")
            ),
            tabPanel(
                title = "Summary data",
                inferSummaryUi("inferSummary")
            )
        ),
        navbarMenu(
            title = "Equivalence",
            tabPanel(
                title = "Raw data",
                equivRawUi("equivRaw")
            ),
            tabPanel(
                title = "Summary data",
                equivSummaryUi("equivSummary")
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
    superRawServer(
        id = "superRaw"
    )
    superSummaryServer(
        id = "superSummary"
    )
    inferRawServer(
        id = "inferRaw"
    )
    inferSummaryServer(
        id = "inferSummary"
    )
    equivRawServer(
        id = "equivRaw"
    )
    equivSummaryServer(
        id = "equivSummary"
    )
}

# app
shinyApp(ui, server)