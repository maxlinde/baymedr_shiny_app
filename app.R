# load packages
library("shiny")
library("shinythemes")
library("reactlog")
library("vroom")
library("tidyverse")
library("kableExtra")
library("RColorBrewer")
library("ggpattern")
library("truncdist")
library("cowplot")

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
    # welcome page
    output$welcome <- renderUI(
        expr = {
            includeMarkdown(
                path = "welcome.md"
            )
        }
    )
    # module for superiority with raw data
    superRawServer(
        id = "superRaw"
    )
    # module for superiority with summary data
    superSummaryServer(
        id = "superSummary"
    )
    # module for non-inferiority with raw data
    inferRawServer(
        id = "inferRaw"
    )
    # module for non-inferiority with summary data
    inferSummaryServer(
        id = "inferSummary"
    )
    # module for equivalence with raw data
    equivRawServer(
        id = "equivRaw"
    )
    # module for equivalence with summary data
    equivSummaryServer(
        id = "equivSummary"
    )
}

# app
shinyApp(ui, server)