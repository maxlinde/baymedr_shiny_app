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
library("rlang")
library("scales")
library("rmarkdown")
library("shinyhelper")
library("shinyjs")
library("Rcpp")

sapply(paste0("src/", list.files(path = "src/")), sourceCpp)

# user interface
ui <- fluidPage(
    # theme
    theme = shinytheme(
        theme = "cosmo"
    ),
    useShinyjs(),
    # navigation bar
    navbarPage(
        # title
        title = paste0("baymedr | v", packageVersion(pkg = "baymedr")),
        # welcome tab
        tabPanel(
            title = "Welcome",
            uiOutput("welcome")
        ),
        # superiority menu
        navbarMenu(
            title = "Superiority",
            # raw data
            tabPanel(
                title = "Raw data",
                superRawUi("superRaw")
            ),
            # summary data
            tabPanel(
                title = "Summary data",
                superSummaryUi("superSummary")
            )
        ),
        # non-inferiority menu
        navbarMenu(
            title = "Non-inferiority",
            # raw data
            tabPanel(
                title = "Raw data",
                inferRawUi("inferRaw")
            ),
            # summary data
            tabPanel(
                title = "Summary data",
                inferSummaryUi("inferSummary")
            )
        ),
        # equivalence menu
        navbarMenu(
            title = "Equivalence",
            # raw data
            tabPanel(
                title = "Raw data",
                equivRawUi("equivRaw")
            ),
            # summary data
            tabPanel(
                title = "Summary data",
                equivSummaryUi("equivSummary")
            )
        )
    )
)

# server
server <- function(input, output, session) {
    observe_helpers(
        withMathJax = TRUE
    )
    # welcome page
    output$welcome <- renderUI(
        expr = {
            withMathJax(
                includeHTML(
                    path = render(
                        input = "welcome.Rmd"
                    )
                )
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