general_ui <- function(id) {
    name <- str_split(
        string = id,
        pattern = "-"
    ) %>%
        unlist() %>%
        last()
    switch(
        name,
        # prior_scale
        prior_scale = textInput(
            inputId = id,
            label = "Scale parameter of the Cauchy prior",
            value = "1 / sqrt(2)",
            placeholder = "e.g., '1' or '1 / sqrt(2)'"
        ),
        submit = actionButton(
            inputId = id,
            label = "Calculate Bayes factor"
        )
    )
}

equiv_ui <- function(id) {
    name <- str_split(
        string = id,
        pattern = "-"
    ) %>%
        unlist() %>%
        last()
    switch(
        name,
        # interval_low
        interval_low = numericInput(
            inputId = id,
            label = "Lower boundary of the equivalece interval",
            value = NULL
        ),
        # interval_high
        interval_high = numericInput(
            inputId = id,
            label = "Upper boundary of the equivalece interval",
            value = NULL
        ),
        # interval_std
        interval_std = radioButtons(
            inputId = id,
            label = "Unit for the equivalence interval",
            choices = setNames(
                object = c("TRUE", "FALSE"),
                nm = c("Standardised", "Unstandardised")
            ),
            selected = "TRUE"
        )
    )
}