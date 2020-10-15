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
        # submit
        submit = actionButton(
            inputId = id,
            label = "Calculate Bayes factor"
        ),
        # n_x
        n_x = numericInput(
            inputId = id,
            label = "Sample size of the control condition",
            value = NULL,
            min = 0
        ),
        # n_y
        n_y = numericInput(
            inputId = id,
            label = "Sample size of the experimental condition",
            value = NULL,
            min = 0
        ),
        # mean_x
        mean_x = numericInput(
            inputId = id,
            label = "Sample mean of the control condition",
            value = NULL
        ),
        # mean_y
        mean_y = numericInput(
            inputId = id,
            label = "Sample mean of the experimental condition",
            value = NULL
        ),
        # sd_x
        sd_x = numericInput(
            inputId = id,
            label = "Sample standard deviation of the control condition",
            value = NULL,
            min = 0
        ),
        # sd_y
        sd_y = numericInput(
            inputId = id,
            label = "Sample standard deviation of the experimental condition",
            value = NULL,
            min = 0
        ),
        # ci_margin
        ci_margin = numericInput(
            inputId = id,
            label = "Margin of the confidence interval for the mean difference",
            value = NULL,
            min = 0
        ),
        # ci_level
        ci_level = numericInput(
            inputId = id,
            label = "Level of the confidence interval for the mean difference",
            value = NULL,
            min = 0,
            max = 1
        ),
        # choose_sd_ci
        choose_sd_ci = radioButtons(
            inputId = id,
            label = "Which do you want to use?",
            choices = setNames(
                object = c("sd", "ci"),
                nm = c("Standard deviations", "Confidence interval")
            ),
            selected = "sd"
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