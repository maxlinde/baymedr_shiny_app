# general ui inputs
general_ui <- function(id) {
    # extract last part of id
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
            value = "1 / sqrt(2)"
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
# equivalence ui inputs
equiv_ui <- function(id) {
    # extract last part of id
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
            label = "Lower boundary of the equivalence interval",
            value = NULL
        ) %>%
            helper(
                icon = "question",
                colour = "red",
                type = "markdown",
                content = "interval"
            ),
        # interval_high
        interval_high = numericInput(
            inputId = id,
            label = "Upper boundary of the equivalence interval",
            value = NULL
        ) %>%
            helper(
                icon = "question",
                colour = "red",
                type = "markdown",
                content = "interval"
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

# non-inferiority ui inputs
infer_ui <- function(id) {
    # extract last part of id
    name <- str_split(
        string = id,
        pattern = "-"
    ) %>%
        unlist() %>%
        last()
    switch(
        name,
        # ni_margin
        ni_margin = numericInput(
            inputId = id,
            label = "Non-inferiority margin",
            value = NULL
        ) %>%
            helper(
                icon = "question",
                colour = "red",
                type = "markdown",
                content = "ni_margin"
            ),
        # ni_margin_std
        ni_margin_std = radioButtons(
            inputId = id,
            label = "Unit for the non-inferiority margin",
            choices = setNames(
                object = c("TRUE", "FALSE"),
                nm = c("Standardised", "Unstandardised")
            ),
            selected = "TRUE"
        ),
        # direction
        direction = radioButtons(
            inputId = id,
            label = "Do high or low values represent 'non-inferiority'?",
            choices = setNames(
                object = c("low", "high"),
                nm = c("Low", "High")
            ),
            selected = "high"
        )
    )
}

# superiority ui inputs
super_ui <- function(id) {
    # extract last part of id
    name <- str_split(
        string = id,
        pattern = "-"
    ) %>%
        unlist() %>%
        last()
    switch(
        name,
        # direction
        direction = radioButtons(
            inputId = id,
            label = "Do high or low values represent 'superiority'?",
            choices = setNames(
                object = c("low", "high"),
                nm = c("Low", "High")
            ),
            selected = "high"
        ),
        # alternative
        alternative = radioButtons(
            inputId = id,
            label = "Do you want a one-sided or two-sided test?",
            choices = setNames(
                object = c("one.sided", "two.sided"),
                nm = c("One-sided", "Two-sided")
            ),
            selected = "one.sided"
        )
    )
}

# results ui outputs
results_ui <- function(id) {
    # extract last part of id
    name = str_split(
        string = id,
        pattern = "-"
    ) %>%
        unlist() %>%
        last()
    switch(
        name,
        # results_show
        results_show = verbatimTextOutput(
            outputId = id
        ),
        # results_plot
        results_plot = plotOutput(
            outputId = id
        ),
        # results_form
        results_form = uiOutput(
            outputId = id
        )
    )
}