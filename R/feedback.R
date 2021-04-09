feedback <- function(id,
                     n_x = NULL,
                     n_y = NULL,
                     sd_x = NULL,
                     sd_y = NULL,
                     ci_margin = NULL,
                     ci_level = NULL) {
    tests <- list()
    if (!is.na(n_x)) {
        tests$n_x_feed <- n_x > 1
        feedbackWarning(
            inputId = "n_x",
            show = !tests$n_x_feed,
            text = "The sample size must be higher than 1."
        )
    }
    if (!is.na(n_y)) {
        tests$n_y_feed <- n_y > 1
        feedbackWarning(
            inputId = "n_y",
            show = !tests$n_y_feed,
            text = "The sample size must be higher than 1."
        )
    }
    if (!is.na(sd_x)) {
        tests$sd_x_feed <- sd_x > 0
        feedbackWarning(
            inputId = "sd_x",
            show = !tests$sd_x_feed,
            text = "The standard deviation must be higher than 0."
        )
    }
    if (!is.na(sd_y)) {
        tests$sd_y_feed <- sd_y > 0
        feedbackWarning(
            inputId = "sd_y",
            show = !tests$sd_y_feed,
            text = "The standard deviation must be higher than 0."
        )
    }
    if (!is.na(ci_margin)) {
        tests$ci_margin_feed <- ci_margin > 0
        feedbackWarning(
            inputId = "ci_margin",
            show = !tests$ci_margin_feed,
            text = "The margin of the confidence interval must be higher than 0."
        )
    }
    if (!is.na(ci_level)) {
        tests$ci_level_feed <- ci_level > 0 & ci_level < 1
        feedbackWarning(
            inputId = "ci_level",
            show = !tests$ci_level_feed,
            text = "The confidence level must be between 0 and 1."
        )
    }
    all(unlist(tests))
}
