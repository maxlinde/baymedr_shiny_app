infer_freq <- function(x = NULL,
                       y = NULL,
                       n_x = NULL,
                       n_y = NULL,
                       mean_x = NULL,
                       mean_y = NULL,
                       sd_x = NULL,
                       sd_y = NULL,
                       ci_margin = NULL,
                       ci_level = NULL,
                       choose_sd_ci = NULL,
                       ni_margin,
                       ni_margin_std,
                       direction) {
    if (!is.null(x) && !is.null(y)) {
        n_x <- length(x)
        n_y <- length(y)
        mean_x <- mean(x)
        mean_y <- mean(y)
        sd_x <- sd(x)
        sd_y <- sd(y)
        choose_sd_ci <- "sd"
    }
    df <- n_x + n_y - 2
    if (choose_sd_ci == "sd") {
        sd_pooled <- sqrt(((n_x - 1) * sd_x ^ 2 + (n_y - 1) * sd_y ^ 2) / df)
        se <- sd_pooled * sqrt(1 / n_x + 1 / n_y)
    }
    if (choose_sd_ci == "ci") {
        perc <- 1 - ((1 - ci_level) / 2)
        se <- ci_margin / qt(p = perc,
                             df = df)
        sd_pooled <- se / sqrt(1 / n_x + 1 / n_y)
    }
    if (str_detect(direction,
                   "low")) {
        if (ni_margin_std == "unstd") {
            ni_margin_std <- ni_margin / sd_pooled
            ni_margin_unstd <- ni_margin
        } else {
            ni_margin_std <- ni_margin
            ni_margin_unstd <- ni_margin * sd_pooled
        }
        t_stat <- (mean_y - mean_x - ni_margin_unstd) / se
        p_value <- pt(q = t_stat,
                      df = df,
                      lower.tail = TRUE)
    } else {
        if (ni_margin_std == "unstd") {
            ni_margin_std <- -ni_margin / sd_pooled
            ni_margin_unstd <- -ni_margin
        } else {
            ni_margin_std <- -ni_margin
            ni_margin_unstd <- -ni_margin * sd_pooled
        }
        t_stat <- (mean_y - mean_x + ni_margin_unstd) / se
        p_value <- pt(q = t_stat,
                      df = df,
                      lower.tail = FALSE)
    }
    list(t_stat,
         df,
         p_value)
}
