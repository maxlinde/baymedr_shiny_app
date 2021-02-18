equiv_freq <- function(x = NULL,
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
                       interval_low,
                       interval_high,
                       interval_std) {
    if (!is.null(x) && !is.null(y)) {
        n_x <- length(x)
        n_y <- length(y)
        mean_x <- mean(x)
        mean_y <- mean(y)
        sd_x <- sd(x)
        sd_y <- sd(y)
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
    if (interval_std == "unstd") {
        interval_low_std <- interval_low / sd_pooled
        interval_high_std <- interval_high / sd_pooled
        interval_low_unstd <- interval_low
        interval_high_unstd <- interval_high
    } else {
        interval_low_std <- interval_low
        interval_high_std <- interval_high
        interval_low_unstd <- interval_low * sd_pooled
        interval_high_unstd <- interval_high * sd_pooled
    }
    if (interval_low_std == 0 & interval_high_std == 0) {
        t_stat <- NULL
        df <- NULL
        p_value <- NULL
        point_null <- TRUE
    } else {
        t_stat <- p_value <- list()
        t_stat$lower <- (mean_y - mean_x + interval_low_unstd) / se
        p_value$lower <- pt(q = t_stat$lower,
                            df = df,
                            lower.tail = TRUE)
        t_stat$upper <- (mean_y - mean_x + interval_high_unstd) / se
        p_value$upper <- pt(q = t_stat$upper,
                            df = df,
                            lower.tail = FALSE)
        point_null <- FALSE
    }
    list(t_stat,
         df,
         p_value,
         point_null)
}
