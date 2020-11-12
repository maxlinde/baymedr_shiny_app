super_freq <- function(x = NULL,
                       y = NULL,
                       n_x = NULL,
                       n_y = NULL,
                       mean_x = NULL,
                       mean_y = NULL,
                       sd_x = NULL,
                       sd_y = NULL,
                       ci_margin = NULL,
                       ci_level = NULL,
                       direction) {
    if (!is.null(x) && !is.null(y)) {
        n_x <- length(x)
        n_y <- length(y)
        mean_x <- mean(x)
        mean_y <- mean(y)
        sd_x <- sd(x)
        sd_y <- sd(y)
    }
        df <- n_x + n_y - 2
    if (!is.null(sd_x) && !is.null(sd_y)) {
        sd_pooled <- sqrt(((n_x - 1) * sd_x ^ 2 + (n_y - 1) * sd_y ^ 2) /
                              df)
        se <- sd_pooled * sqrt(1 / n_x + 1 / n_y)
    } else {
        perc <- 1 - ((1 - ci_level) / 2)
        se <- ci_margin / qt(p = perc,
                             df = df)
    }
    t_stat <- (mean_y - mean_x) / se
    if (direction == "low") {
        p_value <- pt(q = t_stat,
                      df = df,
                      lower.tail = TRUE)
    } else {
        p_value <- pt(q = t_stat,
                      df = df,
                      lower.tail = FALSE)
    }
    list(t_stat,
         df,
         p_value)
}
