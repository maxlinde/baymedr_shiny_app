super_bf <- function(x = NULL,
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
                     prior_scale,
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
    if (choose_sd_ci == "sd") {
        sd_pooled <- sqrt(((n_x - 1) * sd_x ^ 2 + (n_y - 1) * sd_y ^ 2) /
                              (n_x + n_y - 2))
        se <- sd_pooled * sqrt(1 / n_x + 1 / n_y)
    }
    if (choose_sd_ci == "ci") {
        perc <- 1 - ((1 - ci_level) / 2)
        se <- ci_margin / qt(p = perc,
                             df = n_x + n_y - 2)
        sd_pooled <- se / sqrt(1 / n_x + 1 / n_y)
    }
    t_stat <- (mean_y - mean_x) / se
    res <- bf10_t(t = t_stat,
                  n_x = n_x,
                  n_y = n_y,
                  ind_samples = TRUE,
                  prior_loc = 0,
                  prior_scale = prior_scale,
                  prior_df = 1)
    if (str_detect(direction,
                   "low")) {
        bf <- res$bf_min0
        h1 <- "mu_y < mu_x"
    } else {
        bf <- res$bf_plus0
        h1 <- "mu_y > mu_x"
    }
    test <- "Superiority analysis"
    h0 <- "mu_y == mu_x"
    hypotheses <- list(h0 = h0,
                       h1 = h1)
    list(n_x = n_x,
         n_y = n_y,
         t = t_stat,
         test = test,
         hypotheses = hypotheses,
         prior_scale = prior_scale,
         bf = bf,
         direction = direction)
}