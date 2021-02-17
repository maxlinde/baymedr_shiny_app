equiv_bf <- function(x = NULL,
                     y = NULL,
                     n_x = NULL,
                     n_y = NULL,
                     mean_x = NULL,
                     mean_y = NULL,
                     sd_x = NULL,
                     sd_y = NULL,
                     ci_margin = NULL,
                     ci_level = NULL,
                     interval_low,
                     interval_high,
                     interval_std,
                     prior_scale) {
    if (!is.null(x) && !is.null(y)) {
        n_x <- length(x)
        n_y <- length(y)
        mean_x <- mean(x)
        mean_y <- mean(y)
        sd_x <- sd(x)
        sd_y <- sd(y)
    }
    if (!is.null(sd_x) && !is.null(sd_y)) {
        sd_pooled <- sqrt(((n_x - 1) * sd_x ^ 2 + (n_y - 1) * sd_y ^ 2) /
                              (n_x + n_y - 2))
        se <- sd_pooled * sqrt(1 / n_x + 1 / n_y)
    } else {
        perc <- 1 - ((1 - ci_level) / 2)
        se <- ci_margin / qt(p = perc,
                             df = n_x + n_y - 2)
        sd_pooled <- se / sqrt(1 / n_x + 1 / n_y)
    }
    t_stat <- (mean_y - mean_x) / se
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
        res <- bf10_t(t = t_stat,
                      n_x = n_x,
                      n_y = n_y,
                      ind_samples = TRUE,
                      prior_loc = 0,
                      prior_scale = prior_scale,
                      prior_df = 1)
        bf <- 1 / res$bf_10
        h0 <- "mu_y == mu_x"
        h1 <- "mu_y != mu_x"
    } else {
        cdf_t_upper <- cdf_t(x = interval_high_std,
                             t = t_stat,
                             n_x = n_x,
                             n_y = n_y,
                             ind_samples = TRUE,
                             prior_loc = 0,
                             prior_scale = prior_scale,
                             prior_df = 1)
        cdf_t_lower <- cdf_t(x = interval_low_std,
                             t = t_stat,
                             n_x = n_x,
                             n_y = n_y,
                             ind_samples = TRUE,
                             prior_loc = 0,
                             prior_scale = prior_scale,
                             prior_df = 1)
        post_dens <- cdf_t_upper - cdf_t_lower
        if (post_dens < 0) {
            post_dens <- 0
        }
        prior_dens <- pcauchy(q = interval_high_std,
                              scale = prior_scale) - pcauchy(q = interval_low_std,
                                                             scale = prior_scale)
        bf <- (post_dens / prior_dens) /
            ((1 - post_dens) / (1 - prior_dens))
        h0 <- "mu_y - mu_x > c_low AND mu_y - mu_x < c_high"
        h1 <- "mu_y - mu_x < c_low OR mu_y - mu_x > c_high"
    }
    test <- "Equivalence analysis"
    hypotheses <- list(h0 = h0,
                       h1 = h1)
    interval <- list(interval_low_std = interval_low_std,
                     interval_high_std = interval_high_std,
                     interval_low_unstd = interval_low_unstd,
                     interval_high_unstd = interval_high_unstd)
    list(n_x = n_x,
         n_y = n_y,
         t = t_stat,
         test = test,
         hypotheses = hypotheses,
         interval = interval,
         prior_scale = prior_scale,
         bf = bf)
}
