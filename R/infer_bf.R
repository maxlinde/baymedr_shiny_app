infer_bf <- function(x = NULL,
                     y = NULL,
                     n_x = NULL,
                     n_y = NULL,
                     mean_x = NULL,
                     mean_y = NULL,
                     sd_x = NULL,
                     sd_y = NULL,
                     ci_margin = NULL,
                     ci_level = NULL,
                     ni_margin,
                     ni_margin_std,
                     prior_scale,
                     direction) {
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
    if (str_detect(direction,
                   "low")) {
        if (isFALSE(ni_margin_std)) {
            ni_margin_std <- ni_margin / sd_pooled
            ni_margin_unstd <- ni_margin
        } else {
            ni_margin_std <- ni_margin
            ni_margin_unstd <- ni_margin * sd_pooled
        }
        t_stat <- (mean_y - mean_x - ni_margin_unstd) / se
        res <- bf10_t(t = t_stat,
                      n_x = n_x,
                      n_y = n_y,
                      ind_samples = TRUE,
                      prior_loc = ni_margin_std,
                      prior_scale = prior_scale,
                      prior_df = 1)
        bf <- res[[3]] / res[[2]]
        h0 <- "mu_y - mu_x > ni_margin"
        h1 <- "mu_y - mu_x < ni_margin"
    } else {
        if (isFALSE(ni_margin_std)) {
            ni_margin_std <- -ni_margin / sd_pooled
            ni_margin_unstd <- -ni_margin
        } else {
            ni_margin_std <- -ni_margin
            ni_margin_unstd <- -ni_margin * sd_pooled
        }
        t_stat <- (mean_y - mean_x + ni_margin_unstd) / se
        res <- bf10_t(t = t_stat,
                      n_x = n_x,
                      n_y = n_y,
                      ind_samples = TRUE,
                      prior_loc = ni_margin_std,
                      prior_scale = prior_scale,
                      prior_df = 1)
        bf <- res[[2]] / res[[3]]
        h0 <- "mu_y - mu_x < -ni_margin"
        h1 <- "mu_y - mu_x > -ni_margin"
    }
    test <- "Non-inferiority analysis"
    hypotheses <- list(h0 = h0,
                       h1 = h1)
    ni_margin <- list(ni_margin_std = ni_margin_std,
                      ni_margin_unstd = ni_margin_unstd)
    bf_all <- res
    list(n_x = n_x,
         n_y = n_y,
         t = t_stat,
         test = test,
         hypotheses = hypotheses,
         ni_margin = ni_margin,
         prior_scale = prior_scale,
         bf = bf,
         bf_all = bf_all,
         direction = direction)
}