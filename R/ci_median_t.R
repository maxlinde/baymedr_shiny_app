ci_median_t <- function(t,
                        n1,
                        n2,
                        ind_samples,
                        prior_loc,
                        prior_scale,
                        prior_df,
                        ci = 0.95,
                        type = "two-sided",
                        tol = 0.0000000001,
                        max_iter = 1000000000,
                        rel_tol = .Machine$double.eps^0.25) {
    lower <- (1 - ci) / 2
    upper <- ci + (1 - ci) / 2
    med <- 0.5
    post_area_smaller_0 <- cdf_t(x = 0,
                                 t = t,
                                 n1 = n1,
                                 n2 = n2,
                                 ind_samples = ind_samples,
                                 prior_loc = prior_loc,
                                 prior_scale = prior_scale,
                                 prior_df = prior_df,
                                 rel_tol = rel_tol)
    if (type == "plus-sided") {
        lower <- post_area_smaller_0 + (1 - post_area_smaller_0) * lower
        upper <- post_area_smaller_0 + (1 - post_area_smaller_0) * upper
        med <- post_area_smaller_0 + (1 - post_area_smaller_0) * med
    } else if (type == "min-sided") {
        lower <- post_area_smaller_0 * lower
        upper <- post_area_smaller_0 * upper
        med <- post_area_smaller_0 * med
    }
    ci_lower <- quantile_t(lower,
                           t = t,
                           n1 = n1,
                           n2 = n2,
                           ind_samples = ind_samples,
                           prior_loc = prior_loc,
                           prior_scale = prior_scale,
                           prior_df = prior_df,
                           rel_tol = rel_tol)
    ci_upper <- quantile_t(upper,
                           t = t,
                           n1 = n1,
                           n2 = n2,
                           ind_samples = ind_samples,
                           prior_loc = prior_loc,
                           prior_scale = prior_scale,
                           prior_df = prior_df,
                           rel_tol = rel_tol)
    median <- quantile_t(med,
                         t = t,
                         n1 = n1,
                         n2 = n2,
                         ind_samples = ind_samples,
                         prior_loc = prior_loc,
                         prior_scale = prior_scale,
                         prior_df = prior_df,
                         rel_tol = rel_tol)
    return(list(ci_lower = ci_lower,
                median = median,
                ci_upper = ci_upper))
}
