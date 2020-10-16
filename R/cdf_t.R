cdf_t <- function(x,
                  t,
                  n_x,
                  n_y,
                  ind_samples,
                  prior_loc,
                  prior_scale,
                  prior_df,
                  rel_tol = .Machine$double.eps^0.25) {
    out <- integrate(posterior_t,
                     lower = -Inf,
                     upper = x,
                     t = t,
                     n_x = n_x,
                     n_y = n_y,
                     ind_samples = ind_samples,
                     prior_loc = prior_loc,
                     prior_scale = prior_scale,
                     prior_df = prior_df,
                     rel.tol = rel_tol)$value
    if (out > 1) {
        out <- 1
        warn(str_c(
            "Numerical integration yields a CDF value slightly larger than 1. ",
            "The CDF value has been replaced by 1."
        ))
    }
    out
}
