quantile_t <- function(q,
                       t,
                       n_x,
                       n_y,
                       ind_samples,
                       prior_loc,
                       prior_scale,
                       prior_df,
                       tol = 0.0000000001,
                       max_iter = 1000000000,
                       rel_tol = .Machine$double.eps^0.25) {
    # compute quantiles via Newton-Raphson method
    x_cur <- Inf
    # get reasonable starting value
    delta <- seq(from = -2,
                 to = 2,
                 length.out = 400)
    dens <- posterior_t(delta,
                        t = t,
                        n_x = n_x,
                        n_y = n_y,
                        ind_samples = ind_samples,
                        prior_loc = prior_loc,
                        prior_scale = prior_scale,
                        prior_df = prior_df)
    x_new <- delta[which.max(dens)]
    i <- 1
    while (abs(x_cur - x_new) > tol && i < max_iter) {
        x_cur <- x_new
        x_new <- x_cur -
            (cdf_t(x_cur,
                   t = t,
                   n_x = n_x,
                   n_y = n_y,
                   ind_samples = ind_samples,
                   prior_loc = prior_loc,
                   prior_scale = prior_scale,
                   prior_df = prior_df,
                   rel_tol = rel_tol) - q) /
            posterior_t(x_cur,
                        t = t,
                        n_x = n_x,
                        n_y = n_y,
                        ind_samples = ind_samples,
                        prior_loc = prior_loc,
                        prior_scale = prior_scale,
                        prior_df = prior_df)
        i <- i + 1
    }
    x_new
}
