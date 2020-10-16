bf10_t <- function(t,
                   n_x,
                   n_y,
                   ind_samples,
                   prior_loc,
                   prior_scale,
                   prior_df,
                   rel_tol = .Machine$double.eps^0.25) {
    neff <- ifelse(test = ind_samples,
                   yes = n_x * n_y / (n_x + n_y),
                   no = n_x)
    nu <- ifelse(test = ind_samples,
                 yes = n_x + n_y - 2,
                 no = n_x - 1)
    mu_delta <- prior_loc
    gamma <- prior_scale
    kappa <- prior_df
    numerator <- integrate(integrand_t,
                           lower = -Inf,
                           upper = Inf,
                           t = t,
                           n = neff,
                           nu = nu,
                           mu_delta = mu_delta,
                           gamma = gamma,
                           kappa = kappa,
                           rel.tol = rel_tol)$value
    denominator <- dt(x = t,
                      df = nu)
    bf10 <- numerator / denominator
    prior_area_smaller_0 <- pt(q = -mu_delta / gamma,
                               df = kappa)
    post_area_smaller_0 <- cdf_t(x = 0,
                                 t = t,
                                 n_x = n_x,
                                 n_y = n_y,
                                 ind_samples = ind_samples,
                                 prior_loc = prior_loc,
                                 prior_scale = prior_scale,
                                 prior_df = prior_df,
                                 rel_tol = rel_tol)
    bf_min_x <- post_area_smaller_0 / prior_area_smaller_0
    bf_plus1 <- (1 - post_area_smaller_0) / (1 - prior_area_smaller_0)
    bf_min0 <- bf_min_x * bf10
    bf_plus0 <- bf_plus1 * bf10
    return(list(bf_10 = bf10,
                bf_plus0 = bf_plus0,
                bf_min0 = bf_min0))
}
