posterior_t <- function(delta,
                        t,
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
    numerator <- suppressWarnings(
        dt(x = t,
           df = nu,
           ncp = sqrt(neff) * delta) * 1 / gamma * 
            dt(x = (delta - mu_delta) / gamma,
               df = kappa)
    )
    denominator <- integrate(integrand_t,
                             lower = -Inf,
                             upper = Inf,
                             t = t,
                             n = neff,
                             nu = nu,
                             mu_delta = mu_delta,
                             gamma = gamma,
                             kappa = kappa,
                             rel.tol = rel_tol)$value
    out <- numerator / denominator
    out[is.na(out)] <- 0
    out
}
