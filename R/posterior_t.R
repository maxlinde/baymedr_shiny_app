posterior_t <- function(delta,
                        t,
                        n1,
                        n2,
                        ind_samples,
                        prior_loc,
                        prior_scale,
                        prior_df,
                        rel_tol = .Machine$double.eps^0.25) {
    neff <- ifelse(test = ind_samples,
                   yes = n1 * n2 / (n1 + n2),
                   no = n1)
    nu <- ifelse(test = ind_samples,
                 yes = n1 + n2 - 2,
                 no = n1 - 1)
    mu_delta <- prior_loc
    gamma <- prior_scale
    kappa <- prior_df
    numerator <- suppressWarnings(
        dt(x = t,
           df = nu,
           ncp = sqrt(neff) * delta) * 1 / gamma * dt(
               (delta - mu_delta) / gamma,
               df = kappa))
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
