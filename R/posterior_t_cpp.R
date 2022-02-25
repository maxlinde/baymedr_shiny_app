posterior_t_cpp <- function(delta,
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
    tmp_1 <- Rmpfr::mpfr(eval(parse(text = "dnct(x = t,
                                                 df = nu,
                                                 ncp = sqrt(neff) * delta)")),
                         256)
    tmp_2 <- 1 / gamma * dt(x = (delta - mu_delta) / gamma,
                            df = kappa)
    numerator <- tmp_1 * tmp_2
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
    as.numeric(out)
}
