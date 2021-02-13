super_plot <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       prior_scale = prior_scale,
                       bf = bf,
                       bf_all = bf_all,
                       direction = direction) {
    colours <- brewer.pal(
        n = 4,
        name = "Set1"
    )
    hdi80_prior_low <- qcauchy(
        p = 0.1,
        location = 0,
        scale = prior_scale
    )
    hdi80_prior_high <- qcauchy(
        p = 0.9,
        location = 0,
        scale = prior_scale
    )
    hdi95_posterior_low <- unlist(
        ci_median_t(
            t = t,
            n_x = n_x,
            n_y = n_y,
            ind_samples = TRUE,
            prior_loc = 0,
            prior_scale = prior_scale,
            prior_df = 1
        )$ci_lower
    )
    hdi95_posterior_high <- unlist(
        ci_median_t(
            t = t,
            n_x = n_x,
            n_y = n_y,
            ind_samples = TRUE,
            prior_loc = 0,
            prior_scale = prior_scale,
            prior_df = 1
        )$ci_upper
    )
    if (direction == "low") {
        max_dens_prior <- dcauchy(
            x = 0,
            location = 0,
            scale = prior_scale
        ) / pcauchy(
            q = 0,
            location = 0,
            scale = prior_scale
        )
        if (hdi95_posterior_low > 0) {
            max_dens_posterior <- posterior_t(
                delta = 0,
                t = t,
                n_x = n_x,
                n_y = n_y,
                ind_samples = TRUE,
                prior_loc = 0,
                prior_scale = prior_scale,
                prior_df = 1
            ) / cdf_t(
                x = 0,
                t = t,
                n_x = n_x,
                n_y = n_y,
                ind_samples = TRUE,
                prior_loc = 0,
                prior_scale = prior_scale,
                prior_df = 1
            )
        } else {
            max_dens_posterior <- optimize(
                f = posterior_t,
                interval = c(hdi95_posterior_low, 0),
                t = t,
                n_x = n_x,
                n_y = n_y,
                ind_samples = TRUE,
                prior_loc = 0,
                prior_scale = prior_scale,
                prior_df = 1,
                maximum = TRUE,
                tol = 0.00000000001
            )$objective / cdf_t(
                x = 0,
                t = t,
                n_x = n_x,
                n_y = n_y,
                ind_samples = TRUE,
                prior_loc = 0,
                prior_scale = prior_scale,
                prior_df = 1
            )
        }
        x_lim <- c(min(hdi80_prior_low, hdi95_posterior_low),
                   max(hdi80_prior_high, hdi95_posterior_high))
        y_lim <- c(0,
                   max(max_dens_prior, max_dens_posterior) * 1.4)
        dens_prior0 <- max_dens_prior
        dens_posterior0 <- posterior_t(
            delta = 0,
            t = t,
            n_x = n_x,
            n_y = n_y,
            ind_samples = TRUE,
            prior_loc = 0,
            prior_scale = prior_scale,
            prior_df = 1
        ) / cdf_t(
            x = 0,
            t = t,
            n_x = n_x,
            n_y = n_y,
            ind_samples = TRUE,
            prior_loc = 0,
            prior_scale = prior_scale,
            prior_df = 1
        )
        ggplot(
            data = data.frame(x = 0),
            mapping = aes(
                x = x
            )
        ) +
            stat_function(
                mapping = aes(
                    linetype = "prior"
                ),
                fun = dtrunc,
                n = 10001,
                xlim = c(x_lim[1], 0),
                size = 2,
                args = list(spec = "cauchy",
                            a = -Inf,
                            b = 0,
                            location = 0,
                            scale = prior_scale)
            ) +
            stat_function(
                mapping = aes(
                    linetype = "posterior"
                ),
                fun = function(x) {
                    posterior_t(delta = x,
                                t = t,
                                n_x = n_x,
                                n_y = n_y,
                                ind_samples = TRUE,
                                prior_loc = 0,
                                prior_scale = prior_scale,
                                prior_df = 1) / cdf_t(
                                    x = 0,
                                    t = t,
                                    n_x = n_x,
                                    n_y = n_y,
                                    ind_samples = TRUE,
                                    prior_loc = 0,
                                    prior_scale = prior_scale,
                                    prior_df = 1
                                )
                },
                n = 10001,
                xlim = c(x_lim[1], 0),
                size = 2
            ) +
            geom_line(
                data = data.frame(
                    x = c(0, 0),
                    y = c(0, dens_prior0)
                ),
                mapping = aes(
                    x = x,
                    y = y,
                    linetype = "prior"
                ),
                size = 2
            ) +
            geom_line(
                data = data.frame(
                    x = c(0, 0),
                    y = c(0, dens_posterior0)
                ),
                mapping = aes(
                    x = x,
                    y = y,
                    linetype = "posterior"
                ),
                size = 2
            ) +
            geom_point(
                data = data.frame(
                    x = c(0, 0),
                    y = c(dens_prior0, dens_posterior0)
                ),
                mapping = aes(
                    x = x,
                    y = y,
                    colour = colours[c(1, 3)]
                ),
                size = 5,
                show.legend = FALSE
            ) +
            scale_x_continuous(
                name = "Population effect size",
                limits = x_lim,
                expand = c(0, 0)
            ) +
            scale_y_continuous(
                name = "Density",
                limits = y_lim
            ) +
            scale_linetype_manual(
                name = NULL,
                values = c("posterior" = "solid",
                           "prior" = "dotted"),
                labels = c("posterior" = "Posterior",
                           "prior" = "Prior")
            ) +
            scale_colour_identity() +
            theme_bw() +
            theme(
                text = element_text(
                    size = 16
                ),
                panel.grid = element_blank(),
                legend.position = c(0.5, 0.9),
                legend.key.width = unit(2.5, "cm")
            )
    } else {
        max_dens_prior <- dcauchy(
            x = 0,
            location = 0,
            scale = prior_scale
        ) / pcauchy(
            q = 0,
            location = 0,
            scale = prior_scale
        )
        if (hdi95_posterior_high < 0) {
            max_dens_posterior <- posterior_t(
                delta = 0,
                t = t,
                n_x = n_x,
                n_y = n_y,
                ind_samples = TRUE,
                prior_loc = 0,
                prior_scale = prior_scale,
                prior_df = 1
            ) / (1 - cdf_t(
                x = 0,
                t = t,
                n_x = n_x,
                n_y = n_y,
                ind_samples = TRUE,
                prior_loc = 0,
                prior_scale = prior_scale,
                prior_df = 1
            ))
        } else {
            max_dens_posterior <- optimize(
                f = posterior_t,
                interval = c(0, hdi95_posterior_high),
                t = t,
                n_x = n_x,
                n_y = n_y,
                ind_samples = TRUE,
                prior_loc = 0,
                prior_scale = prior_scale,
                prior_df = 1,
                maximum = TRUE,
                tol = 0.00000000001
            )$objective / (1 - cdf_t(
                x = 0,
                t = t,
                n_x = n_x,
                n_y = n_y,
                ind_samples = TRUE,
                prior_loc = 0,
                prior_scale = prior_scale,
                prior_df = 1
            ))
        }
        x_lim <- c(min(hdi80_prior_low, hdi95_posterior_low),
                   max(hdi80_prior_high, hdi95_posterior_high))
        y_lim <- c(0,
                   max(max_dens_prior, max_dens_posterior) * 1.4)
        dens_prior0 <- max_dens_prior
        dens_posterior0 <- posterior_t(
            delta = 0,
            t = t,
            n_x = n_x,
            n_y = n_y,
            ind_samples = TRUE,
            prior_loc = 0,
            prior_scale = prior_scale,
            prior_df = 1
        ) / (1 - cdf_t(
            x = 0,
            t = t,
            n_x = n_x,
            n_y = n_y,
            ind_samples = TRUE,
            prior_loc = 0,
            prior_scale = prior_scale,
            prior_df = 1
        ))
        ggplot(
            data = data.frame(x = 0),
            mapping = aes(
                x = x
            )
        ) +
            stat_function(
                mapping = aes(
                    linetype = "prior"
                ),
                fun = dtrunc,
                n = 10001,
                xlim = c(0, x_lim[2]),
                size = 2,
                args = list(spec = "cauchy",
                            a = 0,
                            b = Inf,
                            location = 0,
                            scale = prior_scale)
            ) +
            stat_function(
                mapping = aes(
                    linetype = "posterior"
                ),
                fun = function(x) {
                    posterior_t(delta = x,
                                t = t,
                                n_x = n_x,
                                n_y = n_y,
                                ind_samples = TRUE,
                                prior_loc = 0,
                                prior_scale = prior_scale,
                                prior_df = 1) / (1 - cdf_t(
                                    x = 0,
                                    t = t,
                                    n_x = n_x,
                                    n_y = n_y,
                                    ind_samples = TRUE,
                                    prior_loc = 0,
                                    prior_scale = prior_scale,
                                    prior_df = 1
                                ))
                },
                n = 10001,
                xlim = c(0, x_lim[2]),
                size = 2
            ) +
            geom_line(
                data = data.frame(
                    x = c(0, 0),
                    y = c(0, dens_prior0)
                ),
                mapping = aes(
                    x = x,
                    y = y,
                    linetype = "prior"
                ),
                size = 2
            ) +
            geom_line(
                data = data.frame(
                    x = c(0, 0),
                    y = c(0, dens_posterior0)
                ),
                mapping = aes(
                    x = x,
                    y = y,
                    linetype = "posterior"
                ),
                size = 2
            ) +
            geom_point(
                data = data.frame(
                    x = c(0, 0),
                    y = c(dens_prior0, dens_posterior0)
                ),
                mapping = aes(
                    x = x,
                    y = y,
                    colour = colours[c(1, 3)]
                ),
                size = 5,
                show.legend = FALSE
            ) +
            scale_x_continuous(
                name = "Population effect size",
                limits = x_lim,
                expand = c(0, 0)
            ) +
            scale_y_continuous(
                name = "Density",
                limits = y_lim
            ) +
            scale_linetype_manual(
                name = NULL,
                values = c("posterior" = "solid",
                           "prior" = "dotted"),
                labels = c("posterior" = "Posterior",
                           "prior" = "Prior")
            ) +
            scale_colour_identity() +
            theme_bw() +
            theme(
                text = element_text(
                    size = 16
                ),
                panel.grid = element_blank(),
                legend.position = c(0.5, 0.9),
                legend.key.width = unit(2.5, "cm")
            )
    }
}
