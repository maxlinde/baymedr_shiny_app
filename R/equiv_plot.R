equiv_plot <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       interval = interval,
                       prior_scale = prior_scale,
                       bf = bf) {
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
    max_dens_prior <- dcauchy(
        x = 0,
        location = 0,
        scale = prior_scale
    )
    max_dens_posterior <- optimize(
        f = posterior_t,
        interval = c(hdi95_posterior_low, hdi95_posterior_high),
        t = t,
        n_x = n_x,
        n_y = n_y,
        ind_samples = TRUE,
        prior_loc = 0,
        prior_scale = prior_scale,
        prior_df = 1,
        maximum = TRUE,
        tol = 0.00000000001
    )$objective
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
    )
    if (interval$interval_low_std == interval$interval_high_std) {
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
                fun = dcauchy,
                n = 10001,
                xlim = x_lim,
                size = 2,
                args = list(location = 0,
                            scale = prior_scale)
            ) +
            stat_function(
                mapping = aes(
                    linetype = "posterior"
                ),
                fun = posterior_t_cpp,
                n = 10001,
                xlim = x_lim,
                size = 2,
                args = list(t = t,
                            n_x = n_x,
                            n_y = n_y,
                            ind_samples = TRUE,
                            prior_loc = 0,
                            prior_scale = prior_scale,
                            prior_df = 1)
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
                fun = dcauchy,
                n = 10001,
                xlim = c(x_lim[1], interval$interval_low_std),
                size = 2,
                args = list(location = 0,
                            scale = prior_scale)
            ) +
            stat_function(
                mapping = aes(
                    linetype = "prior"
                ),
                fun = dcauchy,
                n = 10001,
                xlim = c(interval$interval_high_std, x_lim[2]),
                size = 2,
                args = list(location = 0,
                            scale = prior_scale)
            ) +
            stat_function(
                mapping = aes(
                    linetype = "prior"
                ),
                fun = dcauchy,
                n = 10001,
                xlim = c(interval$interval_low_std, interval$interval_high_std),
                size = 2,
                args = list(location = 0,
                            scale = prior_scale)
            ) +
            stat_function(
                mapping = aes(
                    linetype = "posterior"
                ),
                fun = posterior_t_cpp,
                n = 10001,
                xlim = c(x_lim[1], interval$interval_low_std),
                size = 2,
                args = list(t = t,
                            n_x = n_x,
                            n_y = n_y,
                            ind_samples = TRUE,
                            prior_loc = 0,
                            prior_scale = prior_scale,
                            prior_df = 1)
            ) +
            stat_function(
                mapping = aes(
                    linetype = "posterior"
                ),
                fun = posterior_t_cpp,
                n = 10001,
                xlim = c(interval$interval_high_std, x_lim[2]),
                size = 2,
                args = list(t = t,
                            n_x = n_x,
                            n_y = n_y,
                            ind_samples = TRUE,
                            prior_loc = 0,
                            prior_scale = prior_scale,
                            prior_df = 1)
            ) +
            stat_function(
                mapping = aes(
                    linetype = "posterior"
                ),
                fun = posterior_t_cpp,
                n = 10001,
                xlim = c(interval$interval_low_std, interval$interval_high_std),
                size = 2,
                args = list(t = t,
                            n_x = n_x,
                            n_y = n_y,
                            ind_samples = TRUE,
                            prior_loc = 0,
                            prior_scale = prior_scale,
                            prior_df = 1)
            ) +
            geom_area_pattern(
                stat = "function",
                fun = posterior_t_cpp,
                args = list(t = t,
                            n_x = n_x,
                            n_y = n_y,
                            ind_samples = TRUE,
                            prior_loc = 0,
                            prior_scale = prior_scale,
                            prior_df = 1),
                xlim = c(x_lim[1], interval$interval_low_std),
                pattern = "stripe",
                pattern_colour = colours[3],
                pattern_fill = colours[3],
                pattern_angle = 45,
                pattern_density = 0.2,
                pattern_alpha = 0.5,
                pattern_spacing = 0.01,
                fill = "white",
                alpha = 0
            ) +
            geom_area_pattern(
                stat = "function",
                fun = posterior_t_cpp,
                args = list(t = t,
                            n_x = n_x,
                            n_y = n_y,
                            ind_samples = TRUE,
                            prior_loc = 0,
                            prior_scale = prior_scale,
                            prior_df = 1),
                xlim = c(interval$interval_high_std, x_lim[2]),
                pattern = "stripe",
                pattern_colour = colours[3],
                pattern_fill = colours[3],
                pattern_angle = 45,
                pattern_density = 0.2,
                pattern_alpha = 0.5,
                pattern_spacing = 0.01,
                fill = "white",
                alpha = 0
            ) +
            geom_area_pattern(
                stat = "function",
                fun = posterior_t_cpp,
                args = list(t = t,
                            n_x = n_x,
                            n_y = n_y,
                            ind_samples = TRUE,
                            prior_loc = 0,
                            prior_scale = prior_scale,
                            prior_df = 1),
                xlim = c(interval$interval_low_std, interval$interval_high_std),
                pattern = "stripe",
                pattern_colour = colours[4],
                pattern_fill = colours[4],
                pattern_angle = 45,
                pattern_density = 0.2,
                pattern_alpha = 0.5,
                pattern_spacing = 0.01,
                fill = "white",
                alpha = 0
            ) +
            geom_area_pattern(
                stat = "function",
                fun = "dcauchy",
                args = list(location = 0,
                            scale = prior_scale),
                xlim = c(x_lim[1], interval$interval_low_std),
                pattern = "stripe",
                pattern_colour = colours[1],
                pattern_fill = colours[1],
                pattern_angle = 135,
                pattern_density = 0.2,
                pattern_alpha = 0.5,
                pattern_spacing = 0.01,
                fill = "white",
                alpha = 0
            ) +
            geom_area_pattern(
                stat = "function",
                fun = "dcauchy",
                args = list(location = 0,
                            scale = prior_scale),
                xlim = c(interval$interval_high_std, x_lim[2]),
                pattern = "stripe",
                pattern_colour = colours[1],
                pattern_fill = colours[1],
                pattern_angle = 135,
                pattern_density = 0.2,
                pattern_alpha = 0.5,
                pattern_spacing = 0.01,
                fill = "white",
                alpha = 0
            ) +
            geom_area_pattern(
                stat = "function",
                fun = "dcauchy",
                args = list(location = 0,
                            scale = prior_scale),
                xlim = c(interval$interval_low_std, interval$interval_high_std),
                pattern = "stripe",
                pattern_colour = colours[2],
                pattern_fill = colours[2],
                pattern_angle = 135,
                pattern_density = 0.2,
                pattern_alpha = 0.5,
                pattern_spacing = 0.01,
                fill = "white",
                alpha = 0
            ) +
            geom_vline(
                xintercept = c(interval$interval_low_std, interval$interval_high_std),
                linetype = "dashed"
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
