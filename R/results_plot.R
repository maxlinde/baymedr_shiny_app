results_plot <- function(id, ...) {
    name <- str_sub(
        string = id,
        start = 1,
        end = 5
    )
    switch(
        name,
        equiv = do.call(
            what = equiv_plot,
            args = ...
        ),
        infer = do.call(
            what = infer_plot,
            args = ...
        ),
        super = do.call(
            what = super_plot,
            args = ...
        )
    )
}

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
                fun = posterior_t,
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
                    colour = colours[c(2, 4)]
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
                limits = y_lim,
                expand = c(0, 0)
            ) +
            scale_linetype_manual(
                name = NULL,
                values = c("posterior" = "solid",
                           "prior" = "dotted"),
                labels = c("posterior" = "Posterior",
                           "prior" = "Prior")
            ) +
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
                fun = posterior_t,
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
                fun = posterior_t,
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
                fun = posterior_t,
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
                fun = posterior_t,
                args = list(t = t,
                            n_x = n_x,
                            n_y = n_y,
                            ind_samples = TRUE,
                            prior_loc = 0,
                            prior_scale = prior_scale,
                            prior_df = 1),
                xlim = c(x_lim[1], interval$interval_low_std),
                pattern = "stripe",
                pattern_colour = colours[1],
                pattern_fill = colours[1],
                pattern_angle = 45,
                pattern_density = 0.2,
                pattern_alpha = 0.5,
                pattern_spacing = 0.01,
                fill = "white",
                alpha = 0
            ) +
            geom_area_pattern(
                stat = "function",
                fun = posterior_t,
                args = list(t = t,
                            n_x = n_x,
                            n_y = n_y,
                            ind_samples = TRUE,
                            prior_loc = 0,
                            prior_scale = prior_scale,
                            prior_df = 1),
                xlim = c(interval$interval_high_std, x_lim[2]),
                pattern = "stripe",
                pattern_colour = colours[1],
                pattern_fill = colours[1],
                pattern_angle = 45,
                pattern_density = 0.2,
                pattern_alpha = 0.5,
                pattern_spacing = 0.01,
                fill = "white",
                alpha = 0
            ) +
            geom_area_pattern(
                stat = "function",
                fun = posterior_t,
                args = list(t = t,
                            n_x = n_x,
                            n_y = n_y,
                            ind_samples = TRUE,
                            prior_loc = 0,
                            prior_scale = prior_scale,
                            prior_df = 1),
                xlim = c(interval$interval_low_std, interval$interval_high_std),
                pattern = "stripe",
                pattern_colour = colours[2],
                pattern_fill = colours[2],
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
                pattern_colour = colours[3],
                pattern_fill = colours[3],
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
                pattern_colour = colours[3],
                pattern_fill = colours[3],
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
                pattern_colour = colours[4],
                pattern_fill = colours[4],
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
                limits = y_lim,
                expand = c(0, 0)
            ) +
            scale_linetype_manual(
                name = NULL,
                values = c("posterior" = "solid",
                           "prior" = "dotted"),
                labels = c("posterior" = "Posterior",
                           "prior" = "Prior")
            ) +
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

infer_plot <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       ni_margin = ni_margin,
                       prior_scale = prior_scale,
                       bf = bf,
                       bf_all = bf_all,
                       direction = directio) {
    colours <- brewer.pal(
        n = 4,
        name = "Set1"
    )
    hdi80_prior_low <- qcauchy(
        p = 0.1,
        location = ni_margin$ni_margin_std,
        scale = prior_scale
    )
    hdi80_prior_high <- qcauchy(
        p = 0.9,
        location = ni_margin$ni_margin_std,
        scale = prior_scale
    )
    hdi95_posterior_low <- unlist(
        ci_median_t(
            t = t,
            n_x = n_x,
            n_y = n_y,
            ind_samples = TRUE,
            prior_loc = ni_margin$ni_margin_std,
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
            prior_loc = ni_margin$ni_margin_std,
            prior_scale = prior_scale,
            prior_df = 1
        )$ci_upper
    )
    max_dens_prior_m0 <- optimize(
        f = dtrunc,
        interval = c(hdi80_prior_low, 0),
        spec = "cauchy",
        a = -Inf,
        b = 0,
        location = ni_margin$ni_margin_std,
        scale = prior_scale,
        maximum = TRUE,
        tol = 0.00000000001
    )$objective
    max_dens_posterior_m0 <- optimize(
        f = posterior_t,
        interval = c(hdi95_posterior_low, 0),
        t = t,
        n_x = n_x,
        n_y = n_y,
        ind_samples = TRUE,
        prior_loc = ni_margin$ni_margin_std,
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
        prior_loc = ni_margin$ni_margin_std,
        prior_scale = prior_scale,
        prior_df = 1
    )
    x_lim <- c(min(hdi80_prior_low, hdi95_posterior_low),
               max(hdi80_prior_high, hdi95_posterior_high))
    y_lim_m0 <- c(0,
                  max(max_dens_prior_m0, max_dens_posterior_m0) * 1.4)
    dens_prior0_m0 <- dtrunc(
        x = 0,
        spec = "cauchy",
        a = -Inf,
        b = 0,
        location = ni_margin$ni_margin_std,
        scale = prior_scale
    )
    dens_posterior0_m0 <- posterior_t(
        delta = 0,
        t = t,
        n_x = n_x,
        n_y = n_y,
        ind_samples = TRUE,
        prior_loc = ni_margin$ni_margin_std,
        prior_scale = prior_scale,
        prior_df = 1
    ) / cdf_t(
        x = 0,
        t = t,
        n_x = n_x,
        n_y = n_y,
        ind_samples = TRUE,
        prior_loc = ni_margin$ni_margin_std,
        prior_scale = prior_scale,
        prior_df = 1
    )
    plt_m0 <- ggplot(
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
                        location = ni_margin$ni_margin_std,
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
                            prior_loc = ni_margin$ni_margin_std,
                            prior_scale = prior_scale,
                            prior_df = 1) / cdf_t(
                                x = 0,
                                t = t,
                                n_x = n_x,
                                n_y = n_y,
                                ind_samples = TRUE,
                                prior_loc = ni_margin$ni_margin_std,
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
                y = c(0, dens_prior0_m0)
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
                y = c(0, dens_posterior0_m0)
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
                y = c(dens_prior0_m0, dens_posterior0_m0)
            ),
            mapping = aes(
                x = x,
                y = y,
                colour = colours[c(2, 4)]
            ),
            size = 5,
            show.legend = FALSE
        ) +
        scale_x_continuous(
            name = "Population effect size",
            breaks = breaks_width(
                width = 1,
                offset = -ni_margin$ni_margin_std
            ),
            labels = function(x) {
                x + ni_margin$ni_margin_std
            },
            limits = x_lim,
            expand = c(0, 0)
        ) +
        scale_y_continuous(
            name = "Density",
            limits = y_lim_m0,
            expand = c(0, 0)
        ) +
        scale_linetype_manual(
            name = NULL,
            values = c("posterior" = "solid",
                       "prior" = "dotted"),
            labels = c("posterior" = "Posterior",
                       "prior" = "Prior")
        ) +
        theme_bw() +
        theme(
            text = element_text(
                size = 16
            ),
            panel.grid = element_blank(),
            legend.position = c(0.5, 0.9),
            legend.key.width = unit(2.5, "cm")
        )
    max_dens_prior_p0 <- optimize(
        f = dtrunc,
        interval = c(0, hdi80_prior_high),
        spec = "cauchy",
        a = 0,
        b = Inf,
        location = ni_margin$ni_margin_std,
        scale = prior_scale,
        maximum = TRUE,
        tol = 0.00000000001
    )$objective
    max_dens_posterior_p0 <- optimize(
        f = posterior_t,
        interval = c(0, hdi95_posterior_high),
        t = t,
        n_x = n_x,
        n_y = n_y,
        ind_samples = TRUE,
        prior_loc = ni_margin$ni_margin_std,
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
        prior_loc = ni_margin$ni_margin_std,
        prior_scale = prior_scale,
        prior_df = 1
    ))
    x_lim <- c(min(hdi80_prior_low, hdi95_posterior_low),
               max(hdi80_prior_high, hdi95_posterior_high))
    y_lim_p0 <- c(0,
                  max(max_dens_prior_p0, max_dens_posterior_p0) * 1.4)
    dens_prior0_p0 <- dtrunc(
        x = 0,
        spec = "cauchy",
        a = 0,
        b = Inf,
        location = ni_margin$ni_margin_std,
        scale = prior_scale
    )
    dens_posterior0_p0 <- posterior_t(
        delta = 0,
        t = t,
        n_x = n_x,
        n_y = n_y,
        ind_samples = TRUE,
        prior_loc = ni_margin$ni_margin_std,
        prior_scale = prior_scale,
        prior_df = 1
    ) / (1 - cdf_t(
        x = 0,
        t = t,
        n_x = n_x,
        n_y = n_y,
        ind_samples = TRUE,
        prior_loc = ni_margin$ni_margin_std,
        prior_scale = prior_scale,
        prior_df = 1
    ))
    plt_p0 <- ggplot(
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
                        location = ni_margin$ni_margin_std,
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
                            prior_loc = ni_margin$ni_margin_std,
                            prior_scale = prior_scale,
                            prior_df = 1) / (1 - cdf_t(
                                x = 0,
                                t = t,
                                n_x = n_x,
                                n_y = n_y,
                                ind_samples = TRUE,
                                prior_loc = ni_margin$ni_margin_std,
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
                y = c(0, dens_prior0_p0)
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
                y = c(0, dens_posterior0_p0)
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
                y = c(dens_prior0_p0, dens_posterior0_p0)
            ),
            mapping = aes(
                x = x,
                y = y,
                colour = colours[c(2, 4)]
            ),
            size = 5,
            show.legend = FALSE
        ) +
        scale_x_continuous(
            name = "Population effect size",
            breaks = breaks_width(
                width = 1,
                offset = -ni_margin$ni_margin_std
            ),
            labels = function(x) {
                x + ni_margin$ni_margin_std
            },
            limits = x_lim,
            expand = c(0, 0)
        ) +
        scale_y_continuous(
            name = "Density",
            limits = y_lim_p0,
            expand = c(0, 0)
        ) +
        scale_linetype_manual(
            name = NULL,
            values = c("posterior" = "solid",
                       "prior" = "dotted"),
            labels = c("posterior" = "Posterior",
                       "prior" = "Prior")
        ) +
        theme_bw() +
        theme(
            text = element_text(
                size = 16
            ),
            panel.grid = element_blank(),
            legend.position = c(0.5, 0.9),
            legend.key.width = unit(2.5, "cm")
        )
    if (direction == "low") {
        plot_grid(
            plotlist = list(plt_m0, plt_p0),
            nrow = 2,
            labels = c("A", "B"),
            align = "hv"
        )
    } else {
        plot_grid(
            plotlist = list(plt_p0, plt_m0),
            nrow = 2,
            labels = c("A", "B"),
            align = "hv"
        )
    }
}

super_plot <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       prior_scale = prior_scale,
                       bf = bf,
                       bf_all = bf_all,
                       direction = direction,
                       alternative = alternative) {
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
    if (alternative == "two.sided") {
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
                fun = posterior_t,
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
                    colour = colours[c(2, 4)]
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
                limits = y_lim,
                expand = c(0, 0)
            ) +
            scale_linetype_manual(
                name = NULL,
                values = c("posterior" = "solid",
                           "prior" = "dotted"),
                labels = c("posterior" = "Posterior",
                           "prior" = "Prior")
            ) +
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
        if (direction == "low") {
            max_dens_prior <- dtrunc(
                x = 0,
                spec = "cauchy",
                a = -Inf,
                b = 0,
                location = 0,
                scale = prior_scale
            )
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
                        colour = colours[c(2, 4)]
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
                    limits = y_lim,
                    expand = c(0, 0)
                ) +
                scale_linetype_manual(
                    name = NULL,
                    values = c("posterior" = "solid",
                               "prior" = "dotted"),
                    labels = c("posterior" = "Posterior",
                               "prior" = "Prior")
                ) +
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
            max_dens_prior <- dtrunc(
                x = 0,
                spec = "cauchy",
                a = 0,
                b = Inf,
                location = 0,
                scale = prior_scale
            )
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
                        colour = colours[c(2, 4)]
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
                    limits = y_lim,
                    expand = c(0, 0)
                ) +
                scale_linetype_manual(
                    name = NULL,
                    values = c("posterior" = "solid",
                               "prior" = "dotted"),
                    labels = c("posterior" = "Posterior",
                               "prior" = "Prior")
                ) +
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
}
