prior_scale_plot <- function(prior_scale) {
    prior_scale <- eval(
        parse(
            text = prior_scale
        )
    )
    x_lim <- qcauchy(
        p = c(0.1, 0.9),
        location = 0,
        scale = prior_scale
    )
    y_max <- dcauchy(
        x = 0,
        location = 0,
        scale = prior_scale
    )
    ggplot(
        data = data.frame(
            x = 0
        ),
        mapping = aes(
            x = x
        )
    ) +
        stat_function(
            mapping = aes(linetype = "prior"),
            fun = dcauchy,
            n = 10001,
            size = 2,
            xlim = x_lim,
            args = list(location = 0,
                        scale = prior_scale)
        ) +
        scale_x_continuous(
            name = "Population effect size",
            limits = x_lim,
            expand = c(0, 0)
        ) +
        scale_y_continuous(
            name = "Density",
            limits = c(0, y_max * 1.4)
        ) +
        scale_linetype_manual(
            name = NULL,
            values = c("prior" = "dotted"),
            labels = c("prior" = "Prior")
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