descriptives <- function(id) {
    list(
        column(
            width = 3,
            general_ui(NS(id, "n_x")),
            general_ui(NS(id, "n_y"))
        ),
        column(
            width = 3,
            general_ui(NS(id, "mean_x")),
            general_ui(NS(id, "mean_y"))
        ),
        column(
            width = 3,
            general_ui(NS(id, "choose_sd_ci"))
        ),
        column(
            width = 3,
            conditionalPanel(
                condition = "input.choose_sd_ci == 'sd'",
                general_ui(NS(id, "sd_x")),
                general_ui(NS(id, "sd_y")),
                ns = NS(id)
            ),
            conditionalPanel(
                condition = "input.choose_sd_ci == 'ci'",
                general_ui(NS(id, "ci_margin")),
                general_ui(NS(id, "ci_level")),
                ns = NS(id)
            )
        )
    )
}
