super_show <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       prior_scale = prior_scale,
                       bf = bf,
                       direction = direction) {
    cat(
        strrep(
            x = "*",
            times = 30
        ),
        "\n",
        test,
        "\n",
        strrep(
            x = "-",
            times = nchar(test)
        ),
        "\n",
        "H0 (non-superiority):         ",
        hypotheses$h0,
        "\n",
        if (direction == "low") {
            "H- (superiority):             "
        } else {
            "H+ (superiority):             "
        },
        hypotheses$h1,
        "\n",
        "Cauchy prior scale:           ",
        formatC(
            x = prior_scale,
            digits = 3,
            format = "f"
        ),
        "\n",
        "\n",
        if (direction == "low") {
            "    BF-0 (superiority) = "
        } else {
            "    BF+0 (superiority) = "
        },
        if (bf > 1 / 1000 & bf < 1000) {
            formatC(
                x = bf,
                digits = 3,
                format = "f"
            )
        } else {
            formatC(
                x = bf,
                digits = 3,
                format = "e"
            )
        },
        "\n",
        strrep(
            x = "*",
            times = 30
        ),
        "\n",
        sep = ""
    )
}
