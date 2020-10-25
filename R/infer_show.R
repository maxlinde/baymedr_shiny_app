infer_show <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       ni_margin = ni_margin,
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
        if (direction == "low") {
            "H+ (inferiority):             "
        } else {
            "H- (inferiority):             "
        },
        hypotheses$h0,
        "\n",
        if (direction == "low") {
            "H- (non-inferiority):         "
        } else {
            "H+ (non-inferiority):         "
        },
        hypotheses$h1,
        "\n",
        "Non-inferiority margin:       ",
        formatC(
            x = ni_margin$ni_margin_std,
            digits = 2,
            format = "f"
        ),
        " (standardised)",
        "\n",
        "                              ",
        formatC(
            x = ni_margin$ni_margin_unstd,
            digits = 2,
            format = "f"
        ),
        " (unstandardised)",
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
            "    BF-+ (non-inferiority) = "
        } else {
            "    BF+- (non-inferiority) = "
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