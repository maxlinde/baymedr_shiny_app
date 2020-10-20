equiv_show <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       interval = interval,
                       prior_scale = prior_scale,
                       bf = bf) {
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
        "H0 (equivalence):             ",
        hypotheses$h0,
        "\n",
        "H1 (non-equivalence):         ",
        hypotheses$h1,
        "\n",
        "Equivalence interval:          Lower = ",
        formatC(
            x = interval$interval_low_std,
            digits = 2,
            format = "f"
        ),
        "; Upper = ",
        formatC(
            x = interval$interval_high_std,
            digits = 2,
            format = "f"
        ),
        " (standardised)",
        "\n",
        "                               Lower = ",
        formatC(
            x = interval$interval_low_unstd,
            digits = 2,
            format = "f"
        ),
        "; Upper = ",
        formatC(
            x = interval$interval_high_unstd,
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
        "    BF01 (equivalence) = ",
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
