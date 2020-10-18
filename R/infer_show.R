infer_show <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       ni_margin = ni_margin,
                       prior_scale = prior_scale,
                       bf = bf,
                       bf_all = bf_all,
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
        "H0 (inferiority):             ",
        hypotheses$h0,
        "\n",
        "H1 (non-inferiority):         ",
        hypotheses$h1,
        "\n",
        paste0(
            "Non-inferiority margin:       ",
            formatC(x = ni_margin$ni_mar_std,
                    digits = 2,
                    format = "f"),
            " (standardised)",
            "\n",
            "                              ",
            formatC(x = ni_margin$ni_mar_unstd,
                    digits = 2,
                    format = "f"),
            " (unstandardised)",
        ),
        "\n",
        "Cauchy prior scale:           ",
        formatC(
            x = prior_scale,
            digits = 3,
            format = "f"
        ),
        "\n",
        "\n",
        "    BF10 (non-inferiority) = ",
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
        "\n"
    )
}