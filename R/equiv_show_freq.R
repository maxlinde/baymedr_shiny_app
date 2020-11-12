equiv_show_freq <- function(t_stat,
                            df,
                            p_value,
                            point_null) {
    if (!point_null) {
        cat(
            "Lower bound: ",
            "t(",
            df,
            ") = ",
            formatC(
                x = t_stat$lower,
                digits = 2,
                format = "f"
            ),
            ", p = ",
            formatC(
                x = p_value$lower,
                digits = 3,
                format = "f"
            ),
            "\n",
            "Upper bound: ",
            "t(",
            df,
            ") = ",
            formatC(
                x = t_stat$upper,
                digits = 2,
                format = "f"
            ),
            ", p = ",
            formatC(
                x = p_value$upper,
                digits = 3,
                format = "f"
            ),
            sep = ""
        )
    } else {
        cat(
            "There is no frequentist solution with a point-null hypothesis instead of an interval hypothesis."
        )
    }
}
