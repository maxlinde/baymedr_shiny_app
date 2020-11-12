infer_show_freq <- function(t_stat,
                            df,
                            p_value) {
    cat(
        "t(",
        df,
        ") = ",
        formatC(
            x = t_stat,
            digits = 2,
            format = "f"
        ),
        ", p = ",
        formatC(
            x = p_value,
            digits = 3,
            format = "f"
        ),
        sep = ""
    )
}
