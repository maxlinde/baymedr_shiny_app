prior_scale <- function(id) {
    textInput(
        inputId = id,
        label = "Scale parameter of the Cauchy prior",
        value = "1 / sqrt(2)",
        placeholder = "e.g., '1' or '1 / sqrt(2)'"
    )
}