results_calculate_freq <- function(id, ...) {
    name <- str_sub(
        string = id,
        start = 1,
        end = 5
    )
    arguments <- list(...)
    switch(
        name,
        equiv = do.call(
            what = equiv_freq,
            args = arguments
        ),
        infer = do.call(
            what = infer_freq,
            args = arguments
        ),
        super = do.call(
            what = super_freq,
            args = arguments
        )
    )
}
