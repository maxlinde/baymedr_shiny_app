results_calculate <- function(id, ...) {
    name <- str_sub(
        string = id,
        start = 1,
        end = 5
    )
    arguments <- list(...)
    arguments$prior_scale <- eval(
        parse(
            text = arguments$prior_scale
        )
    )
    switch(
        name,
        equiv = do.call(
            what = equiv_bf,
            args = arguments
        ),
        infer = do.call(
            what = infer_bf,
            args = arguments
        ),
        super = do.call(
            what = super_bf,
            args = arguments
        )
    )
}
