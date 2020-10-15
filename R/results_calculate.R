results_calculate <- function(id, ...) {
    arguments <- list(...)
    arguments$prior_scale <- eval(
        parse(
            text = arguments$prior_scale
        )
    )
    switch(
        id,
        equivRaw = do.call(
            what = equiv_bf,
            args = arguments
        ),
        equivSummary = do.call(
            what = equiv_bf,
            args = arguments
        ),
        inferRaw = do.call(
            what = infer_bf,
            args = arguments
        ),
        inferSummary = do.call(
            what = infer_bf,
            args = arguments
        ),
        superRaw = do.call(
            what = super_bf,
            args = arguments
        ),
        superSummary = do.call(
            what = super_bf,
            args = arguments
        ),
    )
}
