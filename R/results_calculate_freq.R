results_calculate_freq <- function(id, ...) {
    arguments <- list(...)
    switch(
        id,
        equivRaw = do.call(
            what = equiv_freq,
            args = arguments
        ),
        equivSummary = do.call(
            what = equiv_freq,
            args = arguments
        ),
        inferRaw = do.call(
            what = infer_freq,
            args = arguments
        ),
        inferSummary = do.call(
            what = infer_freq,
            args = arguments
        ),
        superRaw = do.call(
            what = super_freq,
            args = arguments
        ),
        superSummary = do.call(
            what = super_freq,
            args = arguments
        ),
    )
}
