results_show_freq <- function(id, ...) {
    name <- str_sub(
        string = id,
        start = 1,
        end = 5
    )
    switch(
        name,
        equiv = do.call(
            what = equiv_show_freq,
            args = ...
        ),
        infer = do.call(
            what = infer_show_freq,
            args = ...
        ),
        super = do.call(
            what = super_show_freq,
            args = ...
        )
    )
}
