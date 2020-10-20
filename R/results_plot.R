results_plot <- function(id, ...) {
    name <- str_sub(
        string = id,
        start = 1,
        end = 5
    )
    switch(
        name,
        equiv = do.call(
            what = equiv_plot,
            args = ...
        ),
        infer = do.call(
            what = infer_plot,
            args = ...
        ),
        super = do.call(
            what = super_plot,
            args = ...
        )
    )
}
