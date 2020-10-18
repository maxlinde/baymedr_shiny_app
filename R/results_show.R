results_show <- function(id, ...) {
    name <- str_sub(
        string = id,
        start = 1,
        end = 5
    )
    switch(
        name,
        equiv = do.call(
            what = equiv_show,
            args = ...
        ),
        infer = do.call(
            what = infer_show,
            args = ...
        ),
        super = do.call(
            what = super_show,
            args = ...
        )
    )
}
