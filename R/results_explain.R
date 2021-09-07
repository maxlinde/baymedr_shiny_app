results_explain <- function(id, ...) {
    name <- str_sub(
        string = id,
        start = 1,
        end = 5
    )
    switch(
        name,
        equiv = do.call(
            what = equiv_explain,
            args = ...
        ),
        infer = do.call(
            what = infer_explain,
            args = ...
        ),
        super = do.call(
            what = super_explain,
            args = ...
        )
    )
}