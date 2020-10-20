results_form <- function(id, ...) {
    name <- str_sub(
        string = id,
        start = 1,
        end = 5
    )
    switch(
        name,
        equiv = do.call(
            what = equiv_form,
            args = ...
        ),
        infer = do.call(
            what = infer_form,
            args = ...
        ),
        super = do.call(
            what = super_form,
            args = ...
        )
    )
}
