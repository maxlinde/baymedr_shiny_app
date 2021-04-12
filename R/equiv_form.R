equiv_form <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       interval = interval,
                       prior_scale = prior_scale,
                       bf = bf) {
    colours <- viridis(
        n = 4
    )
    if (interval$interval_low_std == interval$interval_high_std) {
        withMathJax(
            helpText(
                sprintf(
                    "$$\\Large\\text{BF}_{01}=\\dfrac{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_1\\right)}}{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_1\\right)}}$$",
                    colours[3],
                    colours[1]
                )
            )
        )
    } else {
        withMathJax(
            helpText(
                sprintf(
                    "$$\\Large\\text{BF}_{01}=\\dfrac{\\dfrac{\\color{%s}{\\text{p}\\left(\\mathcal{H}_0\\mid D\\right)}}{\\color{%s}{\\text{p}\\left(\\mathcal{H}_1\\mid D\\right)}}}{\\dfrac{\\color{%s}{\\text{p}\\left(\\mathcal{H}_0\\right)}}{\\color{%s}{\\text{p}\\left(\\mathcal{H}_1\\right)}}}$$",
                    colours[4],
                    colours[3],
                    colours[2],
                    colours[1]
                )
            )
        )
    }
}