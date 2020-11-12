equiv_form <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       interval = interval,
                       prior_scale = prior_scale,
                       bf = bf) {
    if (interval$interval_low_std == interval$interval_high_std) {
        withMathJax(
            helpText(
                "$$\\Large\\text{BF}_{01}=\\dfrac{\\color{#4DAF4A}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_1\\right)}}{\\color{#E41A1C}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_1\\right)}}$$"
            )
        )
    } else {
        withMathJax(
            helpText(
                "$$\\Large\\text{BF}_{01}=\\dfrac{\\dfrac{\\color{#984EA3}{\\text{p}\\left(\\mathcal{H}_0\\mid D\\right)}}{\\color{#4DAF4A}{\\text{p}\\left(\\mathcal{H}_1\\mid D\\right)}}}{\\dfrac{\\color{#377EB8}{\\text{p}\\left(\\mathcal{H}_0\\right)}}{\\color{#E41A1C}{\\text{p}\\left(\\mathcal{H}_1\\right)}}}$$"
            )
        )
    }
}