infer_form <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       ni_margin = ni_margin,
                       prior_scale = prior_scale,
                       bf = bf,
                       direction = direction) {
    if (direction == "low") {
        withMathJax(
            helpText(
                "$$\\Large\\text{BF}_{-+}=\\dfrac{\\text{BF}_{-0}}{\\text{BF}_{+0}}=\\dfrac{\\dfrac{\\color{#E41A1C}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_-\\right)}}{\\color{#4DAF4A}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_-\\right)}}}{\\dfrac{\\color{#377EB8}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_+\\right)}}{\\color{#984EA3}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_+\\right)}}}$$"
            )
        )
    } else {
        withMathJax(
            helpText(
                "$$\\Large\\text{BF}_{+-}=\\dfrac{\\text{BF}_{+0}}{\\text{BF}_{-0}}=\\dfrac{\\dfrac{\\color{#377EB8}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_+\\right)}}{\\color{#984EA3}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_+\\right)}}}{\\dfrac{\\color{#E41A1C}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_-\\right)}}{\\color{#4DAF4A}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_-\\right)}}}$$"
            )
        )
    }
}