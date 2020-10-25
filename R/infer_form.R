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
                "$$\\huge\\text{BF}_{10}=\\dfrac{\\dfrac{\\color{#E41A1C}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_1\\right)}}{\\color{#4DAF4A}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_1\\right)}}}{\\dfrac{\\color{#377EB8}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_1\\right)}}{\\color{#984EA3}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_1\\right)}}}$$"
            )
        )
    } else {
        withMathJax(
            helpText(
                "$$\\huge\\text{BF}_{10}=\\dfrac{\\dfrac{\\color{#377EB8}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_1\\right)}}{\\color{#984EA3}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_1\\right)}}}{\\dfrac{\\color{#E41A1C}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_1\\right)}}{\\color{#4DAF4A}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_1\\right)}}}$$"
            )
        )
    }
}