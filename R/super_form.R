super_form <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       prior_scale = prior_scale,
                       bf = bf,
                       direction = direction) {
    if (direction == "low") {
        withMathJax(
            helpText(
                "$$\\huge\\text{BF}_{-0}=\\dfrac{\\color{#E41A1C}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_-\\right)}}{\\color{#4DAF4A}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_-\\right)}}$$"
            )
        )
    } else {
        withMathJax(
            helpText(
                "$$\\huge\\text{BF}_{+0}=\\dfrac{\\color{#E41A1C}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_+\\right)}}{\\color{#4DAF4A}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_+\\right)}}$$"
            )
        )
    }
}