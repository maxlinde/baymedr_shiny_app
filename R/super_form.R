super_form <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       prior_scale = prior_scale,
                       bf = bf,
                       direction = direction) {
    colours <- viridis(
        n = 4
    )
    if (direction == "low") {
        withMathJax(
            helpText(
                sprintf(
                    "$$\\Large\\text{BF}_{-0}=\\dfrac{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_-\\right)}}{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_-\\right)}}$$",
                    colours[1],
                    colours[3]
                )
            )
        )
    } else {
        withMathJax(
            helpText(
                sprintf(
                    "$$\\Large\\text{BF}_{+0}=\\dfrac{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_+\\right)}}{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_+\\right)}}$$",
                    colours[1],
                    colours[3]
                )
            )
        )
    }
}