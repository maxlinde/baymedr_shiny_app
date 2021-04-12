infer_form <- function(n_x = n_x,
                       n_y = n_y,
                       t = t_stat,
                       test = test,
                       hypotheses = hypotheses,
                       ni_margin = ni_margin,
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
                    "$$\\Large\\text{BF}_{-+}=\\dfrac{\\text{BF}_{-0}}{\\text{BF}_{+0}}=\\dfrac{\\dfrac{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_-\\right)}}{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_-\\right)}}}{\\dfrac{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_+\\right)}}{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_+\\right)}}}$$",
                    colours[1],
                    colours[3],
                    colours[2],
                    colours[4]
                )
            )
        )
    } else {
        withMathJax(
            helpText(
                sprintf(
                    "$$\\Large\\text{BF}_{+-}=\\dfrac{\\text{BF}_{+0}}{\\text{BF}_{-0}}=\\dfrac{\\dfrac{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_+\\right)}}{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_+\\right)}}}{\\dfrac{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid\\mathcal{H}_-\\right)}}{\\color{%s}{\\text{p}\\left(\\delta=\\delta_0\\mid D\\text{,}~\\mathcal{H}_-\\right)}}}$$",
                    colours[2],
                    colours[4],
                    colours[1],
                    colours[3]
                )
            )
        )
    }
}