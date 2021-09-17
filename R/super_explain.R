super_explain <- function(n_x = n_x,
                          n_y = n_y,
                          t = t_stat,
                          test = test,
                          hypotheses = hypotheses,
                          prior_scale = prior_scale,
                          bf = bf,
                          direction = direction) {
    if (direction == "low") {
        "The plot shows the truncated prior (dotted line) and truncated posterior (solid line) for population effect sizes belonging to the one-sided negative alternative hypothesis. The one-sided Bayes factor (BF-0) is the ratio of the density of the prior at a population effect size of zero (purple dot) and the density of the posterior at a population effect size of zero (green dot)."
    } else {
        "The plot shows the truncated prior (dotted line) and truncated posterior (solid line) for population effect sizes belonging to the one-sided positive alternative hypothesis. The one-sided Bayes factor (BF+0) is the ratio of the density of the prior at a population effect size of zero (purple dot) and the density of the posterior at a population effect size of zero (green dot)."
    }
}
