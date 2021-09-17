infer_explain <- function(n_x = n_x,
                          n_y = n_y,
                          t = t_stat,
                          test = test,
                          hypotheses = hypotheses,
                          ni_margin = ni_margin,
                          prior_scale = prior_scale,
                          bf = bf,
                          direction = direction) {
    if (direction == "low") {
        "Plot A shows the truncated prior (dotted line) and truncated posterior (solid line) for population effect sizes belonging to the one-sided negative alternative hypothesis. Plot B shows the truncated prior (dotted line) and truncated posterior (solid line) for population effect sizes belonging to the one-sided positive null hypothesis. The Bayes factor (BF-+) is the ratio of two one-sided Bayes factors: The first one-sided Bayes factor (BF-0) is the density of the prior in plot A at a population effect size that equals the non-inferiority margin (green dot) over the density of the posterior in plot A at a population effect size that equals the non-inferiority margin (purple dot); the second one-sided Bayes factor (BF+0) is the density of the prior in plot B at a population effect size that equals the non-inferiority margin (blue dot) over the density of the posterior in plot B at a population effect size that equals the non-inferiority margin (yellow dot)."
    } else {
        "Plot A shows the truncated prior (dotted line) and truncated posterior (solid line) for population effect sizes belonging to the one-sided positive alternative hypothesis. Plot B shows the truncated prior (dotted line) and truncated posterior (solid line) for population effect sizes belonging to the one-sided negative null hypothesis. The Bayes factor (BF+-) is the ratio of two one-sided Bayes factors: The first one-sided Bayes factor (BF+0) is the density of the prior in plot A at a population effect size that equals the non-inferiority margin (blue dot) over the density of the posterior in plot A at a population effect size that equals the non-inferiority margin (yellow dot); the second one-sided Bayes factor (BF-0) is the density of the prior in plot B at a population effect size that equals the non-inferiority margin (purple dot) over the density of the posterior in plot B at a population effect size that equals the non-inferiority margin (green dot)."
	}
}
