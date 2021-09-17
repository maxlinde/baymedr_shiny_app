equiv_explain <- function(n_x = n_x,
                          n_y = n_y,
                          t = t_stat,
                          test = test,
                          hypotheses = hypotheses,
                          interval = interval,
                          prior_scale = prior_scale,
                          bf = bf) {
    if (interval$interval_low_std == interval$interval_high_std) {
        "The plot shows the prior (dotted line) and posterior (solid line) for the population effect size. The Bayes factor (BF01) is the ratio of the density of the posterior at a population effect size of zero (green dot) and the density of the prior at a population effect size of zero (purple dot)."
    } else {
        "The plot shows the prior (dotted line) and posterior (solid line) for the population effect size. The two vertical dashed lines demarcate the equivalence interval. The Bayes factor (BF01) is the ratio of the posterior odds and the prior odds. The posterior odds is the ratio of the area inside the equivalence interval under the posterior (yellow area) and the area outside the equivalence interval under the posterior (green area). The prior odds is the ratio of the area inside the equivalence interval under the prior (blue area) and the area outside the equivalence interval under the prior (purple area)."
    }
}
