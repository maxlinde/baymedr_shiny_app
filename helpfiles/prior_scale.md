The scale of the Cauchy prior is a parameter, which determines the spread of the distribution. For example, if the scale is $1$, then $50\%$ of the area under the Cauchy prior lies between $-1$ and $1$. The Cauchy prior scale can be used to set the range of plausible or interesting effect sizes. If only small effect sizes are expected, then a small Cauchy prior scale could be selected. In turn, if large effect sizes are expected, then a larger Cauchy prior scale could be chosen.

The plot just below the field for entering the desired Cauchy prior scale shows the prior with the entered scale parameter. Its purpose is to help you choose a proper Cauchy prior scale prior to actually conducting the analysis. Note that for some research designs (e.g., non-inferiority, superiority) we use so-called truncated Cauchy priors, where the Cauchy distribution is cut at a certain point (e.g., only considering $x$-values from $-\infty$ to 0). Therefore, this plot only serves illustrative purposes and might not match the prior that is used in the actual calculations.

In many software packages, default Cauchy prior scales can be selected. For example, the BayesFactor software (Morey & Rouder, 2018) written in R (R Core Team, 2019) uses the following:

- medium: $1 / \sqrt{2} \approx 0.707$
- wide: $1$
- ultrawide: $\sqrt{2} \approx 1.414$

These default values often provide good starting points. Ultimately, however, the chosen Cauchy prior scale should depend on the situation at hand.
