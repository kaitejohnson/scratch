Let's assume that the data is formatted such that we have a cumulative count of the number of ED visits with a diagnosis admitted at a reference date t, with a reporting delay of d weeks, $C_{t,d}$

We will assume we know the final number of counts reported at all reference dates t, since we are computing this retrospectively.

Therefore, at each reference date, we can compute an empirical CDF of the proportion of cases reported as of each week since initial admit d.

$$
p_t(d)= \frac{C_{t,d}} {C_{t, d = d_{max}}}
$$

Where $p_t(d)$ is the monotonically increasing vector of the proportion of all diagnoses that have been reported as of delay $d$ at week $t$, and $d_{max}$ is the maximum delay, say in this case, 10 weeks.

We get a distribution of $p_t(d)$ vectors by repeating this computation for all reference dates, say, for one entire year/season.

We then use the distribution of proportions at each delay to get the median and the 2.5th and 97.5th quantiles.

To generate the nowcasted counts, we simply multiply the already observed counts, $C_{t,d}$ by the inverse of the proportion reported as of the delay $d$, to get the expected cumulative reports

$$\hat{C}_{t, d_{max}}= \frac{C_{t,d}}{M(p(d)_t)}$$

With an upper bound of: $$\hat{C}_{t, d_{max}} = \frac{C_{t,d}}{lb(p(d)_t)}$$

And a lower bound of: $$\hat{C}_{t, d_{max}} = \frac{C_{t,d}}{ub(p(d)_t)}$$
