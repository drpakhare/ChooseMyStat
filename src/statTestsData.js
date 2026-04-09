// ──────────────────────────────────────────────────────────
// ChooseMyStat — Test Definitions & Decision Engine
// ──────────────────────────────────────────────────────────
// Each test has two SAP/example variants:
//   sap / example          → ASA 2016 compliant (effect-first, no bright-line)
//   sapTraditional / exampleTraditional → Traditional (p < 0.05 threshold)
// R code uses tidyverse ecosystem: gtsummary, sjPlot, finalfit, broom, etc.

export const TESTS = {

  // ━━━ T-TESTS ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  independent_t: {
    name: "Independent Samples t-test",
    when: "Comparing means of a continuous, normally distributed outcome between two independent groups.",
    assumptions: "Normal distribution in each group (or n >= 30 by CLT), equal variances (Levene's test), independent observations.",

    sap: `Continuous variables will be compared between the two groups using the Independent Samples t-test after assessing normality (Shapiro-Wilk test and histogram inspection). Welch's correction will be applied if Levene's test indicates unequal variances. The primary measure of effect will be the mean difference with its 95% confidence interval. Cohen's d will be reported as a standardised effect size. The p-value will be reported as a continuous index of evidence and interpreted alongside the effect estimate and its precision.`,
    sapTraditional: `Continuous variables will be compared between the two groups using the Independent Samples t-test after confirming normality using the Shapiro-Wilk test. Equality of variances will be assessed using Levene's test; Welch's correction will be applied if variances are unequal. Results will be expressed as mean ± SD. A two-sided p-value < 0.05 will be considered statistically significant.`,

    example: `The mean SBP in the intervention group was 128.4 ± 12.3 mmHg vs 138.7 ± 14.1 mmHg in control (mean difference: −10.3 mmHg, 95% CI: −15.2 to −5.4, t(98) = −4.12, p < 0.001, Cohen's d = 0.78). The 10 mmHg reduction exceeds the MCID of 5 mmHg, and the CI excludes trivially small effects, providing strong evidence for a clinically meaningful benefit.`,
    exampleTraditional: `The mean SBP in the intervention group was 128.4 ± 12.3 mmHg compared to 138.7 ± 14.1 mmHg in the control group. The difference was statistically significant (mean difference: −10.3 mmHg, 95% CI: −15.2 to −5.4, t(98) = −4.12, p < 0.001).`,

    report: "Mean ± SD per group, mean difference with 95% CI, t-statistic, df, p-value, Cohen's d, clinical interpretation",

    jasp: `Analyses → T-Tests → Independent Samples T-Test

1. Drag outcome → "Variables", group → "Grouping Variable"
2. Tests: Student (+ Welch if variances may differ)
3. Additional Statistics:
   • Mean difference + 95% CI
   • Effect size: Cohen's d + CI
   • Descriptives
4. Assumption Checks: Normality + Levene's`,

    r: `library(tidyverse)
library(gtsummary)
library(effectsize)
library(car)

# ── Descriptives + test in one table ──
df |>
  select(group, outcome) |>
  tbl_summary(
    by = group,
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 1
  ) |>
  add_difference() |>
  add_p(test = all_continuous() ~ "t.test")

# ── Welch's t-test (default) ──
t.test(outcome ~ group, data = df)

# ── Cohen's d ──
cohens_d(outcome ~ group, data = df)

# ── Assumption checks ──
df |> group_by(group) |>
  summarise(shapiro_p = shapiro.test(outcome)$p.value)
leveneTest(outcome ~ group, data = df)`
  },

  paired_t: {
    name: "Paired t-test",
    when: "Comparing means of a continuous, normally distributed outcome measured at two time points in the same subjects.",
    assumptions: "Paired differences are normally distributed, observations are paired.",

    sap: `The change in [outcome] from baseline to follow-up will be assessed using the Paired t-test after confirming normality of the paired differences (Shapiro-Wilk test). The primary effect measure will be the mean paired difference with 95% CI. Cohen's d for paired designs will be reported. The p-value will be interpreted alongside the magnitude of change and its clinical context.`,
    sapTraditional: `The change in [outcome] from baseline to follow-up will be assessed using the Paired t-test after confirming that the paired differences are normally distributed (Shapiro-Wilk test). Results will be expressed as mean difference ± SD. A two-sided p-value < 0.05 will be considered statistically significant.`,

    example: `HbA1c decreased from 8.6 ± 1.2% at baseline to 7.4 ± 1.1% at 6 months (mean change: −1.2%, 95% CI: −1.6 to −0.8, t(49) = −5.87, p < 0.001, Cohen's d = 1.04). The 1.2% reduction is clinically meaningful (MCID ≥ 0.5%) and the narrow CI indicates a precise estimate.`,
    exampleTraditional: `The mean HbA1c decreased from 8.6 ± 1.2% at baseline to 7.4 ± 1.1% at 6 months. The difference was statistically significant (mean difference: −1.2%, 95% CI: −1.6 to −0.8, t(49) = −5.87, p < 0.001).`,

    report: "Pre/post means ± SD, mean difference with 95% CI, t-statistic, df, p-value, Cohen's d",

    jasp: `Analyses → T-Tests → Paired Samples T-Test

1. Move pre and post variables into paired boxes
2. Tests: Student
3. Additional Statistics:
   • Mean difference + 95% CI
   • Effect size: Cohen's d + CI
   • Descriptives
4. Assumption Checks: Normality`,

    r: `library(tidyverse)
library(gtsummary)
library(effectsize)

# ── Paired t-test ──
t.test(df$post, df$pre, paired = TRUE)

# ── Tidy summary table ──
df_long <- df |>
  pivot_longer(c(pre, post), names_to = "time", values_to = "outcome")

df_long |>
  tbl_summary(by = time,
    statistic = all_continuous() ~ "{mean} ({sd})") |>
  add_p(test = everything() ~ "paired.t.test",
        group = id)

# ── Effect size ──
cohens_d(df$post, df$pre, paired = TRUE)

# ── Normality of differences ──
shapiro.test(df$post - df$pre)`
  },

  one_sample_t: {
    name: "One-Sample t-test",
    when: "Comparing the mean of a single group against a known or hypothesized population value.",
    assumptions: "Normal distribution (or n >= 30), continuous outcome.",

    sap: `The mean [outcome] will be compared against the reference value of [value] using the one-sample t-test. The primary effect measure will be the mean difference from the reference with its 95% CI. Cohen's d will quantify the standardised distance from the reference.`,
    sapTraditional: `The mean [outcome] will be compared against the reference value of [value] using the one-sample t-test. Results will be expressed as mean ± SD. A two-sided p-value < 0.05 will be considered statistically significant.`,

    example: `Mean haemoglobin was 10.8 ± 1.6 g/dL, 1.2 g/dL below the WHO threshold of 12 g/dL (95% CI of difference: −1.6 to −0.8, t(79) = −5.4, p < 0.001, Cohen's d = 0.75). The CI indicates the population mean is likely 0.8–1.6 g/dL below the threshold — a clinically meaningful deficit.`,
    exampleTraditional: `The mean haemoglobin was 10.8 ± 1.6 g/dL, which was significantly lower than the WHO threshold of 12 g/dL (mean difference: −1.2 g/dL, 95% CI: −1.6 to −0.8, t(79) = −5.4, p < 0.001).`,

    report: "Mean ± SD, 95% CI of mean, difference from reference with 95% CI, t-statistic, df, p-value, Cohen's d",

    jasp: `Analyses → T-Tests → One Sample T-Test

1. Move variable → "Variables"
2. Set "Test value" to reference
3. Additional Statistics: Mean difference + CI, Cohen's d + CI, Descriptives`,

    r: `library(gtsummary)
library(effectsize)

# ── One-sample t-test ──
t.test(df$outcome, mu = 12)

# ── Effect size ──
cohens_d(df$outcome, mu = 12)

# ── Quick descriptives ──
df |> select(outcome) |>
  tbl_summary(statistic = all_continuous() ~ "{mean} ({sd})")`
  },

  // ━━━ NON-PARAMETRIC ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  mann_whitney: {
    name: "Mann-Whitney U Test",
    when: "Comparing a continuous or ordinal outcome between two independent groups when data is non-normal.",
    assumptions: "Independent observations, similar distribution shape across groups. Also called Wilcoxon rank-sum test.",

    sap: `Non-normally distributed continuous variables will be compared using the Mann-Whitney U test. Normality will be assessed via Shapiro-Wilk test and histograms. The Hodges-Lehmann estimate of the location shift with 95% CI will be the primary effect measure. The rank-biserial correlation will quantify effect magnitude.`,
    sapTraditional: `Non-normally distributed continuous variables will be compared between the two groups using the Mann-Whitney U test. Normality will be assessed using the Shapiro-Wilk test. Results will be expressed as median (IQR). A two-sided p-value < 0.05 will be considered statistically significant.`,

    example: `Median hospital stay was 5 days (IQR: 3–8) in the intervention group vs 8 days (IQR: 5–12) in control (Hodges-Lehmann shift: −3 days, 95% CI: −5 to −1, U = 312, p = 0.008, rank-biserial r = 0.38). The estimated 3-day reduction is clinically relevant and the CI excludes zero.`,
    exampleTraditional: `The median hospital stay was 5 days (IQR: 3–8) in the intervention group compared to 8 days (IQR: 5–12) in the control group. The difference was statistically significant (Mann-Whitney U = 312, p = 0.008).`,

    report: "Median (IQR) per group, Hodges-Lehmann estimate with 95% CI, U-statistic, p-value, rank-biserial r",

    jasp: `Analyses → T-Tests → Independent Samples T-Test

1. Variables + Grouping Variable as usual
2. Tests: check "Mann-Whitney" (uncheck Student/Welch)
3. Additional Statistics:
   • Hodges-Lehmann estimate + CI
   • Rank biserial correlation + CI
   • Descriptives`,

    r: `library(tidyverse)
library(gtsummary)
library(effectsize)

# ── Descriptives + p-value ──
df |>
  tbl_summary(by = group,
    statistic = all_continuous() ~ "{median} ({p25}, {p75})") |>
  add_p(test = all_continuous() ~ "wilcox.test")

# ── Mann-Whitney with CI ──
wilcox.test(outcome ~ group, data = df, conf.int = TRUE)

# ── Effect size ──
rank_biserial(outcome ~ group, data = df)`
  },

  wilcoxon: {
    name: "Wilcoxon Signed-Rank Test",
    when: "Comparing a continuous or ordinal outcome at two time points in the same subjects when differences are non-normal.",
    assumptions: "Paired observations, differences are symmetric around the median.",

    sap: `The change in [outcome] will be assessed using the Wilcoxon Signed-Rank test. The pseudo-median of differences with 95% CI will be the primary effect measure. The matched-pairs rank-biserial correlation will quantify effect magnitude.`,
    sapTraditional: `The change in [outcome] will be assessed using the Wilcoxon Signed-Rank test, as the paired differences are not normally distributed. Results will be expressed as median (IQR). A two-sided p-value < 0.05 will be considered statistically significant.`,

    example: `Median pain score decreased from 7 (IQR: 5–8) to 4 (IQR: 2–6) at 4 weeks (pseudo-median difference: −3, 95% CI: −4 to −2, V = 412, p < 0.001, matched-pairs r = 0.72). The 3-point reduction exceeds the MCID of 2 points on this 10-point scale.`,
    exampleTraditional: `The median pain score decreased from 7 (IQR: 5–8) at baseline to 4 (IQR: 2–6) at 4 weeks (Wilcoxon Z = −3.42, p < 0.001).`,

    report: "Median (IQR) at each time point, pseudo-median difference with 95% CI, V-statistic, p-value, matched-pairs r",

    jasp: `Analyses → T-Tests → Paired Samples T-Test

1. Move pre/post variables into paired boxes
2. Tests: check "Wilcoxon signed-rank" (uncheck Student)
3. Additional Statistics: Hodges-Lehmann estimate + CI, Rank biserial + CI, Descriptives`,

    r: `library(tidyverse)
library(effectsize)

# ── Wilcoxon signed-rank with CI ──
wilcox.test(df$post, df$pre, paired = TRUE, conf.int = TRUE)

# ── Effect size ──
rank_biserial(df$post, df$pre, paired = TRUE)

# ── Descriptives ──
tibble(
  time  = c("Pre", "Post"),
  med   = c(median(df$pre), median(df$post)),
  iqr_l = c(quantile(df$pre, .25), quantile(df$post, .25)),
  iqr_u = c(quantile(df$pre, .75), quantile(df$post, .75))
)`
  },

  // ━━━ ANOVA / KRUSKAL-WALLIS ━━━━━━━━━━━━━━━━━━━━━━━━━━

  one_way_anova: {
    name: "One-Way ANOVA",
    when: "Comparing means of a continuous, normally distributed outcome across three or more independent groups.",
    assumptions: "Normality within each group, homogeneity of variances (Levene's test), independent observations.",

    sap: `Continuous variables will be compared across [number] groups using One-Way ANOVA. Eta-squared will quantify the proportion of variance explained. Post-hoc pairwise comparisons will use Tukey's HSD, reporting mean differences with 95% CIs. Welch's ANOVA will be used if variances are heterogeneous. Interpretation will focus on the pattern and magnitude of group differences.`,
    sapTraditional: `Continuous variables will be compared across the [number] groups using One-Way ANOVA after confirming normality and homogeneity of variances. Post-hoc pairwise comparisons will be performed using Tukey's HSD if the overall F-test is significant at p < 0.05. Results will be expressed as mean ± SD. A p-value < 0.05 will be considered statistically significant.`,

    example: `Fasting glucose differed across three groups (A: 126 ± 18, B: 142 ± 22, C: 158 ± 25 mg/dL; F(2,147) = 8.45, p < 0.001, η² = 0.10). Post-hoc Tukey HSD: A vs C −32 mg/dL (95% CI: −48 to −16, p < 0.001) — clinically relevant; A vs B −16 mg/dL (95% CI: −32 to 0, p = 0.05) — of uncertain clinical importance.`,
    exampleTraditional: `There was a statistically significant difference in fasting glucose across groups (F(2,147) = 8.45, p < 0.001). Post-hoc Tukey HSD revealed Group A was significantly lower than Group C (mean difference: −32 mg/dL, p < 0.001).`,

    report: "Mean ± SD per group, F-statistic, df, p-value, eta-squared, post-hoc mean differences with 95% CIs",

    jasp: `Analyses → ANOVA → ANOVA

1. Outcome → "Dependent Variable", Group → "Fixed Factors"
2. Additional Statistics: η² or ω² + CI, Descriptives, Homogeneity corrections
3. Post Hoc Tests: move factor right → select Tukey, check CI adjustment
4. Assumption Checks: Levene's, Q-Q plots`,

    r: `library(tidyverse)
library(gtsummary)
library(car)
library(effectsize)

# ── Descriptives + ANOVA p in one table ──
df |>
  tbl_summary(by = group,
    statistic = all_continuous() ~ "{mean} ({sd})") |>
  add_p(test = all_continuous() ~ "aov")

# ── One-Way ANOVA ──
model <- aov(outcome ~ group, data = df)
summary(model)
eta_squared(model, ci = 0.95)

# ── Post-hoc (Tukey) ──
TukeyHSD(model)

# ── If variances unequal → Welch's ANOVA ──
oneway.test(outcome ~ group, data = df)
# + Games-Howell post-hoc
library(PMCMRplus)
gamesHowellTest(outcome ~ group, data = df)

# ── Assumption checks ──
leveneTest(outcome ~ group, data = df)
shapiro.test(residuals(model))`
  },

  kruskal_wallis: {
    name: "Kruskal-Wallis Test",
    when: "Comparing a continuous or ordinal outcome across three or more independent groups when data is non-normal.",
    assumptions: "Independent observations, similar distribution shape across groups.",

    sap: `Non-normally distributed variables will be compared across [number] groups using the Kruskal-Wallis test. Epsilon-squared will quantify effect magnitude. Post-hoc Dunn's test with Bonferroni correction will identify pairwise differences. Results will be expressed as median (IQR).`,
    sapTraditional: `Non-normally distributed variables will be compared across groups using the Kruskal-Wallis test. Post-hoc Dunn's test with Bonferroni correction will be used if the overall test is significant. Results will be expressed as median (IQR). A p-value < 0.05 will be considered statistically significant.`,

    example: `ICU stay differed by severity (Mild: 2d, IQR 1–4; Moderate: 5d, IQR 3–8; Severe: 10d, IQR 7–15; H(2) = 24.6, p < 0.001, ε² = 0.17). Dunn's post-hoc: Severe vs Mild adjusted p < 0.001 — a clinically substantial 8-day difference.`,
    exampleTraditional: `There was a statistically significant difference in ICU stay across severity groups (H(2) = 24.6, p < 0.001). Dunn's post-hoc test showed a significant difference between Severe and Mild groups (adjusted p < 0.001).`,

    report: "Median (IQR) per group, H-statistic, df, p-value, epsilon-squared, Dunn's post-hoc with adjusted p-values",

    jasp: `Analyses → ANOVA → ANOVA

1. Outcome → "Dependent Variable", Group → "Fixed Factors"
2. Check "Nonparametric: Kruskal-Wallis"
3. Post-hoc: Dunn's test with Bonferroni correction
4. Descriptives for medians/IQRs`,

    r: `library(tidyverse)
library(gtsummary)
library(effectsize)
library(dunn.test)

# ── Descriptives + test ──
df |>
  tbl_summary(by = group,
    statistic = all_continuous() ~ "{median} ({p25}, {p75})") |>
  add_p(test = all_continuous() ~ "kruskal.test")

# ── Kruskal-Wallis ──
kruskal.test(outcome ~ group, data = df)
rank_epsilon_squared(outcome ~ group, data = df)

# ── Post-hoc: Dunn's test ──
dunn.test(df$outcome, df$group, method = "bonferroni")`
  },

  // ━━━ CATEGORICAL TESTS ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  chi_square: {
    name: "Chi-Square Test",
    when: "Testing the association between two categorical variables or comparing proportions across groups.",
    assumptions: "Independent observations, expected cell frequency ≥ 5 in ≥ 80% of cells.",

    sap: `The association between [exposure] and [outcome] will be assessed using the Chi-square test. If > 20% of expected cells are < 5, Fisher's exact test will be used. The primary effect measure will be Cramér's V (or phi for 2×2). Risk difference or odds ratio with 95% CI will be reported for 2×2 tables.`,
    sapTraditional: `The association between [exposure] and [outcome] will be assessed using the Chi-square test of independence. Fisher's exact test will be used if expected cell frequencies are < 5. Results will be expressed as frequency (%). A two-sided p-value < 0.05 will be considered statistically significant.`,

    example: `Treatment success: 42/60 (70%) intervention vs 28/60 (46.7%) control (χ² = 6.94, df = 1, p = 0.008, φ = 0.24; risk difference: 23.3%, 95% CI: 6.8%–39.8%; OR = 2.69, 95% CI: 1.27–5.67). The data provide evidence that ~23 additional patients per 100 achieve success with the intervention.`,
    exampleTraditional: `Treatment success was observed in 42/60 (70%) in the intervention group vs 28/60 (46.7%) in control. The difference was statistically significant (χ² = 6.94, df = 1, p = 0.008).`,

    report: "Frequency (%) per group, χ² statistic, df, p-value, phi/Cramér's V, risk difference or OR with 95% CI",

    jasp: `Analyses → Frequencies → Contingency Tables

1. Row variable → "Rows", Column → "Columns"
2. Statistics: Chi-squared, Phi/Cramér's V, Odds ratio + CI
3. Cells: Observed + Expected counts, Row or Column %
4. Optionally: Z-test for column proportions`,

    r: `library(tidyverse)
library(gtsummary)
library(effectsize)
library(epitools)

# ── Publication-ready table ──
df |>
  tbl_summary(by = group,
    statistic = all_categorical() ~ "{n} ({p}%)") |>
  add_p(test = all_categorical() ~ "chisq.test") |>
  add_difference()

# ── Chi-square ──
tab <- table(df$exposure, df$outcome)
chisq.test(tab)
cramers_v(tab, ci = 0.95)

# ── Odds ratio + risk difference (2×2) ──
oddsratio(tab)
prop.test(c(42, 28), c(60, 60))  # risk diff with CI`
  },

  fisher: {
    name: "Fisher's Exact Test",
    when: "Testing association between two categorical variables when sample size is small or expected cells < 5.",
    assumptions: "Independent observations, typically 2×2 table.",

    sap: `The association between [exposure] and [outcome] will be assessed using Fisher's exact test given expected cell frequencies < 5. The odds ratio with 95% CI will be the primary effect measure. Interpretation will emphasise the precision of the estimate (CI width) given the small sample.`,
    sapTraditional: `The association between [exposure] and [outcome] will be assessed using Fisher's exact test, given the small sample size. Results will be expressed as frequency (%). A two-sided p-value < 0.05 will be considered statistically significant.`,

    example: `ADR occurred in 3/15 (20%) Drug A vs 1/15 (6.7%) Drug B (Fisher's p = 0.598, OR = 3.50, 95% CI: 0.32–38.3). The wide CI reflects the small sample; the data are compatible with both increased and decreased risk — a larger study is needed.`,
    exampleTraditional: `ADR was observed in 3/15 (20%) receiving Drug A vs 1/15 (6.7%) receiving Drug B. The difference was not statistically significant (Fisher's exact test, p = 0.598).`,

    report: "Frequency (%) per group, Fisher's p-value, OR with 95% CI",

    jasp: `Analyses → Frequencies → Contingency Tables

1. Row/Column as usual
2. Statistics: Chi-squared (JASP auto-applies Fisher's when cells < 5), Odds ratio + CI
3. Cells: Observed counts + Percentages`,

    r: `library(gtsummary)
library(epitools)

# ── Fisher's exact test ──
tab <- table(df$exposure, df$outcome)
fisher.test(tab)   # includes OR + 95% CI

# ── gtsummary table ──
df |>
  tbl_summary(by = group) |>
  add_p(test = all_categorical() ~ "fisher.test")`
  },

  mcnemar: {
    name: "McNemar's Test",
    when: "Comparing paired proportions — e.g., before/after a binary outcome in the same subjects.",
    assumptions: "Paired binary data, test uses discordant pairs.",

    sap: `The change in proportion of [outcome] before and after [intervention] will be assessed using McNemar's test. The primary effect measure will be the difference in paired proportions with 95% CI. The exact version will be used if discordant pairs < 25.`,
    sapTraditional: `The change in proportion of [outcome] before and after [intervention] will be assessed using McNemar's test for paired proportions. Results will be expressed as proportions at each time point. A two-sided p-value < 0.05 will be considered statistically significant.`,

    example: `Adequate knowledge increased from 32% (16/50) pre-training to 78% (39/50) post-training (McNemar's χ² = 18.6, p < 0.001; absolute increase: 46%, 95% CI: 30%–62%). Of 34 initially unknowledgeable patients, 23 gained adequate knowledge — a substantial and practically important shift.`,
    exampleTraditional: `The proportion with adequate knowledge increased from 32% (16/50) before training to 78% (39/50) after training (McNemar's χ² = 18.6, p < 0.001).`,

    report: "Proportions at each time point, difference with 95% CI, McNemar's χ², p-value, discordant pair counts",

    jasp: `Analyses → Frequencies → Contingency Tables

1. "Before" → Rows, "After" → Columns
2. Statistics: McNemar's test, Odds ratio + CI
3. Cells: Observed counts + Percentages
(Data must be wide format: one row per subject)`,

    r: `library(tidyverse)

# ── McNemar's test ──
tab <- table(df$before, df$after)
mcnemar.test(tab)

# ── Difference in proportions with CI ──
n <- nrow(df)
p1 <- mean(df$before == "Yes")
p2 <- mean(df$after  == "Yes")
diff <- p2 - p1
se   <- sqrt((p1*(1-p1) + p2*(1-p2) - 2*cov(df$before=="Yes", df$after=="Yes")) / n)
cat(sprintf("Diff: %.1f%% (95%% CI: %.1f%% to %.1f%%)",
  diff*100, (diff - 1.96*se)*100, (diff + 1.96*se)*100))`
  },

  // ━━━ CORRELATION ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  correlation: {
    name: "Pearson / Spearman Correlation",
    when: "Assessing the strength and direction of association between two continuous variables.",
    assumptions: "Pearson: bivariate normality, linear relationship. Spearman: ordinal or non-normal data, monotonic relationship.",

    sap: `The correlation between [variable 1] and [variable 2] will be assessed using Pearson's r (if bivariate normality holds and the relationship is linear) or Spearman's ρ (otherwise). The coefficient with 95% CI will be the primary effect measure. Strength: weak (|r| < 0.3), moderate (0.3–0.7), strong (> 0.7). A scatter plot will accompany the analysis.`,
    sapTraditional: `The correlation between [variable 1] and [variable 2] will be assessed using Pearson's or Spearman's correlation coefficient. The strength will be interpreted as weak (r < 0.3), moderate (0.3–0.7), or strong (r > 0.7). A p-value < 0.05 will be considered statistically significant.`,

    example: `BMI and fasting glucose showed a moderate positive correlation (Pearson r = 0.48, 95% CI: 0.31–0.62, p < 0.001; R² = 0.23). Approximately 23% of glucose variance is associated with BMI, suggesting a meaningful but not dominant relationship.`,
    exampleTraditional: `There was a statistically significant moderate positive correlation between BMI and fasting blood glucose (Pearson r = 0.48, p < 0.001).`,

    report: "Correlation coefficient (r or ρ) with 95% CI, R² (Pearson), scatter plot, p-value",

    jasp: `Analyses → Regression → Correlation

1. Move both variables → "Variables"
2. Coefficients: Pearson (normal) + Spearman (non-normal)
3. Check: Confidence intervals, Flag significant
4. Plots: Scatter plots, Marginal densities`,

    r: `library(tidyverse)
library(gtsummary)

# ── Pearson (with CI) ──
cor.test(df$var1, df$var2, method = "pearson")

# ── Spearman (with CI) ──
cor.test(df$var1, df$var2, method = "spearman")

# ── Scatter plot ──
ggplot(df, aes(x = var1, y = var2)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue") +
  labs(x = "Variable 1", y = "Variable 2") +
  theme_minimal()

# ── Correlation matrix (multiple variables) ──
df |> select(var1, var2, var3) |>
  tbl_summary(statistic = everything() ~ "{mean} ({sd})") |>
  add_p()`
  },

  // ━━━ REGRESSION MODELS ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  linear_regression: {
    name: "Multiple Linear Regression",
    when: "Examining the relationship between a continuous outcome and predictors while adjusting for confounders. Also covers ANCOVA.",
    assumptions: "Linear relationship, normal residuals, homoscedasticity, no multicollinearity, independence.",

    sap: `The association between [exposure] and [continuous outcome] will be assessed using multiple linear regression, adjusting for [confounders]. Model assumptions will be checked via residual plots and VIF. The primary effect measure will be the regression coefficient (B) with 95% CI. Adjusted R² will summarise model fit. P-values will be interpreted alongside effect magnitudes and their precision.`,
    sapTraditional: `The association between [exposure] and [continuous outcome] will be assessed using multiple linear regression, adjusting for [confounders]. Results will be expressed as regression coefficients (B) with 95% CI. A p-value < 0.05 will be considered statistically significant.`,

    example: `After adjusting for age, sex, and BMI, each 1-unit increase in activity score was associated with a 2.3 mg/dL decrease in FBS (B = −2.3, 95% CI: −3.8 to −0.8, p = 0.003; adjusted R² = 0.34). This modest but consistent effect aligns with prior dose-response evidence.`,
    exampleTraditional: `Physical activity score was significantly associated with FBS after adjusting for confounders (B = −2.3, 95% CI: −3.8 to −0.8, p = 0.003). The model explained 34% of the variance (adjusted R² = 0.34).`,

    report: "B coefficients with 95% CI and p-values, adjusted R², overall F-test, VIF, residual diagnostics",

    jasp: `Analyses → Regression → Linear Regression

1. Outcome → "Dependent Variable"
2. Continuous predictors → "Covariates", Categorical → "Factors"
3. Statistics: Estimates + CI, R², Adj R², Collinearity (VIF)
4. Plots: Residuals vs Predicted, Q-Q plot`,

    r: `library(tidyverse)
library(gtsummary)
library(sjPlot)
library(car)
library(finalfit)

# ── Quick regression table (publication-ready) ──
model <- lm(outcome ~ exposure + age + sex + bmi, data = df)

# Option A: gtsummary
tbl_regression(model, intercept = FALSE) |>
  bold_p() |> add_glance_table(include = c(r.squared, adj.r.squared, nobs))

# Option B: sjPlot (forest plot + table)
tab_model(model, show.ci = TRUE, show.std = TRUE)
plot_model(model, type = "est")   # forest plot

# Option C: finalfit
df |> finalfit("outcome", c("exposure", "age", "sex", "bmi"))

# ── Diagnostics ──
vif(model)
par(mfrow = c(2, 2)); plot(model)`
  },

  logistic_regression: {
    name: "Logistic Regression",
    when: "Examining the relationship between a binary outcome and predictors while adjusting for confounders.",
    assumptions: "Binary outcome, independent observations, no multicollinearity, ≥ 10 events per predictor.",

    sap: `The association between [exposure] and [binary outcome] will be assessed using binary logistic regression, adjusting for [confounders]. The primary effect measure will be the adjusted odds ratio (aOR) with 95% CI. Model calibration will be assessed via Hosmer-Lemeshow test and discrimination via AUC. P-values will be interpreted alongside the magnitude and precision of the odds ratios.`,
    sapTraditional: `The association between [exposure] and [binary outcome] will be assessed using binary logistic regression, adjusting for [confounders]. Results will be expressed as adjusted odds ratios (aOR) with 95% CI. A p-value < 0.05 will be considered statistically significant.`,

    example: `After adjusting for age, diabetes duration, and smoking, insulin therapy was associated with 2.8× higher odds of glycemic control (aOR = 2.8, 95% CI: 1.4–5.6, p = 0.004; AUC = 0.73). The CI indicates at least a 40% increase in odds — a clinically relevant benefit.`,
    exampleTraditional: `Insulin therapy was significantly associated with glycemic control after adjustment (aOR = 2.8, 95% CI: 1.4–5.6, p = 0.004). The model showed acceptable fit (Hosmer-Lemeshow p = 0.74).`,

    report: "Adjusted ORs with 95% CI, Hosmer-Lemeshow, AUC, Nagelkerke R²",

    jasp: `Analyses → Regression → Logistic Regression

1. Binary outcome → "Dependent Variable"
2. Covariates + Factors as appropriate
3. Statistics: OR + CI, Pseudo R², Classification table, Hosmer-Lemeshow
4. Plots: ROC curve (if available)`,

    r: `library(tidyverse)
library(gtsummary)
library(sjPlot)
library(finalfit)
library(pROC)

model <- glm(outcome ~ exposure + age + sex + smoking,
             data = df, family = binomial)

# ── Publication-ready OR table ──
tbl_regression(model, exponentiate = TRUE) |>
  bold_p() |> add_glance_table(include = c(nobs, logLik, AIC))

# ── sjPlot forest plot ──
tab_model(model, show.ci = TRUE)
plot_model(model, type = "est", transform = "exp")

# ── finalfit (univariable + multivariable in one table) ──
df |> finalfit("outcome", c("exposure", "age", "sex", "smoking"))

# ── ROC / AUC ──
roc_obj <- roc(df$outcome, fitted(model))
auc(roc_obj)
ggroc(roc_obj) + theme_minimal()

# ── Hosmer-Lemeshow ──
library(ResourceSelection)
hoslem.test(model$y, fitted(model), g = 10)`
  },

  ordinal_logistic: {
    name: "Ordinal Logistic Regression",
    when: "Examining the relationship between an ordinal outcome (3+ ordered categories) and predictors while adjusting for confounders.",
    assumptions: "Proportional odds (effect is consistent across thresholds), independent observations, adequate sample size.",

    sap: `The association between [exposure] and [ordinal outcome] will be assessed using proportional odds logistic regression, adjusting for [confounders]. The proportional odds assumption will be tested via the Brant test. The primary effect measure will be the adjusted cumulative OR with 95% CI. If the assumption is violated, multinomial logistic regression will be used instead.`,
    sapTraditional: `The association between [exposure] and [ordinal outcome] will be assessed using ordinal logistic regression (proportional odds model), adjusting for [confounders]. The Brant test will verify the proportional odds assumption. Results will be expressed as cumulative OR with 95% CI. A p-value < 0.05 will be considered statistically significant.`,

    example: `Higher BMI was associated with greater disease severity (mild < moderate < severe): adjusted cumulative OR = 1.12 per unit BMI (95% CI: 1.05–1.20, p = 0.001; Brant test p = 0.42). Each additional BMI unit increases the odds of being in a higher severity category by ~12%.`,
    exampleTraditional: `BMI was significantly associated with disease severity on ordinal logistic regression (cumulative OR = 1.12, 95% CI: 1.05–1.20, p = 0.001). The proportional odds assumption was met (Brant test p = 0.42).`,

    report: "Adjusted cumulative ORs with 95% CI, Brant test, model fit statistics",

    jasp: `JASP does not have a built-in ordinal logistic module (as of v0.18).

Workaround: use the R integration in JASP, or use R/SPSS directly.
In SPSS: Analyze → Regression → Ordinal`,

    r: `library(MASS)
library(brant)
library(gtsummary)
library(sjPlot)

# ── Ordinal logistic (proportional odds) ──
df$outcome_ord <- factor(df$outcome,
  levels = c("Mild", "Moderate", "Severe"), ordered = TRUE)

model <- polr(outcome_ord ~ exposure + age + comorbidity,
              data = df, Hess = TRUE)

# ── Publication-ready table ──
tbl_regression(model, exponentiate = TRUE)

# ── sjPlot table ──
tab_model(model)

# ── Brant test for proportional odds ──
brant(model)

# ── If assumption violated → multinomial ──
library(nnet)
model_multi <- multinom(outcome_ord ~ exposure + age, data = df)
tbl_regression(model_multi, exponentiate = TRUE)`
  },

  // ━━━ SURVIVAL ANALYSIS ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  log_rank: {
    name: "Log-Rank Test (+ Kaplan-Meier)",
    when: "Comparing survival curves between two or more groups without adjusting for confounders.",
    assumptions: "Independent censoring, non-informative censoring.",

    sap: `Survival will be estimated using the Kaplan-Meier method. Curves will be compared using the log-rank test. Median survival with 95% CI and survival at key time points will be reported. Interpretation will focus on the separation of curves and absolute differences in survival, not solely the p-value.`,
    sapTraditional: `Survival analysis will be performed using the Kaplan-Meier method. Survival curves will be compared using the log-rank test. Median survival with 95% CI will be reported. A p-value < 0.05 will be considered statistically significant.`,

    example: `One-year survival was 82% (95% CI: 74–90%) intervention vs 65% (95% CI: 55–75%) control (log-rank χ² = 7.8, p = 0.005). The absolute 17% difference (95% CI: 4–30%) corresponds to ~1 additional survivor per 6 patients treated — a clinically important benefit.`,
    exampleTraditional: `The 1-year overall survival was significantly higher in the intervention group (82%, 95% CI: 74–90%) compared to control (65%, 95% CI: 55–75%; log-rank p = 0.005).`,

    report: "KM curves, survival rates at key time points with 95% CI, median survival with 95% CI, log-rank statistic, p-value",

    jasp: `Analyses → Survival → Kaplan-Meier (JASP 0.17+)

1. Time → "Time", Event/status → "Status" (specify event code)
2. Group → "Factor"
3. Check: Log-rank test, Median survival + CI, Survival table, Plot`,

    r: `library(survival)
library(survminer)
library(gtsummary)

# ── Kaplan-Meier ──
km <- survfit(Surv(time, event) ~ group, data = df)

# ── Publication-ready KM table ──
tbl_survfit(km, times = c(12, 24, 60),
  label_header = "**{time} months**")

# ── Log-rank test ──
survdiff(Surv(time, event) ~ group, data = df)

# ── Beautiful KM plot ──
ggsurvplot(km, data = df,
  pval = TRUE, conf.int = TRUE,
  risk.table = TRUE, risk.table.col = "strata",
  xlab = "Time (months)", ylab = "Survival probability",
  ggtheme = theme_minimal())`
  },

  cox_regression: {
    name: "Cox Proportional Hazards Regression",
    when: "Examining the effect of predictors on time-to-event data while adjusting for confounders.",
    assumptions: "Proportional hazards (constant HR over time), independent censoring, ≥ 10 events per predictor.",

    sap: `The independent effect of [exposure] on [time-to-event outcome] will be estimated using Cox proportional hazards regression, adjusting for [confounders]. The PH assumption will be tested via Schoenfeld residuals. The primary effect measure will be the adjusted hazard ratio (aHR) with 95% CI. Kaplan-Meier curves and log-rank test will provide the unadjusted analysis.`,
    sapTraditional: `Cox proportional hazards regression will be used to assess the independent effect of [exposure] on [outcome], adjusting for [confounders]. The PH assumption will be tested. Results will be expressed as adjusted hazard ratios (aHR) with 95% CI. A p-value < 0.05 will be considered statistically significant.`,

    example: `Median relapse-free survival: 14.2 months (intervention) vs 9.8 months (control). After adjusting for age and stage: aHR = 0.60, 95% CI: 0.42–0.86, p = 0.005. The true hazard reduction likely lies between 14% and 58% — a clinically meaningful range. PH assumption satisfied (Schoenfeld global p = 0.31).`,
    exampleTraditional: `After adjusting for age and disease stage, the intervention was associated with a significantly lower hazard of relapse (aHR = 0.60, 95% CI: 0.42–0.86, p = 0.005). The proportional hazards assumption was met (p = 0.31).`,

    report: "KM curves, median survival with 95% CI, log-rank, adjusted HRs with 95% CI, Schoenfeld test",

    jasp: `Analyses → Survival → Cox Proportional Hazards (JASP 0.17+)

1. Time → "Time", Event → "Status"
2. Covariates / Factors as appropriate
3. Check: HR + CI, Schoenfeld residuals test`,

    r: `library(survival)
library(survminer)
library(gtsummary)
library(sjPlot)
library(finalfit)

# ── Cox model ──
cox <- coxph(Surv(time, event) ~ group + age + stage, data = df)

# ── Publication-ready table ──
tbl_regression(cox, exponentiate = TRUE) |>
  bold_p()

# ── sjPlot forest plot ──
plot_model(cox, type = "est", transform = "exp")
tab_model(cox)

# ── finalfit (univariable + multivariable) ──
df |> finalfit("Surv(time, event)",
  explanatory = c("group", "age", "stage"))

# ── Diagnostics ──
cox.zph(cox)            # PH assumption
ggcoxzph(cox.zph(cox))  # visual check`
  },

  // ━━━ MIXED / REPEATED MEASURES ━━━━━━━━━━━━━━━━━━━━━━━

  mixed_effects: {
    name: "Linear Mixed-Effects Model",
    when: "Analyzing paired/repeated continuous data while adjusting for confounders. Accounts for within-subject correlation.",
    assumptions: "Normal residuals, linear relationship, normally distributed random effects.",

    sap: `The effect of [exposure] on [outcome] measured at [time points] will be assessed using a linear mixed-effects model with a random intercept per subject. Fixed effects: exposure, time, [confounders]. The primary effect measure will be the adjusted mean difference with 95% CI. If outcome is non-normal, a GLMM will be considered.`,
    sapTraditional: `A linear mixed-effects model with random intercept for each subject will be used to assess the effect of [exposure] on [outcome] across [time points], adjusting for [confounders]. A p-value < 0.05 will be considered statistically significant.`,

    example: `After adjusting for age and sex, the intervention group showed a greater HbA1c decline over 12 months (group × time: B = −0.8%, 95% CI: −1.2 to −0.4, p < 0.001). Adjusted mean HbA1c at 12 months: 7.1% intervention vs 7.9% control (difference: −0.8%, 95% CI: −1.3 to −0.3). Random intercept SD = 0.6% indicating moderate between-patient variability.`,
    exampleTraditional: `The group × time interaction was statistically significant (B = −0.8%, 95% CI: −1.2 to −0.4, p < 0.001), indicating a greater HbA1c decline in the intervention group.`,

    report: "Fixed-effect B with 95% CI, random-effect variances, AIC/BIC, residual diagnostics",

    jasp: `Analyses → Mixed Models → Linear Mixed Models (JASP 0.17+)

1. Outcome → "Dependent Variable"
2. Fixed-effect predictors → "Fixed Effects"
3. Subject ID → "Random Effects" (specify random intercept)
4. Check: Estimates + CI, Variance components`,

    r: `library(lme4)
library(lmerTest)   # Satterthwaite p-values
library(gtsummary)
library(sjPlot)

# ── Mixed model ──
model <- lmer(outcome ~ group * time + age + sex + (1 | subject_id),
              data = df_long)

# ── Publication-ready table ──
tbl_regression(model)

# ── sjPlot table + plots ──
tab_model(model, show.ci = TRUE, show.re.var = TRUE)
plot_model(model, type = "est")
plot_model(model, type = "pred", terms = c("time", "group"))

# ── Diagnostics ──
plot(model)
qqnorm(residuals(model)); qqline(residuals(model))

# ── Model comparison ──
model0 <- lmer(outcome ~ time + age + sex + (1 | subject_id),
               data = df_long)
anova(model0, model)   # likelihood ratio test`
  },

  gee: {
    name: "GEE (Generalized Estimating Equations)",
    when: "Analyzing paired/repeated binary or categorical outcomes while adjusting for confounders. Population-averaged approach.",
    assumptions: "Correct working correlation structure, adequate number of clusters (subjects), independent clusters.",

    sap: `The effect of [exposure] on [binary/categorical outcome] at [time points] will be assessed using GEE with a logit link and exchangeable working correlation. Robust (sandwich) standard errors will be used. The primary effect measure will be the adjusted OR with 95% CI. Sensitivity to alternative correlation structures will be checked.`,
    sapTraditional: `GEE with a logit link function and exchangeable working correlation will be used to assess the effect of [exposure] on [outcome] across time points, adjusting for [confounders]. Robust standard errors will be reported. A p-value < 0.05 will be considered statistically significant.`,

    example: `After adjusting for age and stage using GEE, the intervention was associated with 2.1× higher odds of treatment response at each visit (aOR = 2.1, 95% CI: 1.3–3.4, p = 0.002, robust SE). Working correlation = 0.45. Results were robust to alternative structures (unstructured: aOR = 2.0, AR(1): aOR = 2.2).`,
    exampleTraditional: `The intervention was significantly associated with higher odds of response across visits (aOR = 2.1, 95% CI: 1.3–3.4, p = 0.002) on GEE with exchangeable correlation.`,

    report: "Adjusted ORs with 95% CI (robust SE), working correlation, QIC, sensitivity analysis",

    jasp: `JASP does not currently have a GEE module.

Use R (see R tab) or SPSS:
SPSS: Analyze → Generalized Linear Models → Generalized Estimating Equations`,

    r: `library(geepack)
library(tidyverse)
library(broom)

# ── GEE (binary outcome, exchangeable) ──
model <- geeglm(outcome ~ group * time + age + sex,
                 id = subject_id, data = df_long,
                 family = binomial, corstr = "exchangeable")
summary(model)

# ── Tidy OR table ──
tidy(model, conf.int = TRUE, exponentiate = TRUE)

# ── Manual formatted table ──
coefs <- summary(model)$coefficients
data.frame(
  term = rownames(coefs),
  OR   = exp(coefs[, 1]),
  lower = exp(coefs[, 1] - 1.96 * coefs[, 2]),
  upper = exp(coefs[, 1] + 1.96 * coefs[, 2]),
  p    = coefs[, 4]
) |> mutate(across(c(OR, lower, upper), ~round(., 2)))

# ── Compare correlation structures ──
for (cs in c("exchangeable", "ar1", "unstructured")) {
  m <- geeglm(outcome ~ group * time + age + sex,
    id = subject_id, data = df_long,
    family = binomial, corstr = cs)
  cat(cs, ": OR =", round(exp(coef(m)["groupIntervention"]), 2), "\\n")
}`
  }
};

// ──────────────────────────────────────────────────────────
// Decision tree: maps user choices → test keys
// ──────────────────────────────────────────────────────────
export function recommend(answers) {
  const { outcome, comparison, distribution, adjust, sampleSize } = answers;
  const results = [];

  if (comparison === "single") {
    if (outcome === "continuous") results.push("one_sample_t");
    return results;
  }

  if (comparison === "correlation") {
    results.push("correlation");
    if (adjust === "yes") results.push("linear_regression");
    return results;
  }

  if (outcome === "time_to_event") {
    results.push("log_rank");
    if (adjust === "yes") results.push("cox_regression");
    return results;
  }

  if (outcome === "binary" || outcome === "categorical") {
    if (comparison === "two_paired") {
      results.push("mcnemar");
      if (adjust === "yes") results.push("gee");
    } else {
      if (sampleSize === "small") results.push("fisher");
      else results.push("chi_square");
      if (adjust === "yes") {
        results.push(outcome === "binary" ? "logistic_regression" : "ordinal_logistic");
      }
    }
    return results;
  }

  if (outcome === "continuous") {
    if (comparison === "two_independent") {
      results.push(distribution === "normal" ? "independent_t" : "mann_whitney");
      if (adjust === "yes") results.push("linear_regression");
    } else if (comparison === "two_paired") {
      results.push(distribution === "normal" ? "paired_t" : "wilcoxon");
      if (adjust === "yes") results.push("mixed_effects");
    } else if (comparison === "three_plus") {
      results.push(distribution === "normal" ? "one_way_anova" : "kruskal_wallis");
      if (adjust === "yes") results.push("linear_regression");
    }
    return results;
  }

  return results;
}

// ──────────────────────────────────────────────────────────
// Question steps
// ──────────────────────────────────────────────────────────
export const STEPS = [
  {
    id: "outcome",
    title: "What type is your outcome variable?",
    subtitle: "The variable you are trying to explain or predict",
    options: [
      { value: "continuous", label: "Continuous", desc: "BP, HbA1c, weight, age, score", icon: "📏" },
      { value: "binary", label: "Binary", desc: "Yes/No, Cured/Not cured, Dead/Alive", icon: "🔘" },
      { value: "categorical", label: "Categorical (3+ ordered)", desc: "Mild / Moderate / Severe", icon: "📊" },
      { value: "time_to_event", label: "Time-to-Event", desc: "Survival time, time to relapse", icon: "⏱" },
    ],
  },
  {
    id: "comparison",
    title: "What is your comparison structure?",
    subtitle: "How are your groups or variables arranged?",
    options: [
      { value: "two_independent", label: "2 Independent Groups", desc: "Treatment vs Control, Exposed vs Unexposed", icon: "👥" },
      { value: "two_paired", label: "2 Paired / Before-After", desc: "Same subjects measured twice", icon: "🔄" },
      { value: "three_plus", label: "3+ Independent Groups", desc: "Multiple treatment arms, severity grades", icon: "👥👥" },
      { value: "correlation", label: "Association (2 variables)", desc: "Correlation between two measurements", icon: "📈" },
      { value: "single", label: "Single Group (vs reference)", desc: "Compare sample mean to a known value", icon: "1️⃣" },
    ],
  },
  {
    id: "distribution",
    title: "Is your outcome normally distributed?",
    subtitle: "Check with histogram, Q-Q plot, or Shapiro-Wilk test",
    show: (a) => a.outcome === "continuous" && a.comparison !== "single" && a.comparison !== "correlation",
    options: [
      { value: "normal", label: "Yes, Normal", desc: "Bell-shaped, Shapiro-Wilk p > 0.05", icon: "📐" },
      { value: "skewed", label: "No, Skewed", desc: "Non-normal, Shapiro-Wilk p < 0.05", icon: "📉" },
    ],
  },
  {
    id: "adjust",
    title: "Do you need to adjust for confounders?",
    subtitle: "Multivariable analysis to control for other variables",
    show: (a) => a.comparison !== "single",
    options: [
      { value: "no", label: "No", desc: "Simple unadjusted comparison", icon: "✅" },
      { value: "yes", label: "Yes", desc: "Adjusted analysis (regression)", icon: "⚙️" },
    ],
  },
  {
    id: "sampleSize",
    title: "What is your approximate sample size per group?",
    subtitle: "This affects the choice between certain tests",
    show: (a) =>
      (a.outcome === "binary" || a.outcome === "categorical") &&
      a.comparison !== "two_paired" &&
      a.comparison !== "single" &&
      a.comparison !== "correlation",
    options: [
      { value: "small", label: "Small (< 30/group)", desc: "Or expected cell frequency < 5", icon: "🔬" },
      { value: "large", label: "Moderate–Large (≥ 30)", desc: "Expected cell frequencies ≥ 5", icon: "🏥" },
    ],
  },
];
