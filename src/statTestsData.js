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

  stuart_maxwell: {
    name: "Stuart-Maxwell Test (Marginal Homogeneity)",
    when: "Comparing paired proportions across 3+ ordered categories — e.g., disease severity before/after treatment in the same subjects.",
    assumptions: "Paired ordinal data (same subjects measured at two time points), 3+ ordered categories.",

    sap: `The change in distribution of [outcome] (3+ ordered categories) before and after [intervention] will be assessed using the Stuart-Maxwell test of marginal homogeneity. This extends McNemar's test to square tables larger than 2×2. The pattern of shifts across categories will be described using a transition matrix.`,
    sapTraditional: `The change in distribution of [outcome] before and after [intervention] will be assessed using the Stuart-Maxwell test of marginal homogeneity. Results will be expressed as frequencies (%) at each time point. A two-sided p-value < 0.05 will be considered statistically significant.`,

    example: `Disease severity shifted after treatment: Mild 20%→45%, Moderate 50%→35%, Severe 30%→20% (Stuart-Maxwell χ² = 12.4, df = 2, p = 0.002). The transition matrix shows 65% of Severe patients improved to Moderate or Mild — a clinically meaningful shift.`,
    exampleTraditional: `The distribution of disease severity changed significantly after treatment (Stuart-Maxwell test, χ² = 12.4, df = 2, p = 0.002).`,

    report: "Frequencies (%) per category at each time point, transition matrix, Stuart-Maxwell χ², df, p-value",

    jasp: `JASP does not have a built-in Stuart-Maxwell / marginal homogeneity test.

Use R (see R tab) or SPSS:
SPSS: Analyze → Nonparametric Tests → Related Samples → select Marginal Homogeneity`,

    r: `library(coin)

# ── Data: paired ordinal categories (before/after) ──
# df should have: subject_id, before (factor), after (factor)
df$before <- factor(df$before,
  levels = c("Mild", "Moderate", "Severe"), ordered = TRUE)
df$after  <- factor(df$after,
  levels = c("Mild", "Moderate", "Severe"), ordered = TRUE)

# ── Stuart-Maxwell / Marginal Homogeneity test ──
mh_test(before ~ after, data = df)

# ── Transition matrix ──
tab <- table(Before = df$before, After = df$after)
print(tab)
prop.table(tab, margin = 1) |> round(2)  # row %

# ── Descriptive summary ──
tibble(
  time  = c(rep("Before", nrow(df)), rep("After", nrow(df))),
  level = c(as.character(df$before), as.character(df$after))
) |> count(time, level) |>
  group_by(time) |> mutate(pct = round(n / sum(n) * 100, 1))`
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
      results.push(outcome === "binary" ? "mcnemar" : "stuart_maxwell");
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
      { value: "correlation", label: "Association (2 variables)", desc: "Correlation between two measurements", icon: "📈", showWhen: (a) => a.outcome === "continuous" },
      { value: "single", label: "Single Group (vs reference)", desc: "Compare sample mean to a known value", icon: "1️⃣", showWhen: (a) => a.outcome === "continuous" },
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

// ━━━ DESCRIPTIVE STATISTICS ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

export const DESCRIPTIVES = {
  continuous_normal: {
    name: "Continuous Variable (Normal)",
    summary: "Mean ± SD",
    visual: "Histogram with normal curve, Box plot",
    sap: `Continuous variables that are normally distributed will be summarised as mean ± standard deviation (SD). Normality will be assessed using the Shapiro-Wilk test and visual inspection of histograms and Q-Q plots. For skewed variables, median (interquartile range, IQR) will be used instead (see below).`,
    sapTraditional: `Normally distributed continuous variables will be expressed as mean ± SD. Normality will be assessed using the Shapiro-Wilk test. Non-normal variables will be expressed as median (IQR).`,
    jasp: `For descriptive tables:\nAnalyses → Descriptives → Descriptive Statistics\n\n1. Move continuous variables into "Variables"\n2. Under "Statistics," check:\n   • Central Tendency: Mean, Median\n   • Dispersion: Std. Deviation, IQR, Range, Minimum, Maximum\n   • Distribution: Skewness, Kurtosis\n3. Under "Plots," check:\n   • Distribution plots (histogram)\n   • Box plots\n   • Q-Q plots`,
    r: `library(tidyverse)
library(gtsummary)

# ── Publication-ready Table 1 ──
df |>
  select(age, bmi, sbp, hba1c) |>
  tbl_summary(
    type = everything() ~ "continuous",
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 1
  ) |>
  add_n() |>
  modify_header(label = "**Variable**")

# ── Normality check ──
df |>
  select(where(is.numeric)) |>
  pivot_longer(everything()) |>
  group_by(name) |>
  summarise(
    shapiro_p = shapiro.test(value)$p.value,
    skewness  = e1071::skewness(value),
    .groups   = "drop"
  )

# ── Histogram + Q-Q plot ──
ggplot(df, aes(x = outcome)) +
  geom_histogram(aes(y = after_stat(density)),
    bins = 20, fill = "steelblue", alpha = 0.7) +
  geom_density(colour = "red") +
  theme_minimal()

ggplot(df, aes(sample = outcome)) +
  stat_qq() + stat_qq_line(colour = "red") +
  theme_minimal()`
  },

  continuous_skewed: {
    name: "Continuous Variable (Skewed / Non-Normal)",
    summary: "Median (IQR) or Median (P25–P75)",
    visual: "Histogram, Box plot, Violin plot",
    sap: `Continuous variables that are not normally distributed will be summarised as median (interquartile range, IQR: 25th–75th percentile). Range (minimum–maximum) will also be reported. For variables with extreme outliers, the median is preferred as it is robust to outlying values.`,
    sapTraditional: `Non-normally distributed continuous variables will be expressed as median (IQR). Range will also be reported where relevant.`,
    jasp: `Analyses → Descriptives → Descriptive Statistics\n\n1. Move variables into "Variables"\n2. Under "Statistics," check:\n   • Central Tendency: Median\n   • Dispersion: IQR, Range, Minimum, Maximum\n   • Percentile Values: Quartiles (25th, 75th)\n3. Under "Plots," check Box plots and Distribution plots`,
    r: `library(tidyverse)
library(gtsummary)

# ── Table with median (IQR) ──
df |>
  select(hospital_stay, pain_score, cost) |>
  tbl_summary(
    type = everything() ~ "continuous",
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    digits = all_continuous() ~ 1
  )

# ── Box plot + violin ──
ggplot(df, aes(x = group, y = outcome)) +
  geom_violin(fill = "lightblue", alpha = 0.5) +
  geom_boxplot(width = 0.2, fill = "white") +
  theme_minimal()`
  },

  categorical: {
    name: "Categorical / Binary Variable",
    summary: "Frequency (n) and Percentage (%)",
    visual: "Bar chart, Pie chart (≤ 5 categories)",
    sap: `Categorical variables will be summarised as frequency and percentage (%). Binary variables will be reported as the count and proportion of the outcome of interest. Percentages will be calculated over non-missing values; the number of missing observations will be noted separately.`,
    sapTraditional: `Categorical variables will be expressed as frequency and percentage (%).`,
    jasp: `Analyses → Descriptives → Descriptive Statistics\n\n1. Move categorical variables into "Variables"\n2. Under "Statistics," check:\n   • Frequency tables: On\n3. Under "Plots," check:\n   • Bar plots\n   • Pie charts (for ≤ 5 categories)\n\nAlternatively:\nAnalyses → Frequencies → Contingency Tables\nfor cross-tabulations of two categorical variables`,
    r: `library(tidyverse)
library(gtsummary)

# ── Frequency table ──
df |>
  select(gender, smoking, disease_stage) |>
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ c(0, 1)
  )

# ── Bar chart ──
ggplot(df, aes(x = fct_infreq(disease_stage), fill = disease_stage)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5) +
  labs(x = "Disease Stage", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")`
  },

  table_one: {
    name: "Baseline / Table 1 (by group)",
    summary: "Stratified summary of all baseline variables by study group",
    visual: "Publication-ready Table 1",
    sap: `Baseline characteristics will be summarised by study group. Continuous variables will be expressed as mean ± SD (if normally distributed) or median (IQR) (if skewed). Categorical variables will be expressed as frequency (%). The standardised mean difference (SMD) or p-values from appropriate bivariate tests may be reported to characterise balance across groups.`,
    sapTraditional: `Baseline characteristics will be presented as Table 1, stratified by group. Continuous variables will be expressed as mean ± SD or median (IQR), and categorical variables as n (%). P-values from bivariate tests will indicate differences between groups.`,
    jasp: `JASP does not generate a combined Table 1 directly.\n\nWorkaround:\n1. Analyses → Descriptives → Descriptive Statistics\n   • Split by your grouping variable\n   • Check all relevant statistics\n2. Combine with Contingency Tables for categorical variables\n\nFor a true Table 1, use R (gtsummary) — see the R tab.`,
    r: `library(gtsummary)
library(tidyverse)

# ── The classic Table 1 ──
df |>
  select(group, age, sex, bmi, smoking, disease_stage,
         sbp, hba1c) |>
  tbl_summary(
    by = group,
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "ifany"        # show missing counts
  ) |>
  add_p() |>               # bivariate p-values
  add_overall() |>          # total column
  add_n() |>                # sample size per variable
  modify_header(label = "**Characteristic**") |>
  bold_labels()

# ── With SMD instead of p-values ──
library(tableone)
vars <- c("age", "sex", "bmi", "smoking", "disease_stage")
CreateTableOne(vars = vars, strata = "group",
  data = df, test = FALSE, smd = TRUE)

# ── Export to Word/Excel ──
tbl <- df |> tbl_summary(by = group) |> add_p()
# To Word:
tbl |> as_flex_table() |> flextable::save_as_docx(path = "table1.docx")
# To Excel:
tbl |> as_tibble() |> writexl::write_xlsx("table1.xlsx")`
  }
};

export const DESCRIPTIVE_STEPS = [
  {
    id: "desc_goal",
    title: "What do you want to summarise?",
    subtitle: "Pick the type of descriptive analysis you need",
    options: [
      { value: "single_continuous", label: "A continuous variable", desc: "BP, HbA1c, weight, hospital stay, score", icon: "desc_continuous" },
      { value: "single_categorical", label: "A categorical / binary variable", desc: "Gender, disease stage, Yes/No outcome", icon: "desc_categorical" },
      { value: "table_one", label: "Baseline table (Table 1)", desc: "Summarise all variables by study group", icon: "table" },
    ],
  },
  {
    id: "desc_distribution",
    title: "Is your continuous variable normally distributed?",
    subtitle: "Check with histogram, Q-Q plot, or Shapiro-Wilk test",
    show: (a) => a.desc_goal === "single_continuous",
    options: [
      { value: "normal", label: "Yes, Normal", desc: "Bell-shaped, Shapiro-Wilk p > 0.05", icon: "normal" },
      { value: "skewed", label: "No, Skewed", desc: "Non-normal, Shapiro-Wilk p < 0.05", icon: "skewed" },
      { value: "not_sure", label: "Not sure yet", desc: "I'll check — show me how to assess normality", icon: "not_sure" },
    ],
  },
];

export function explainReasoning(answers) {
  const { outcome, comparison, distribution, adjust, sampleSize } = answers;
  const parts = [];

  const outcomeLabels = { continuous: "continuous", binary: "binary", categorical: "ordered categorical (3+ levels)", time_to_event: "time-to-event (survival)" };
  const compLabels = { two_independent: "two independent groups", two_paired: "paired/before-after measurements", three_plus: "three or more independent groups", correlation: "association between two variables", single: "a single group vs. a known reference" };

  if (outcome) parts.push(`Your outcome is **${outcomeLabels[outcome] || outcome}**`);
  if (comparison) parts.push(`with **${compLabels[comparison] || comparison}**`);
  if (distribution === "normal") parts.push("and the data is **normally distributed**");
  if (distribution === "skewed") parts.push("and the data is **not normally distributed**");
  if (sampleSize === "small") parts.push("with a **small sample size** (expected cell count < 5)");
  if (adjust === "yes") parts.push("You also need to **adjust for confounders**, so a regression model is added.");
  else if (adjust === "no") parts.push("No confounder adjustment is needed.");

  return parts.join(", ").replace(/,([^,]*)$/, ".$1") || "";
}

export function recommendDescriptive(answers) {
  const { desc_goal, desc_distribution } = answers;
  if (desc_goal === "table_one") return ["table_one"];
  if (desc_goal === "single_categorical") return ["categorical"];
  if (desc_goal === "single_continuous") {
    if (desc_distribution === "normal") return ["continuous_normal"];
    if (desc_distribution === "skewed") return ["continuous_skewed"];
    // "not_sure" → show both so they can decide after checking
    return ["continuous_normal", "continuous_skewed"];
  }
  return [];
}

// ━━━ DIAGNOSTIC ACCURACY ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

export const DIAGNOSTIC_TESTS = {

  diagnostic_2x2: {
    name: "Diagnostic Accuracy (2×2 Table)",
    when: "Evaluating the performance of a binary diagnostic test (positive/negative) against a reference standard (gold standard).",
    assumptions: "Independent observations, a valid reference standard applied to all subjects (no verification bias), representative disease spectrum.",

    sap: `The diagnostic accuracy of [index test] will be evaluated against [reference standard]. Sensitivity, specificity, positive predictive value (PPV), negative predictive value (NPV), positive likelihood ratio (LR+), and negative likelihood ratio (LR−) will be calculated from the 2×2 contingency table with exact 95% confidence intervals (Clopper-Pearson method). The overall accuracy (proportion correctly classified) will also be reported. Results will be presented following the STARD 2015 reporting guidelines.`,
    sapTraditional: `The diagnostic accuracy of [index test] will be evaluated against [reference standard]. Sensitivity, specificity, positive predictive value (PPV), negative predictive value (NPV), positive likelihood ratio (LR+), and negative likelihood ratio (LR−) will be calculated with 95% confidence intervals. A p-value < 0.05 will be considered statistically significant where applicable.`,

    example: `The rapid antigen test had a sensitivity of 87.5% (95% CI: 78.2–93.8) and specificity of 92.3% (95% CI: 86.4–96.2) against RT-PCR as the reference standard. The PPV was 84.0% (95% CI: 74.1–91.2) and NPV was 94.1% (95% CI: 88.7–97.4). The LR+ was 11.4 (95% CI: 6.4–20.1) and LR− was 0.14 (95% CI: 0.07–0.24), indicating strong diagnostic utility. Overall accuracy was 90.6% (210/232).`,
    exampleTraditional: `The rapid antigen test demonstrated sensitivity of 87.5% (95% CI: 78.2–93.8%) and specificity of 92.3% (95% CI: 86.4–96.2%) when compared against RT-PCR. The PPV was 84.0% and NPV was 94.1%. The LR+ was 11.4 and LR− was 0.14.`,

    report: "2×2 table, Sensitivity with 95% CI, Specificity with 95% CI, PPV, NPV, LR+, LR−, Overall accuracy, STARD flow diagram",

    jasp: `JASP does not have a built-in diagnostic accuracy module.

Use R or an online calculator (e.g., MedCalc, OpenEpi) instead.

For 2×2 tables in JASP:
1. Frequencies → Contingency Tables
2. Enter your test result vs reference standard
3. This gives chi-square and odds ratio but NOT
   sensitivity/specificity/PPV/NPV directly.

→ Recommended: Use R code provided or an online
  diagnostic accuracy calculator.`,

    r: `library(caret)
library(epiR)

# ── Create 2×2 table ──
# Rows = Test result (Positive, Negative)
# Cols = Reference (Disease+, Disease−)
# IMPORTANT: epiR expects this exact layout
tab <- matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE,
              dimnames = list(
                Test = c("Positive", "Negative"),
                Reference = c("Disease+", "Disease-")))

# ── Diagnostic accuracy with 95% CI ──
epi.tests(as.table(tab), conf.level = 0.95)
# Returns: Se, Sp, PPV, NPV, LR+, LR-, DOR, accuracy

# ── Alternative: manual calculation ──
TP <- tab[1,1]; FP <- tab[1,2]
FN <- tab[2,1]; TN <- tab[2,2]

sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
lr_pos <- sensitivity / (1 - specificity)
lr_neg <- (1 - sensitivity) / specificity
accuracy <- (TP + TN) / (TP + FP + FN + TN)

# ── Exact CIs (Clopper-Pearson) ──
binom.test(TP, TP + FN)$conf.int  # Sensitivity CI
binom.test(TN, TN + FP)$conf.int  # Specificity CI`
  },

  roc_auc: {
    name: "ROC Curve & AUC Analysis",
    when: "Evaluating the discriminative ability of a continuous or ordinal test variable (e.g., biomarker, score) for distinguishing between two groups (disease vs. no disease).",
    assumptions: "Binary outcome (disease present/absent), independent observations, adequate sample in both groups.",

    sap: `The discriminative ability of [biomarker/score] for [condition] will be assessed using Receiver Operating Characteristic (ROC) curve analysis. The Area Under the ROC Curve (AUC) with 95% confidence interval (DeLong method) will be the primary measure of discrimination. The optimal cutoff will be determined using Youden's index (J = Sensitivity + Specificity − 1), and sensitivity, specificity, PPV, NPV, and likelihood ratios at this cutoff will be reported. If multiple markers are evaluated, AUC values will be compared using the DeLong test for correlated ROC curves.`,
    sapTraditional: `The discriminative ability of [biomarker/score] for [condition] will be assessed using Receiver Operating Characteristic (ROC) curve analysis. The Area Under the Curve (AUC) with 95% confidence interval will be calculated. The optimal cutoff value will be determined using Youden's index. Sensitivity and specificity at the optimal cutoff will be reported. AUC > 0.7 will be considered acceptable discrimination. A p-value < 0.05 will be considered statistically significant.`,

    example: `Serum ferritin showed good discrimination for iron deficiency anaemia with an AUC of 0.87 (95% CI: 0.81–0.93, p < 0.001). The optimal cutoff was 30 ng/mL (Youden's index = 0.68), yielding sensitivity of 88.2% (95% CI: 79.4–94.2), specificity of 79.5% (95% CI: 71.0–86.5), PPV of 76.9%, and NPV of 89.7%. The LR+ of 4.3 indicates that a ferritin ≤ 30 ng/mL is 4.3 times more likely in patients with iron deficiency anaemia than without.`,
    exampleTraditional: `The AUC for serum ferritin in diagnosing iron deficiency anaemia was 0.87 (95% CI: 0.81–0.93), which was statistically significant (p < 0.001). At the optimal cutoff of 30 ng/mL (Youden's index), the sensitivity was 88.2% and specificity was 79.5%.`,

    report: "ROC curve figure, AUC with 95% CI and p-value, optimal cutoff (Youden's), Sensitivity & Specificity at cutoff, PPV, NPV, LR+, LR−",

    jasp: `JASP does not currently have a built-in ROC analysis module.

→ Use R (pROC package) for ROC analysis.

Alternative free tools:
• MedCalc (free trial): Diagnostic test evaluation
• EasyROC (web-based): https://www.biosoft.hacettepe.edu.tr/easyROC/
• OpenEpi: Open-source epidemiologic calculators`,

    r: `library(pROC)
library(cutpointr)
library(ggplot2)

# ── ROC analysis ──
roc_obj <- roc(df$disease_status, df$biomarker,
               levels = c("Negative", "Positive"),
               direction = "<")  # lower values = negative

# ── AUC with 95% CI (DeLong) ──
auc(roc_obj)
ci.auc(roc_obj, conf.level = 0.95)

# ── Optimal cutoff (Youden's index) ──
coords(roc_obj, "best", best.method = "youden",
       ret = c("threshold", "sensitivity", "specificity",
               "ppv", "npv"))

# ── Alternative: cutpointr (tidy approach) ──
cp <- cutpointr(df, biomarker, disease_status,
                method = maximize_metric,
                metric = youden)
summary(cp)

# ── Plot ROC curve ──
ggroc(roc_obj, colour = "steelblue", linewidth = 1.2) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
               linetype = "dashed", colour = "grey50") +
  annotate("text", x = 0.3, y = 0.2,
           label = paste0("AUC = ", round(auc(roc_obj), 3))) +
  theme_minimal() +
  labs(title = "ROC Curve", x = "Specificity", y = "Sensitivity")

# ── Compare two ROC curves (DeLong test) ──
roc1 <- roc(df$disease, df$marker1)
roc2 <- roc(df$disease, df$marker2)
roc.test(roc1, roc2, method = "delong")`
  }
};

export const DIAGNOSTIC_STEPS = [
  {
    id: "diag_test_type",
    title: "What type is your diagnostic test result?",
    subtitle: "The test you are evaluating against a reference/gold standard",
    options: [
      { value: "binary", label: "Binary (Positive / Negative)", desc: "Rapid test, culture result, clinical sign present/absent", icon: "🔘" },
      { value: "continuous", label: "Continuous / Score", desc: "Biomarker level, risk score, lab value with numeric range", icon: "📏" },
    ],
  },
];

export function recommendDiagnostic(answers) {
  const { diag_test_type } = answers;
  if (diag_test_type === "binary") return ["diagnostic_2x2"];
  if (diag_test_type === "continuous") return ["roc_auc"];
  return [];
}

export function explainDiagnosticReasoning(answers) {
  const { diag_test_type } = answers;
  if (diag_test_type === "binary") {
    return "Your diagnostic test gives a **binary result** (positive/negative). You need a **2×2 diagnostic accuracy table** with sensitivity, specificity, PPV, NPV, and likelihood ratios against your reference standard.";
  }
  if (diag_test_type === "continuous") {
    return "Your diagnostic test gives a **continuous value** (e.g., biomarker level, score). You need **ROC curve analysis** to determine the AUC (discrimination ability) and the optimal cutoff using Youden's index, along with sensitivity and specificity at that cutoff.";
  }
  return "";
}

// ━━━ AGREEMENT / RELIABILITY ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

export const AGREEMENT_TESTS = {

  cohen_kappa: {
    name: "Cohen's Kappa (Unweighted)",
    when: "Measuring agreement between two raters on a nominal/categorical variable (e.g., both raters classify samples as positive/negative, or into diagnostic categories).",
    assumptions: "Two raters, same set of subjects, nominal (unordered) categories, categories are mutually exclusive and exhaustive.",

    sap: `Inter-rater agreement between [rater 1] and [rater 2] for [classification] will be assessed using Cohen's kappa (κ). The kappa coefficient with 95% confidence interval will be reported and interpreted as: < 0.20 poor, 0.21–0.40 fair, 0.41–0.60 moderate, 0.61–0.80 substantial, and 0.81–1.00 almost perfect agreement (Landis & Koch, 1977). The observed agreement (proportion of concordant classifications) will also be reported.`,
    sapTraditional: `Inter-rater agreement between [rater 1] and [rater 2] for [classification] will be assessed using Cohen's kappa (κ) with 95% confidence interval. Kappa values will be interpreted as: < 0.20 poor, 0.21–0.40 fair, 0.41–0.60 moderate, 0.61–0.80 substantial, and 0.81–1.00 almost perfect agreement. A p-value < 0.05 will be considered statistically significant.`,

    example: `Two pathologists independently classified 120 biopsy specimens as benign, atypical, or malignant. The observed agreement was 82.5% (99/120). Cohen's kappa was 0.72 (95% CI: 0.61–0.83, p < 0.001), indicating substantial inter-rater agreement. Disagreements were most frequent between benign and atypical categories (14/21 discordant cases).`,
    exampleTraditional: `The inter-rater agreement between the two pathologists was substantial, with a Cohen's kappa of 0.72 (95% CI: 0.61–0.83, p < 0.001). The observed agreement was 82.5%.`,

    report: "Observed agreement (%), Expected agreement (%), Kappa with 95% CI, p-value, Interpretation (Landis & Koch scale), Cross-tabulation of rater classifications",

    jasp: `Frequencies → Contingency Tables

1. Enter Rater 1 classification as Rows
2. Enter Rater 2 classification as Columns
3. Under Statistics, check:
   • Nominal: Kappa (Cohen's kappa)
4. The output will show:
   • Cross-tabulation
   • Kappa coefficient with standard error
   • Approximate significance`,

    r: `library(irr)
library(psych)
library(vcd)

# ── Data setup: two raters, same subjects ──
# ratings should be a data frame with columns: rater1, rater2
# containing factor/character values

# ── Cohen's kappa ──
kappa2(ratings[, c("rater1", "rater2")])
# Returns: kappa, z-statistic, p-value

# ── With 95% CI (psych package) ──
cohen.kappa(table(ratings$rater1, ratings$rater2))
# Returns: kappa, weighted kappa, CIs

# ── Cross-tabulation ──
tab <- table(Rater1 = ratings$rater1, Rater2 = ratings$rater2)
print(tab)

# ── Observed agreement ──
sum(diag(tab)) / sum(tab)

# ── Visualise agreement ──
library(ggplot2)
ggplot(ratings, aes(x = rater1, y = rater2)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Agreement Plot", x = "Rater 1", y = "Rater 2")`
  },

  weighted_kappa: {
    name: "Weighted Kappa",
    when: "Measuring agreement between two raters on an ordinal variable (e.g., severity grades: mild/moderate/severe, Likert scales), where some disagreements are more serious than others.",
    assumptions: "Two raters, same set of subjects, ordinal (ordered) categories. Linear or quadratic weights penalise distant disagreements more heavily.",

    sap: `Inter-rater agreement for [ordinal classification] will be assessed using weighted kappa (κw) with quadratic weights, which penalises larger disagreements proportionally more than minor ones. The weighted kappa with 95% confidence interval will be reported and interpreted using the Landis and Koch scale. The unweighted kappa will also be reported for comparison. A cross-tabulation of ratings will be presented.`,
    sapTraditional: `Inter-rater agreement for [ordinal classification] will be assessed using weighted kappa with quadratic weights and 95% confidence interval. Kappa values will be interpreted as: < 0.20 poor, 0.21–0.40 fair, 0.41–0.60 moderate, 0.61–0.80 substantial, and 0.81–1.00 almost perfect agreement. A p-value < 0.05 will be considered statistically significant.`,

    example: `Two radiologists graded 95 knee MRI scans on a 4-point osteoarthritis severity scale (none/mild/moderate/severe). The weighted kappa (quadratic weights) was 0.78 (95% CI: 0.68–0.88), indicating substantial agreement. The unweighted kappa was 0.64. Most disagreements were between adjacent categories (mild vs. moderate: 12/18 discordant cases), with no case differing by more than one grade.`,
    exampleTraditional: `The weighted kappa (quadratic weights) for osteoarthritis grading between the two radiologists was 0.78 (95% CI: 0.68–0.88, p < 0.001), indicating substantial agreement. The unweighted kappa was 0.64.`,

    report: "Cross-tabulation, Weighted kappa (quadratic) with 95% CI, Unweighted kappa for comparison, Observed agreement (%), Weight matrix used",

    jasp: `Frequencies → Contingency Tables

1. Enter Rater 1 as Rows, Rater 2 as Columns
2. Under Statistics:
   • Ordinal: Gamma (related but not identical)
3. Note: JASP does not directly compute weighted kappa.

→ Use R (irr or psych package) for weighted kappa.
   JASP can still visualise the cross-tabulation.`,

    r: `library(irr)
library(psych)

# ── Data: two raters, ordinal categories ──
# Ensure categories are ordered factors
ratings$rater1 <- factor(ratings$rater1,
  levels = c("none", "mild", "moderate", "severe"),
  ordered = TRUE)
ratings$rater2 <- factor(ratings$rater2,
  levels = c("none", "mild", "moderate", "severe"),
  ordered = TRUE)

# ── Weighted kappa (quadratic weights) ──
kappa2(ratings[, c("rater1", "rater2")], weight = "squared")
# Returns: weighted kappa, z, p-value

# ── Both weighted and unweighted (psych) ──
tab <- table(ratings$rater1, ratings$rater2)
cohen.kappa(tab)
# Returns: unweighted kappa, weighted kappa, CIs

# ── Cross-tabulation ──
print(tab)`
  },

  icc: {
    name: "Intraclass Correlation Coefficient (ICC)",
    when: "Measuring agreement or consistency between two or more raters (or methods) for a continuous measurement. Used for inter-rater reliability, test-retest reliability, or comparing measurement instruments.",
    assumptions: "Continuous outcome, normally distributed measurements, same set of subjects rated by all raters.",

    sap: `The reliability of [measurement] across [raters/methods/time points] will be assessed using the Intraclass Correlation Coefficient (ICC). A two-way mixed-effects model with absolute agreement and single measures (ICC(3,1)) will be used for inter-rater reliability [or ICC(2,1) for random raters, or ICC(1,1) for test-retest with different raters]. The ICC with 95% confidence interval will be reported and interpreted as: < 0.50 poor, 0.50–0.75 moderate, 0.75–0.90 good, and > 0.90 excellent reliability (Koo & Li, 2016).`,
    sapTraditional: `The reliability of [measurement] across [raters/methods/time points] will be assessed using the Intraclass Correlation Coefficient (ICC) with 95% confidence interval. ICC values will be interpreted as: < 0.50 poor, 0.50–0.75 moderate, 0.75–0.90 good, and > 0.90 excellent reliability. A p-value < 0.05 will be considered statistically significant.`,

    example: `The ICC for tumour diameter measurement across three radiologists was 0.91 (95% CI: 0.86–0.95, F(79, 158) = 11.4, p < 0.001), indicating excellent inter-rater reliability. The mean difference between any two raters did not exceed 1.2 mm, which is within the clinically acceptable range of ± 2 mm.`,
    exampleTraditional: `The Intraclass Correlation Coefficient for tumour diameter measurement across three radiologists was 0.91 (95% CI: 0.86–0.95, p < 0.001), indicating excellent inter-rater reliability.`,

    report: "ICC value with 95% CI, F-statistic and p-value, ICC model used (one-way/two-way, random/mixed, single/average, consistency/agreement), Interpretation (Koo & Li scale)",

    jasp: `Reliability → Intraclass Correlation

1. Move all rater columns into "Variables"
2. Select ICC type:
   • Two-way mixed, absolute agreement → ICC(3,1)
     (most common for reliability studies)
   • Two-way random, absolute agreement → ICC(2,1)
     (raters are a random sample from population)
3. Check: 95% Confidence Interval
4. Output: ICC, CI, F-test`,

    r: `library(irr)
library(psych)

# ── Data format: rows = subjects, columns = raters ──
# Each cell = measurement by that rater for that subject
ratings_matrix <- df[, c("rater1", "rater2", "rater3")]

# ── ICC using irr package ──
icc(ratings_matrix,
    model = "twoway",    # "oneway" or "twoway"
    type = "agreement",  # "agreement" or "consistency"
    unit = "single")     # "single" or "average"
# Returns: ICC, F-test, p-value, CI

# ── ICC using psych package (all forms at once) ──
ICC(ratings_matrix)
# Returns: ICC1, ICC2, ICC3, ICC1k, ICC2k, ICC3k
# with F-tests and confidence intervals

# ── Quick guide to ICC selection ──
# ICC(1,1) = one-way random, single rater
# ICC(2,1) = two-way random, absolute agreement, single
# ICC(3,1) = two-way mixed, absolute agreement, single
# ICC(2,k) = two-way random, absolute agreement, average
# ICC(3,k) = two-way mixed, consistency, average`
  },

  bland_altman: {
    name: "Bland-Altman Analysis (Method Comparison)",
    when: "Comparing two measurement methods or instruments that measure the same continuous quantity (e.g., comparing a new BP device with a standard mercury sphygmomanometer). Determines whether two methods agree well enough to be used interchangeably.",
    assumptions: "Two methods measuring the same quantity on the same subjects, continuous measurements, differences approximately normally distributed.",

    sap: `Agreement between [method A] and [method B] for measuring [outcome] will be assessed using Bland-Altman analysis. The mean difference (bias) between methods with 95% limits of agreement (mean difference ± 1.96 × SD of differences) will be calculated. A Bland-Altman plot will display the differences against the average of the two methods, with horizontal lines for bias and limits of agreement. Proportional bias will be assessed by regressing the difference on the mean. Clinical acceptability of agreement will be judged against a predefined tolerance of [± X units].`,
    sapTraditional: `Agreement between [method A] and [method B] for measuring [outcome] will be assessed using Bland-Altman analysis. The mean difference (bias) with 95% limits of agreement (LoA) will be calculated. A Bland-Altman plot will be constructed. Agreement will be considered acceptable if the 95% LoA fall within the a priori defined clinically acceptable range of [± X units].`,

    example: `Bland-Altman analysis comparing the automated device with mercury sphygmomanometer for systolic BP showed a mean bias of −2.1 mmHg (95% CI: −3.4 to −0.8), indicating the automated device reads slightly lower on average. The 95% limits of agreement were −14.3 to 10.1 mmHg. The Bland-Altman plot showed no evidence of proportional bias (slope = 0.02, p = 0.74). Given the predefined clinical tolerance of ± 10 mmHg, the limits of agreement slightly exceed this threshold, suggesting the two methods are not fully interchangeable for individual patients, though the systematic bias is small.`,
    exampleTraditional: `The mean difference (bias) between the automated device and mercury sphygmomanometer was −2.1 mmHg (SD = 6.2). The 95% limits of agreement were −14.3 to 10.1 mmHg. The Bland-Altman plot showed no significant proportional bias.`,

    report: "Mean difference (bias) with 95% CI, SD of differences, 95% Limits of Agreement, Bland-Altman plot, Assessment of proportional bias (regression), Comparison with predefined clinical tolerance",

    jasp: `JASP does not have a built-in Bland-Altman module.

→ Use R for Bland-Altman analysis (BlandAltmanLeh or
  blandr package).

Workaround in JASP:
1. Compute new variable: difference = method1 − method2
2. Compute new variable: average = (method1 + method2) / 2
3. Descriptives → get mean and SD of difference
4. Scatter plot: average (x) vs difference (y)
   Then manually add reference lines for bias and LoA.`,

    r: `library(BlandAltmanLeh)
library(ggplot2)

# ── Basic Bland-Altman ──
ba <- bland.altman.stats(df$method1, df$method2,
                          conf.int = 0.95)
print(ba)
# Returns: mean.diffs (bias), lower.limit, upper.limit,
#          sd.diffs, lines (for plotting)

# ── Bland-Altman plot (publication quality) ──
ba_plot <- bland.altman.plot(df$method1, df$method2,
  main = "Bland-Altman Plot",
  xlab = "Mean of Two Methods",
  ylab = "Difference (Method 1 − Method 2)",
  conf.int = 0.95)

# ── Manual approach (more control) ──
df$diff <- df$method1 - df$method2
df$avg  <- (df$method1 + df$method2) / 2

bias <- mean(df$diff)
sd_diff <- sd(df$diff)
upper_loa <- bias + 1.96 * sd_diff
lower_loa <- bias - 1.96 * sd_diff

ggplot(df, aes(x = avg, y = diff)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = bias, colour = "blue") +
  geom_hline(yintercept = c(upper_loa, lower_loa),
             colour = "red", linetype = "dashed") +
  annotate("text", x = Inf, y = bias,
           label = paste0("Bias = ", round(bias, 2)),
           hjust = 1.1, colour = "blue") +
  annotate("text", x = Inf, y = upper_loa,
           label = paste0("+1.96 SD = ", round(upper_loa, 2)),
           hjust = 1.1, colour = "red") +
  annotate("text", x = Inf, y = lower_loa,
           label = paste0("-1.96 SD = ", round(lower_loa, 2)),
           hjust = 1.1, colour = "red") +
  theme_minimal() +
  labs(title = "Bland-Altman Plot",
       x = "Mean of Two Methods",
       y = "Difference (Method 1 − Method 2)")

# ── Test for proportional bias ──
summary(lm(diff ~ avg, data = df))`
  }
};

export const AGREEMENT_STEPS = [
  {
    id: "agree_type",
    title: "What type of measurement are you comparing?",
    subtitle: "The kind of data your raters or methods produce",
    options: [
      { value: "nominal", label: "Categorical (unordered)", desc: "Diagnoses, classifications, binary results (yes/no)", icon: "🏷️" },
      { value: "ordinal", label: "Ordinal (ordered categories)", desc: "Severity grades, Likert scales, staging", icon: "📊" },
      { value: "continuous", label: "Continuous measurement", desc: "BP, lab values, tumour size, scores", icon: "📏" },
    ],
  },
  {
    id: "agree_purpose",
    title: "What is the purpose of the comparison?",
    subtitle: "This determines which agreement statistic is most appropriate",
    show: (a) => a.agree_type === "continuous",
    options: [
      { value: "reliability", label: "Rater / Test-Retest Reliability", desc: "Do raters (or repeated measurements) give consistent results?", icon: "👥" },
      { value: "method_comparison", label: "Method Comparison", desc: "Can a new measurement method replace the standard one?", icon: "🔄" },
      { value: "both", label: "Both reliability and method comparison", desc: "Assess ICC for reliability AND Bland-Altman for interchangeability", icon: "📋" },
    ],
  },
];

export function recommendAgreement(answers) {
  const { agree_type, agree_purpose } = answers;
  if (agree_type === "nominal") return ["cohen_kappa"];
  if (agree_type === "ordinal") return ["weighted_kappa"];
  if (agree_type === "continuous") {
    if (agree_purpose === "reliability") return ["icc"];
    if (agree_purpose === "method_comparison") return ["bland_altman"];
    if (agree_purpose === "both") return ["icc", "bland_altman"];
  }
  return [];
}

export function explainAgreementReasoning(answers) {
  const { agree_type, agree_purpose } = answers;
  if (agree_type === "nominal") {
    return "Your data is **categorical (unordered)**, so **Cohen's kappa** is the appropriate measure of inter-rater agreement. It corrects for agreement that would occur by chance alone.";
  }
  if (agree_type === "ordinal") {
    return "Your data is **ordinal (ordered categories)**, so **weighted kappa** is appropriate. It gives partial credit for near-misses (e.g., classifying as 'moderate' when the other rater said 'mild' is penalised less than 'severe' vs. 'mild').";
  }
  if (agree_type === "continuous") {
    if (agree_purpose === "reliability") return "You want to assess **reliability** of continuous measurements across raters. The **Intraclass Correlation Coefficient (ICC)** quantifies how much of the total variance is due to true differences between subjects vs. rater disagreement.";
    if (agree_purpose === "method_comparison") return "You want to determine if two **measurement methods are interchangeable**. **Bland-Altman analysis** is the standard approach — it shows the bias (systematic difference) and the 95% limits of agreement (range within which most differences fall).";
    if (agree_purpose === "both") return "You need both **reliability (ICC)** and **method interchangeability (Bland-Altman)**. ICC tells you how consistent the measurements are; Bland-Altman tells you whether the two methods agree well enough to be used interchangeably in clinical practice.";
  }
  return "";
}

// ─── Glossary of Key Terms ───

export const GLOSSARY = {
  "p-value": "The probability of observing results as extreme as the data, assuming the null hypothesis is true. It is NOT the probability that the null hypothesis is true. Smaller p-values indicate stronger evidence against the null.",
  "confidence interval": "A range of values that is likely to contain the true population parameter. A 95% CI means if you repeated the study 100 times, approximately 95 of the intervals would contain the true value.",
  "effect size": "A quantitative measure of the magnitude of a phenomenon. Examples: Cohen\u2019s d (mean difference in SD units), Odds Ratio, Hazard Ratio, Correlation coefficient (r). Unlike p-values, effect sizes indicate practical importance.",
  "null hypothesis": "The default assumption that there is no difference or no association between groups/variables. Statistical tests evaluate the evidence against this hypothesis.",
  "confounder": "A variable that is associated with both the exposure and the outcome, potentially distorting the true relationship. Adjusted analysis (regression) controls for confounders.",
  "normality": "The assumption that data follows a bell-shaped (Gaussian) distribution. Assessed using Shapiro-Wilk test, histograms, or Q-Q plots. Many parametric tests require this.",
  "Type I error": "Rejecting the null hypothesis when it is actually true (a false positive). The conventional threshold (\u03b1 = 0.05) means accepting a 5% risk of this error.",
  "Type II error": "Failing to reject the null hypothesis when it is actually false (a false negative). Related to statistical power (1 \u2212 \u03b2).",
  "statistical power": "The probability of correctly detecting a true effect (rejecting a false null hypothesis). Depends on sample size, effect size, and significance level. Typically aimed at \u2265 80%.",
  "degrees of freedom": "The number of independent values that can vary in a statistical calculation. Affects the shape of test distributions (t, \u03c7\u00b2, F). Generally related to sample size minus the number of estimated parameters.",
  "odds ratio": "The ratio of odds of an event in the exposed group to the odds in the unexposed group. OR = 1 means no association; OR > 1 means increased odds; OR < 1 means decreased odds.",
  "hazard ratio": "In survival analysis, the ratio of hazard rates between groups. HR = 1 means equal risk; HR < 1 means lower risk in the treatment group; HR > 1 means higher risk.",
  "regression coefficient": "The estimated change in the outcome for a one-unit increase in the predictor, holding other variables constant. In linear regression, it is the slope (B); in logistic regression, the log-odds.",
  "multicollinearity": "When two or more predictors in a regression model are highly correlated with each other. Assessed using VIF (Variance Inflation Factor). VIF > 5\u201310 suggests a problem.",
  "homoscedasticity": "The assumption that the variance of residuals is constant across all levels of the predictor(s). Violated if a residual plot shows a funnel shape. Required for linear regression.",
  "non-parametric test": "A test that does not assume a specific distribution (e.g., normality). Uses ranks instead of raw values. Examples: Mann-Whitney, Wilcoxon, Kruskal-Wallis. Less powerful than parametric tests when normality holds, but more robust when it does not.",
  "Bonferroni correction": "A method to control Type I error when performing multiple comparisons. Divides the significance level (\u03b1) by the number of comparisons. Conservative but simple.",
  "AUC": "Area Under the ROC Curve. Measures how well a model discriminates between positive and negative cases. AUC = 0.5 means no discrimination (coin flip); AUC = 1.0 means perfect discrimination. Generally: 0.7\u20130.8 acceptable, 0.8\u20130.9 good, >0.9 excellent.",
  "IQR": "Interquartile Range \u2014 the range between the 25th percentile (Q1) and 75th percentile (Q3). Contains the middle 50% of the data. Used to summarise skewed distributions alongside the median.",
  "MCID": "Minimum Clinically Important Difference \u2014 the smallest change in an outcome that patients or clinicians would consider meaningful. Used to interpret whether a statistically significant effect is also clinically relevant.",
  "sensitivity": "The proportion of true positives correctly identified by a test. Sensitivity = TP / (TP + FN). A highly sensitive test rarely misses disease (few false negatives). Also called the true positive rate.",
  "specificity": "The proportion of true negatives correctly identified by a test. Specificity = TN / (TN + FP). A highly specific test rarely gives false alarms (few false positives). Also called the true negative rate.",
  "PPV": "Positive Predictive Value \u2014 the probability that a person with a positive test result actually has the disease. PPV = TP / (TP + FP). Depends on disease prevalence.",
  "NPV": "Negative Predictive Value \u2014 the probability that a person with a negative test result truly does not have the disease. NPV = TN / (TN + FN). Depends on disease prevalence.",
  "likelihood ratio": "How much a test result changes the probability of disease. LR+ = sensitivity / (1 \u2212 specificity); LR\u2212 = (1 \u2212 sensitivity) / specificity. LR+ > 10 or LR\u2212 < 0.1 are strong; LR+ 5\u201310 or LR\u2212 0.1\u20130.2 are moderate.",
  "ROC curve": "Receiver Operating Characteristic curve \u2014 plots sensitivity (y-axis) against 1\u2212specificity (x-axis) at all possible cutoff points. The closer the curve to the top-left corner, the better the discrimination.",
  "Youden's index": "J = Sensitivity + Specificity \u2212 1. The cutoff that maximises Youden\u2019s index is the 'optimal' cutoff balancing sensitivity and specificity. Ranges from 0 (useless test) to 1 (perfect test).",
  "kappa": "Cohen's kappa (\u03ba) measures inter-rater agreement correcting for chance. \u03ba = 0 means agreement no better than chance; \u03ba = 1 means perfect agreement. Landis & Koch scale: <0.20 poor, 0.21\u20130.40 fair, 0.41\u20130.60 moderate, 0.61\u20130.80 substantial, 0.81\u20131.00 almost perfect.",
  "ICC": "Intraclass Correlation Coefficient \u2014 measures reliability/agreement for continuous data among multiple raters. Accounts for both systematic and random differences. Koo & Li scale: <0.50 poor, 0.50\u20130.75 moderate, 0.75\u20130.90 good, >0.90 excellent.",
  "Bland-Altman": "A method for assessing agreement between two measurement methods. Plots the difference between methods against their mean. Shows systematic bias (mean difference) and 95% limits of agreement (mean \u00b1 1.96\u00d7SD). If limits fall within clinical tolerance, methods are interchangeable.",
};
