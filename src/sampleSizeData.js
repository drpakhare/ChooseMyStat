// ──────────────────────────────────────────────────────────
// ChooseMyStat — Sample Size Calculator Engine
// ──────────────────────────────────────────────────────────
// In-browser sample size computation + R code + G*Power + SAP text
// Formulas validated against R pwr package and published references

// ━━━ MATH UTILITIES ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

// Inverse normal CDF (Peter Acklam's algorithm, accurate to ~1.15e-9)
export function qnorm(p) {
  if (p <= 0 || p >= 1) return NaN;
  if (p === 0.5) return 0;
  const a = [-3.969683028665376e1, 2.209460984245205e2, -2.759285104469687e2,
    1.383577518672690e2, -3.066479806614716e1, 2.506628277459239e0];
  const b = [-5.447609879822406e1, 1.615858368580409e2, -1.556989798598866e2,
    6.680131188771972e1, -1.328068155288572e1];
  const c = [-7.784894002430293e-3, -3.223964580411365e-1, -2.400758277161838e0,
    -2.549732539343734e0, 4.374664141464968e0, 2.938163982698783e0];
  const d = [7.784695709041462e-3, 3.224671290700398e-1, 2.445134137142996e0,
    3.754408661907416e0];
  const pLow = 0.02425, pHigh = 1 - pLow;
  let q, r;
  if (p < pLow) {
    q = Math.sqrt(-2 * Math.log(p));
    return (((((c[0]*q+c[1])*q+c[2])*q+c[3])*q+c[4])*q+c[5]) /
           ((((d[0]*q+d[1])*q+d[2])*q+d[3])*q+1);
  } else if (p <= pHigh) {
    q = p - 0.5; r = q * q;
    return (((((a[0]*r+a[1])*r+a[2])*r+a[3])*r+a[4])*r+a[5])*q /
           (((((b[0]*r+b[1])*r+b[2])*r+b[3])*r+b[4])*r+1);
  } else {
    q = Math.sqrt(-2 * Math.log(1 - p));
    return -(((((c[0]*q+c[1])*q+c[2])*q+c[3])*q+c[4])*q+c[5]) /
            ((((d[0]*q+d[1])*q+d[2])*q+d[3])*q+1);
  }
}

const ceil = Math.ceil;

// ━━━ DECISION STEPS ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

export const SS_STEPS = [
  {
    id: "ss_goal",
    title: "What type of analysis are you planning?",
    subtitle: "Select the analysis for which you need a sample size",
    options: [
      { value: "compare", label: "Comparing Groups", desc: "t-test, ANOVA, chi-square — comparing means or proportions across groups", icon: "👥" },
      { value: "association", label: "Correlation / Association", desc: "Pearson or Spearman correlation between two variables", icon: "📈" },
      { value: "regression", label: "Regression Modelling", desc: "Linear or logistic regression with multiple predictors", icon: "⚙️" },
      { value: "survival", label: "Survival / Time-to-Event", desc: "Log-rank test or Cox regression", icon: "⏱" },
      { value: "diagnostic", label: "Diagnostic Test Evaluation", desc: "Sensitivity, specificity, AUC", icon: "🎯" },
      { value: "agreement", label: "Agreement / Reliability", desc: "Kappa, ICC, Bland-Altman", icon: "🤝" },
    ],
  },
  {
    id: "ss_compare_type",
    title: "What is your comparison design?",
    show: (a) => a.ss_goal === "compare",
    options: [
      { value: "two_ind_cont", label: "2 Independent Groups — Continuous", desc: "Comparing means: t-test / Mann-Whitney", icon: "📏" },
      { value: "two_paired_cont", label: "Paired / Before-After — Continuous", desc: "Comparing paired means: paired t-test / Wilcoxon", icon: "🔄" },
      { value: "one_sample", label: "One Group vs Reference Value", desc: "One-sample t-test: comparing sample mean to a known value", icon: "1️⃣" },
      { value: "anova", label: "3+ Groups — Continuous", desc: "One-way ANOVA / Kruskal-Wallis", icon: "👥" },
      { value: "two_proportions", label: "2 Groups — Proportions", desc: "Chi-square or Fisher's exact test", icon: "🔘" },
      { value: "paired_binary", label: "Paired — Binary Outcome", desc: "McNemar's test: before/after with yes/no outcome", icon: "🔄" },
    ],
  },
  {
    id: "ss_regression_type",
    title: "What type of regression?",
    show: (a) => a.ss_goal === "regression",
    options: [
      { value: "linear_reg", label: "Linear Regression", desc: "Continuous outcome, multiple predictors", icon: "📏" },
      { value: "logistic_reg", label: "Logistic Regression", desc: "Binary outcome (yes/no), odds ratios", icon: "🔘" },
    ],
  },
  {
    id: "ss_survival_type",
    title: "What survival analysis?",
    show: (a) => a.ss_goal === "survival",
    options: [
      { value: "logrank", label: "Log-rank Test", desc: "Comparing survival curves between two groups", icon: "⏱" },
      { value: "cox", label: "Cox Regression", desc: "Multivariable survival model with hazard ratios", icon: "⚙️" },
    ],
  },
  {
    id: "ss_diagnostic_type",
    title: "What diagnostic metric?",
    show: (a) => a.ss_goal === "diagnostic",
    options: [
      { value: "sens_spec", label: "Sensitivity / Specificity", desc: "Buderer's formula for diagnostic accuracy study", icon: "🎯" },
      { value: "auc", label: "AUC Comparison", desc: "Comparing AUC of two diagnostic tests", icon: "📊" },
    ],
  },
  {
    id: "ss_agreement_type",
    title: "What type of agreement?",
    show: (a) => a.ss_goal === "agreement",
    options: [
      { value: "kappa", label: "Cohen's Kappa", desc: "Agreement between two raters on categorical outcome", icon: "🤝" },
      { value: "icc", label: "ICC", desc: "Intraclass correlation for continuous measurements", icon: "📏" },
      { value: "bland_altman", label: "Bland-Altman", desc: "Limits of agreement between two measurement methods", icon: "📉" },
    ],
  },
];

// Resolve which calculator to use from answers
export function resolveCalculator(answers) {
  const g = answers.ss_goal;
  if (g === "compare") return answers.ss_compare_type;
  if (g === "association") return "correlation";
  if (g === "regression") return answers.ss_regression_type;
  if (g === "survival") return answers.ss_survival_type;
  if (g === "diagnostic") return answers.ss_diagnostic_type;
  if (g === "agreement") return answers.ss_agreement_type;
  return null;
}

// ━━━ CALCULATOR DEFINITIONS ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

export const CALCULATORS = {

  // ── 1. Independent two-sample t-test ──
  two_ind_cont: {
    name: "Two Independent Groups (t-test / Mann-Whitney)",
    linkedTests: ["independent_t", "mann_whitney"],
    color: "indigo",
    parameters: [
      { id: "input_mode", label: "Effect specification", type: "radio", options: [
        { value: "raw", label: "Raw values (mean difference + SD)" },
        { value: "cohen", label: "Cohen's d (standardised)" },
      ], default: "raw" },
      { id: "mean_diff", label: "Expected mean difference (δ)", type: "number", step: 0.1, placeholder: "e.g. 5", show: (p) => p.input_mode === "raw" },
      { id: "sd", label: "Common standard deviation (σ)", type: "number", step: 0.1, placeholder: "e.g. 15", show: (p) => p.input_mode === "raw" },
      { id: "cohen_d", label: "Cohen's d", type: "number", step: 0.1, placeholder: "0.2 small / 0.5 medium / 0.8 large", show: (p) => p.input_mode === "cohen" },
      { id: "alpha", label: "Significance level (α)", type: "select", options: [
        { value: 0.05, label: "0.05 (standard)" }, { value: 0.01, label: "0.01" }, { value: 0.10, label: "0.10" },
      ], default: 0.05 },
      { id: "power", label: "Power (1 − β)", type: "select", options: [
        { value: 0.80, label: "80%" }, { value: 0.85, label: "85%" }, { value: 0.90, label: "90%" }, { value: 0.95, label: "95%" },
      ], default: 0.80 },
      { id: "sides", label: "Sidedness", type: "radio", options: [
        { value: 2, label: "Two-sided (recommended)" }, { value: 1, label: "One-sided" },
      ], default: 2 },
      { id: "ratio", label: "Allocation ratio (n₂/n₁)", type: "number", step: 0.1, default: 1, placeholder: "1 = equal groups" },
    ],
    compute(p) {
      const d = p.input_mode === "cohen" ? Number(p.cohen_d) : Number(p.mean_diff) / Number(p.sd);
      if (!d || d <= 0) return null;
      const alpha = Number(p.alpha), power = Number(p.power), sides = Number(p.sides);
      const r = Number(p.ratio) || 1;
      const za = qnorm(1 - alpha / sides);
      const zb = qnorm(power);
      const n1 = ceil((za + zb) ** 2 * (1 + 1/r) / (d ** 2));
      const n2 = ceil(n1 * r);
      return {
        n1, n2, total: n1 + n2,
        d: d.toFixed(3),
        label: r === 1 ? `${n1} per group (${n1+n2} total)` : `Group 1: ${n1}, Group 2: ${n2} (${n1+n2} total)`,
        formula: `n₁ = (z_{α/${sides}} + z_β)² × (1 + 1/r) / d²\n   = (${za.toFixed(3)} + ${zb.toFixed(3)})² × (1 + 1/${r}) / ${d.toFixed(3)}²\n   = ${((za+zb)**2).toFixed(2)} × ${(1+1/r).toFixed(2)} / ${(d**2).toFixed(4)}\n   = ${(((za+zb)**2)*(1+1/r)/(d**2)).toFixed(1)} → ${n1}`,
      };
    },
    rCode(p) {
      const d = p.input_mode === "cohen" ? Number(p.cohen_d) : Number(p.mean_diff) / Number(p.sd);
      const r = Number(p.ratio) || 1;
      return `library(pwr)

# Two-sample t-test
pwr.t.test(
  d = ${d.toFixed(4)},          # Cohen's d = mean_diff / SD${p.input_mode === "raw" ? ` = ${p.mean_diff}/${p.sd}` : ""}
  sig.level = ${p.alpha},
  power = ${p.power},
  type = "two.sample",
  alternative = "${Number(p.sides) === 2 ? "two.sided" : "greater"}"
)${r !== 1 ? `\n\n# Unequal allocation (1:${r})
library(pwr)
pwr.t2n.test(
  d = ${d.toFixed(4)},
  n1 = NULL, n2 = NULL,
  sig.level = ${p.alpha},
  power = ${p.power},
  alternative = "${Number(p.sides) === 2 ? "two.sided" : "greater"}"
)
# Then multiply n by allocation ratio` : ""}`;
    },
    gpower(p) {
      const d = p.input_mode === "cohen" ? Number(p.cohen_d) : Number(p.mean_diff) / Number(p.sd);
      return `G*Power Settings:
Test family: t tests
Statistical test: Means — Difference between two independent means (two groups)
Type of power analysis: A priori

Input parameters:
  Tail(s): ${Number(p.sides) === 2 ? "Two" : "One"}
  Effect size d: ${d.toFixed(4)}
  α err prob: ${p.alpha}
  Power (1-β): ${p.power}
  Allocation ratio N2/N1: ${Number(p.ratio) || 1}`;
    },
    sap(p, result) {
      if (!result) return "";
      const r = Number(p.ratio) || 1;
      return `Sample size was calculated for a ${Number(p.sides) === 2 ? "two-sided" : "one-sided"} independent samples t-test with ${p.input_mode === "raw" ? `an expected mean difference of ${p.mean_diff} (SD ${p.sd}, Cohen's d = ${result.d})` : `Cohen's d = ${p.cohen_d}`}, α = ${p.alpha}, and ${Number(p.power)*100}% power. ${r === 1 ? `A minimum of ${result.n1} participants per group (${result.total} total)` : `A minimum of ${result.n1} in group 1 and ${result.n2} in group 2 (${result.total} total)`} will be required.`;
    },
  },

  // ── 2. Paired t-test ──
  two_paired_cont: {
    name: "Paired / Before-After (paired t-test / Wilcoxon)",
    linkedTests: ["paired_t", "wilcoxon_signed"],
    color: "indigo",
    parameters: [
      { id: "input_mode", label: "Effect specification", type: "radio", options: [
        { value: "raw", label: "Raw values (expected change + SD of change)" },
        { value: "cohen", label: "Cohen's d (standardised)" },
      ], default: "raw" },
      { id: "mean_diff", label: "Expected mean change (δ)", type: "number", step: 0.1, placeholder: "e.g. 5", show: (p) => p.input_mode === "raw" },
      { id: "sd_diff", label: "SD of within-subject differences", type: "number", step: 0.1, placeholder: "e.g. 10", show: (p) => p.input_mode === "raw" },
      { id: "cohen_d", label: "Cohen's d", type: "number", step: 0.1, placeholder: "0.2 small / 0.5 medium / 0.8 large", show: (p) => p.input_mode === "cohen" },
      { id: "alpha", label: "Significance level (α)", type: "select", options: [
        { value: 0.05, label: "0.05" }, { value: 0.01, label: "0.01" }, { value: 0.10, label: "0.10" },
      ], default: 0.05 },
      { id: "power", label: "Power (1 − β)", type: "select", options: [
        { value: 0.80, label: "80%" }, { value: 0.85, label: "85%" }, { value: 0.90, label: "90%" }, { value: 0.95, label: "95%" },
      ], default: 0.80 },
      { id: "sides", label: "Sidedness", type: "radio", options: [
        { value: 2, label: "Two-sided (recommended)" }, { value: 1, label: "One-sided" },
      ], default: 2 },
    ],
    compute(p) {
      const d = p.input_mode === "cohen" ? Number(p.cohen_d) : Number(p.mean_diff) / Number(p.sd_diff);
      if (!d || d <= 0) return null;
      const za = qnorm(1 - Number(p.alpha) / Number(p.sides));
      const zb = qnorm(Number(p.power));
      const n = ceil((za + zb) ** 2 / (d ** 2));
      return { n, total: n, d: d.toFixed(3),
        label: `${n} paired subjects`,
        formula: `n = (z_{α/${p.sides}} + z_β)² / d²\n   = (${za.toFixed(3)} + ${zb.toFixed(3)})² / ${d.toFixed(3)}²\n   = ${((za+zb)**2).toFixed(2)} / ${(d**2).toFixed(4)} = ${((za+zb)**2/(d**2)).toFixed(1)} → ${n}`,
      };
    },
    rCode(p) {
      const d = p.input_mode === "cohen" ? Number(p.cohen_d) : Number(p.mean_diff) / Number(p.sd_diff);
      return `library(pwr)

# Paired t-test
pwr.t.test(
  d = ${d.toFixed(4)},          # d = mean_change / SD_change${p.input_mode === "raw" ? ` = ${p.mean_diff}/${p.sd_diff}` : ""}
  sig.level = ${p.alpha},
  power = ${p.power},
  type = "paired",
  alternative = "${Number(p.sides) === 2 ? "two.sided" : "greater"}"
)`;
    },
    gpower(p) {
      const d = p.input_mode === "cohen" ? Number(p.cohen_d) : Number(p.mean_diff) / Number(p.sd_diff);
      return `G*Power Settings:
Test family: t tests
Statistical test: Means — Difference between two dependent means (matched pairs)
Type of power analysis: A priori

Input parameters:
  Tail(s): ${Number(p.sides) === 2 ? "Two" : "One"}
  Effect size dz: ${d.toFixed(4)}
  α err prob: ${p.alpha}
  Power (1-β): ${p.power}`;
    },
    sap(p, result) {
      if (!result) return "";
      return `Sample size was calculated for a ${Number(p.sides) === 2 ? "two-sided" : "one-sided"} paired t-test with ${p.input_mode === "raw" ? `an expected mean change of ${p.mean_diff} (SD of change ${p.sd_diff}, Cohen's d = ${result.d})` : `Cohen's d = ${p.cohen_d}`}, α = ${p.alpha}, and ${Number(p.power)*100}% power. A minimum of ${result.n} paired subjects will be required.`;
    },
  },

  // ── 3. One-sample t-test ──
  one_sample: {
    name: "One Sample vs Reference Value",
    linkedTests: ["one_sample_t"],
    color: "indigo",
    parameters: [
      { id: "mean_diff", label: "Expected difference from reference value", type: "number", step: 0.1, placeholder: "e.g. 3" },
      { id: "sd", label: "Expected standard deviation (σ)", type: "number", step: 0.1, placeholder: "e.g. 10" },
      { id: "alpha", label: "Significance level (α)", type: "select", options: [
        { value: 0.05, label: "0.05" }, { value: 0.01, label: "0.01" }, { value: 0.10, label: "0.10" },
      ], default: 0.05 },
      { id: "power", label: "Power (1 − β)", type: "select", options: [
        { value: 0.80, label: "80%" }, { value: 0.85, label: "85%" }, { value: 0.90, label: "90%" }, { value: 0.95, label: "95%" },
      ], default: 0.80 },
      { id: "sides", label: "Sidedness", type: "radio", options: [
        { value: 2, label: "Two-sided" }, { value: 1, label: "One-sided" },
      ], default: 2 },
    ],
    compute(p) {
      const d = Number(p.mean_diff) / Number(p.sd);
      if (!d || d <= 0) return null;
      const za = qnorm(1 - Number(p.alpha) / Number(p.sides));
      const zb = qnorm(Number(p.power));
      const n = ceil((za + zb) ** 2 / (d ** 2));
      return { n, total: n, d: d.toFixed(3),
        label: `${n} subjects`,
        formula: `n = (z_{α/${p.sides}} + z_β)² / d²\n   = (${za.toFixed(3)} + ${zb.toFixed(3)})² / ${(d).toFixed(3)}²\n   = ${((za+zb)**2/(d**2)).toFixed(1)} → ${n}`,
      };
    },
    rCode(p) {
      const d = Number(p.mean_diff) / Number(p.sd);
      return `library(pwr)

# One-sample t-test
pwr.t.test(
  d = ${d.toFixed(4)},          # d = difference / SD = ${p.mean_diff}/${p.sd}
  sig.level = ${p.alpha},
  power = ${p.power},
  type = "one.sample",
  alternative = "${Number(p.sides) === 2 ? "two.sided" : "greater"}"
)`;
    },
    gpower(p) {
      const d = Number(p.mean_diff) / Number(p.sd);
      return `G*Power Settings:
Test family: t tests
Statistical test: Means — Difference from constant (one sample case)
Type of power analysis: A priori

Input parameters:
  Tail(s): ${Number(p.sides) === 2 ? "Two" : "One"}
  Effect size d: ${d.toFixed(4)}
  α err prob: ${p.alpha}
  Power (1-β): ${p.power}`;
    },
    sap(p, result) {
      if (!result) return "";
      return `Sample size was calculated for a one-sample t-test with an expected difference of ${p.mean_diff} from the reference value (SD ${p.sd}, Cohen's d = ${result.d}), α = ${p.alpha}, and ${Number(p.power)*100}% power (${Number(p.sides) === 2 ? "two-sided" : "one-sided"}). A minimum of ${result.n} subjects will be required.`;
    },
  },

  // ── 4. One-way ANOVA ──
  anova: {
    name: "Three or More Groups (ANOVA)",
    linkedTests: ["one_way_anova", "kruskal_wallis"],
    color: "indigo",
    parameters: [
      { id: "k", label: "Number of groups (k)", type: "number", step: 1, default: 3, placeholder: "e.g. 3" },
      { id: "input_mode", label: "Effect specification", type: "radio", options: [
        { value: "cohen_f", label: "Cohen's f (standardised)" },
        { value: "means", label: "Range of means and common SD" },
      ], default: "cohen_f" },
      { id: "cohen_f", label: "Cohen's f", type: "number", step: 0.05, placeholder: "0.10 small / 0.25 medium / 0.40 large", show: (p) => p.input_mode === "cohen_f" },
      { id: "mean_range", label: "Difference: largest − smallest group mean", type: "number", step: 0.1, placeholder: "e.g. 10", show: (p) => p.input_mode === "means" },
      { id: "sd", label: "Common within-group SD", type: "number", step: 0.1, placeholder: "e.g. 15", show: (p) => p.input_mode === "means" },
      { id: "alpha", label: "Significance level (α)", type: "select", options: [
        { value: 0.05, label: "0.05" }, { value: 0.01, label: "0.01" },
      ], default: 0.05 },
      { id: "power", label: "Power (1 − β)", type: "select", options: [
        { value: 0.80, label: "80%" }, { value: 0.85, label: "85%" }, { value: 0.90, label: "90%" }, { value: 0.95, label: "95%" },
      ], default: 0.80 },
    ],
    compute(p) {
      const k = Number(p.k) || 3;
      let f;
      if (p.input_mode === "means") {
        // Approximate Cohen's f from range of means (assumes equally spaced)
        f = (Number(p.mean_range) / Number(p.sd)) / (2 * Math.sqrt(k));
      } else {
        f = Number(p.cohen_f);
      }
      if (!f || f <= 0) return null;
      const za = qnorm(1 - Number(p.alpha) / 2);
      const zb = qnorm(Number(p.power));
      // Corrected formula: n/group ≈ ((za + zb)² + (k-1)) / (k × f²)
      // This approximation closely matches R's pwr.anova.test (noncentral F)
      const n = ceil(((za + zb) ** 2 + (k - 1)) / (k * f ** 2));
      const total = n * k;
      return { n, k, total, f: f.toFixed(3),
        label: `${n} per group × ${k} groups = ${total} total`,
        formula: `n/group ≈ ((z_{α/2} + z_β)² + (k−1)) / (k × f²)\n   = ((${za.toFixed(3)} + ${zb.toFixed(3)})² + ${k-1}) / (${k} × ${f.toFixed(3)}²)\n   = (${((za+zb)**2).toFixed(2)} + ${k-1}) / ${(k*f**2).toFixed(4)}\n   = ${(((za+zb)**2+(k-1))/(k*f**2)).toFixed(1)} → ${n}\nTotal = ${n} × ${k} = ${total}`,
      };
    },
    rCode(p) {
      const k = Number(p.k) || 3;
      let f;
      if (p.input_mode === "means") {
        f = (Number(p.mean_range) / Number(p.sd)) / (2 * Math.sqrt(k));
      } else { f = Number(p.cohen_f); }
      return `library(pwr)

# One-way ANOVA (${k} groups)
pwr.anova.test(
  k = ${k},                     # number of groups
  f = ${f.toFixed(4)},          # Cohen's f${p.input_mode === "means" ? ` ≈ (${p.mean_range}/${p.sd}) / (2√${k})` : ""}
  sig.level = ${p.alpha},
  power = ${p.power}
)`;
    },
    gpower(p) {
      const k = Number(p.k) || 3;
      let f;
      if (p.input_mode === "means") {
        f = (Number(p.mean_range) / Number(p.sd)) / (2 * Math.sqrt(k));
      } else { f = Number(p.cohen_f); }
      return `G*Power Settings:
Test family: F tests
Statistical test: ANOVA — Fixed effects, omnibus, one-way
Type of power analysis: A priori

Input parameters:
  Effect size f: ${f.toFixed(4)}
  α err prob: ${p.alpha}
  Power (1-β): ${p.power}
  Number of groups: ${k}`;
    },
    sap(p, result) {
      if (!result) return "";
      return `Sample size was calculated for a one-way ANOVA with ${result.k} groups, Cohen's f = ${result.f}, α = ${p.alpha}, and ${Number(p.power)*100}% power. A minimum of ${result.n} participants per group (${result.total} total) will be required.`;
    },
  },

  // ── 5. Two proportions (chi-square / Fisher's) ──
  two_proportions: {
    name: "Two Groups — Proportions (Chi-square / Fisher's)",
    linkedTests: ["chi_square", "fisher_exact"],
    color: "indigo",
    parameters: [
      { id: "p1", label: "Expected proportion in group 1 (p₁)", type: "number", step: 0.01, placeholder: "e.g. 0.30" },
      { id: "p2", label: "Expected proportion in group 2 (p₂)", type: "number", step: 0.01, placeholder: "e.g. 0.50" },
      { id: "alpha", label: "Significance level (α)", type: "select", options: [
        { value: 0.05, label: "0.05" }, { value: 0.01, label: "0.01" }, { value: 0.10, label: "0.10" },
      ], default: 0.05 },
      { id: "power", label: "Power (1 − β)", type: "select", options: [
        { value: 0.80, label: "80%" }, { value: 0.85, label: "85%" }, { value: 0.90, label: "90%" }, { value: 0.95, label: "95%" },
      ], default: 0.80 },
      { id: "sides", label: "Sidedness", type: "radio", options: [
        { value: 2, label: "Two-sided" }, { value: 1, label: "One-sided" },
      ], default: 2 },
      { id: "ratio", label: "Allocation ratio (n₂/n₁)", type: "number", step: 0.1, default: 1, placeholder: "1 = equal" },
    ],
    compute(p) {
      const p1 = Number(p.p1), p2 = Number(p.p2);
      if (!p1 || !p2 || p1 <= 0 || p1 >= 1 || p2 <= 0 || p2 >= 1 || p1 === p2) return null;
      const alpha = Number(p.alpha), power = Number(p.power), sides = Number(p.sides);
      const r = Number(p.ratio) || 1;
      const za = qnorm(1 - alpha / sides);
      const zb = qnorm(power);
      const pbar = (p1 + r * p2) / (1 + r);
      // Fleiss formula with continuity correction
      const n1 = ceil((za * Math.sqrt((1 + 1/r) * pbar * (1 - pbar)) + zb * Math.sqrt(p1*(1-p1) + p2*(1-p2)/r)) ** 2 / ((p1 - p2) ** 2));
      const n2 = ceil(n1 * r);
      return { n1, n2, total: n1 + n2,
        label: r === 1 ? `${n1} per group (${n1+n2} total)` : `Group 1: ${n1}, Group 2: ${n2} (${n1+n2} total)`,
        formula: `Using Fleiss formula:\np̄ = (p₁ + r×p₂)/(1+r) = ${pbar.toFixed(4)}\nn₁ = [z_α√((1+1/r)p̄(1-p̄)) + z_β√(p₁(1-p₁)+p₂(1-p₂)/r)]² / (p₁-p₂)²\n   = ${n1}`,
      };
    },
    rCode(p) {
      const h = 2 * Math.asin(Math.sqrt(Number(p.p1))) - 2 * Math.asin(Math.sqrt(Number(p.p2)));
      return `library(pwr)

# Two-proportion z-test (using Cohen's h)
pwr.2p.test(
  h = ${Math.abs(h).toFixed(4)},           # Cohen's h = 2×arcsin(√p₁) - 2×arcsin(√p₂)
  sig.level = ${p.alpha},                  # h calculated from p₁=${p.p1}, p₂=${p.p2}
  power = ${p.power},
  alternative = "${Number(p.sides) === 2 ? "two.sided" : "greater"}"
)

# Alternative: using epiR (more intuitive)
library(epiR)
epi.sscompc(
  treat = ${p.p2},
  control = ${p.p1},
  n = NA,
  power = ${p.power},
  r = ${Number(p.ratio) || 1},
  design = 1,
  sided.test = ${p.sides},
  conf.level = 1 - ${p.alpha}
)`;
    },
    gpower(p) {
      const h = 2 * Math.asin(Math.sqrt(Number(p.p1))) - 2 * Math.asin(Math.sqrt(Number(p.p2)));
      return `G*Power Settings:
Test family: z tests
Statistical test: Proportions — Difference between two independent proportions
Type of power analysis: A priori

Input parameters:
  Tail(s): ${Number(p.sides) === 2 ? "Two" : "One"}
  Effect size |h|: ${Math.abs(h).toFixed(4)}    (from p₁=${p.p1}, p₂=${p.p2})
  α err prob: ${p.alpha}
  Power (1-β): ${p.power}
  Allocation ratio N2/N1: ${Number(p.ratio) || 1}`;
    },
    sap(p, result) {
      if (!result) return "";
      return `Sample size was calculated for a two-proportion comparison (expected proportions: ${p.p1} vs ${p.p2}), α = ${p.alpha}, ${Number(p.power)*100}% power, ${Number(p.sides) === 2 ? "two-sided" : "one-sided"} test. ${Number(p.ratio) === 1 || !p.ratio ? `A minimum of ${result.n1} participants per group (${result.total} total)` : `A minimum of ${result.n1} in group 1 and ${result.n2} in group 2 (${result.total} total)`} will be required.`;
    },
  },

  // ── 6. McNemar's test ──
  paired_binary: {
    name: "Paired Binary (McNemar's Test)",
    linkedTests: ["mcnemar"],
    color: "indigo",
    parameters: [
      { id: "p_disc", label: "Expected discordant proportion (p₁₂ + p₂₁)", type: "number", step: 0.01, placeholder: "e.g. 0.25 — proportion of pairs that change" },
      { id: "odds_ratio", label: "Expected OR of discordant pairs (p₁₂/p₂₁)", type: "number", step: 0.1, placeholder: "e.g. 2.0 — ratio of discordant cells" },
      { id: "alpha", label: "Significance level (α)", type: "select", options: [
        { value: 0.05, label: "0.05" }, { value: 0.01, label: "0.01" },
      ], default: 0.05 },
      { id: "power", label: "Power (1 − β)", type: "select", options: [
        { value: 0.80, label: "80%" }, { value: 0.85, label: "85%" }, { value: 0.90, label: "90%" },
      ], default: 0.80 },
    ],
    compute(p) {
      const pd = Number(p.p_disc), or = Number(p.odds_ratio);
      if (!pd || pd <= 0 || pd >= 1 || !or || or <= 0 || or === 1) return null;
      const za = qnorm(1 - Number(p.alpha) / 2);
      const zb = qnorm(Number(p.power));
      // Connor (1987) formula
      const p12 = pd * or / (1 + or);
      const p21 = pd / (1 + or);
      const n = ceil((za * Math.sqrt(pd) + zb * Math.sqrt(pd - (p12 - p21)**2)) ** 2 / ((p12 - p21) ** 2));
      return { n, total: n, label: `${n} pairs`,
        formula: `p₁₂ = ${pd} × ${or}/(1+${or}) = ${p12.toFixed(4)}\np₂₁ = ${pd}/(1+${or}) = ${p21.toFixed(4)}\nn = [z_α√(p_d) + z_β√(p_d - (p₁₂-p₂₁)²)]² / (p₁₂-p₂₁)² = ${n}`,
      };
    },
    rCode(p) {
      return `# McNemar's test sample size
# Using the exact R package or manual formula
library(epiR)

# Discordant proportions approach
epi.ssmcnemar(
  pdexp = ${Number(p.p_disc) * Number(p.odds_ratio) / (1 + Number(p.odds_ratio))},  # p12 (discordant: +/−)
  pdnexp = ${Number(p.p_disc) / (1 + Number(p.odds_ratio))},                         # p21 (discordant: −/+)
  n = NA,
  power = ${p.power},
  conf.level = 1 - ${p.alpha}
)`;
    },
    gpower(p) {
      const pd = Number(p.p_disc), or = Number(p.odds_ratio);
      const p12 = pd * or / (1 + or), p21 = pd / (1 + or);
      return `G*Power Settings:
Test family: χ² tests
Statistical test: Proportions — McNemar test
Type of power analysis: A priori

Input parameters:
  Proportion discordant pairs: ${pd}
  Proportion p12 (pos→neg): ${p12.toFixed(4)}
  Proportion p21 (neg→pos): ${p21.toFixed(4)}
  α err prob: ${p.alpha}
  Power (1-β): ${p.power}`;
    },
    sap(p, result) {
      if (!result) return "";
      return `Sample size was calculated for McNemar's test with an expected discordant proportion of ${p.p_disc} and an odds ratio of discordant pairs of ${p.odds_ratio}, α = ${p.alpha}, and ${Number(p.power)*100}% power. A minimum of ${result.n} matched pairs will be required.`;
    },
  },

  // ── 7. Correlation ──
  correlation: {
    name: "Correlation (Pearson / Spearman)",
    linkedTests: ["pearson_r", "spearman_rho"],
    color: "indigo",
    parameters: [
      { id: "r", label: "Expected correlation coefficient (r)", type: "number", step: 0.05, placeholder: "0.10 small / 0.30 medium / 0.50 large" },
      { id: "alpha", label: "Significance level (α)", type: "select", options: [
        { value: 0.05, label: "0.05" }, { value: 0.01, label: "0.01" },
      ], default: 0.05 },
      { id: "power", label: "Power (1 − β)", type: "select", options: [
        { value: 0.80, label: "80%" }, { value: 0.85, label: "85%" }, { value: 0.90, label: "90%" }, { value: 0.95, label: "95%" },
      ], default: 0.80 },
      { id: "sides", label: "Sidedness", type: "radio", options: [
        { value: 2, label: "Two-sided" }, { value: 1, label: "One-sided" },
      ], default: 2 },
    ],
    compute(p) {
      const r = Number(p.r);
      if (!r || Math.abs(r) >= 1) return null;
      const za = qnorm(1 - Number(p.alpha) / Number(p.sides));
      const zb = qnorm(Number(p.power));
      const C = 0.5 * Math.log((1 + Math.abs(r)) / (1 - Math.abs(r))); // Fisher's z
      const n = ceil(((za + zb) / C) ** 2 + 3);
      return { n, total: n, label: `${n} subjects`,
        formula: `Fisher's z = 0.5 × ln((1+|r|)/(1−|r|)) = ${C.toFixed(4)}\nn = ((z_α + z_β) / z_r)² + 3\n   = ((${za.toFixed(3)} + ${zb.toFixed(3)}) / ${C.toFixed(4)})² + 3\n   = ${(((za+zb)/C)**2+3).toFixed(1)} → ${n}`,
      };
    },
    rCode(p) {
      return `library(pwr)

# Correlation test
pwr.r.test(
  r = ${p.r},
  sig.level = ${p.alpha},
  power = ${p.power},
  alternative = "${Number(p.sides) === 2 ? "two.sided" : "greater"}"
)`;
    },
    gpower(p) {
      return `G*Power Settings:
Test family: Exact
Statistical test: Correlation — Bivariate normal model
Type of power analysis: A priori

Input parameters:
  Tail(s): ${Number(p.sides) === 2 ? "Two" : "One"}
  Correlation ρ H1: ${p.r}
  α err prob: ${p.alpha}
  Power (1-β): ${p.power}
  Correlation ρ H0: 0`;
    },
    sap(p, result) {
      if (!result) return "";
      return `Sample size was calculated for a ${Number(p.sides) === 2 ? "two-sided" : "one-sided"} test of correlation (expected r = ${p.r}), α = ${p.alpha}, and ${Number(p.power)*100}% power. A minimum of ${result.n} subjects will be required.`;
    },
  },

  // ── 8. Linear regression ──
  linear_reg: {
    name: "Multiple Linear Regression",
    linkedTests: ["linear_regression"],
    color: "indigo",
    parameters: [
      { id: "input_mode", label: "Effect specification", type: "radio", options: [
        { value: "f2", label: "Cohen's f² (standardised)" },
        { value: "r2", label: "Expected R² and number of predictors" },
      ], default: "r2" },
      { id: "r2", label: "Expected R² (variance explained by model)", type: "number", step: 0.01, placeholder: "e.g. 0.15", show: (p) => p.input_mode === "r2" },
      { id: "f2", label: "Cohen's f²", type: "number", step: 0.01, placeholder: "0.02 small / 0.15 medium / 0.35 large", show: (p) => p.input_mode === "f2" },
      { id: "k", label: "Number of predictors", type: "number", step: 1, placeholder: "e.g. 5" },
      { id: "alpha", label: "Significance level (α)", type: "select", options: [
        { value: 0.05, label: "0.05" }, { value: 0.01, label: "0.01" },
      ], default: 0.05 },
      { id: "power", label: "Power (1 − β)", type: "select", options: [
        { value: 0.80, label: "80%" }, { value: 0.85, label: "85%" }, { value: 0.90, label: "90%" }, { value: 0.95, label: "95%" },
      ], default: 0.80 },
    ],
    compute(p) {
      const k = Number(p.k);
      let f2;
      if (p.input_mode === "r2") {
        const r2 = Number(p.r2);
        if (!r2 || r2 <= 0 || r2 >= 1) return null;
        f2 = r2 / (1 - r2);
      } else {
        f2 = Number(p.f2);
      }
      if (!f2 || f2 <= 0 || !k || k < 1) return null;
      const za = qnorm(1 - Number(p.alpha) / 2);
      const zb = qnorm(Number(p.power));
      // Cohen (1988): n = L/f² + u + 1 where L ≈ (za + zb)²
      const L = (za + zb) ** 2;
      const n = ceil(L / f2 + k + 1);
      return { n, total: n, f2: f2.toFixed(4),
        label: `${n} subjects`,
        formula: `f² = ${p.input_mode === "r2" ? `R²/(1−R²) = ${p.r2}/(1−${p.r2}) = ` : ""}${f2.toFixed(4)}\nL = (z_{α/2} + z_β)² = ${L.toFixed(2)}\nn = L/f² + k + 1 = ${L.toFixed(2)}/${f2.toFixed(4)} + ${k} + 1 = ${(L/f2+k+1).toFixed(1)} → ${n}`,
      };
    },
    rCode(p) {
      const k = Number(p.k);
      let f2;
      if (p.input_mode === "r2") { f2 = Number(p.r2) / (1 - Number(p.r2)); }
      else { f2 = Number(p.f2); }
      return `library(pwr)

# Multiple linear regression (F-test for overall model)
pwr.f2.test(
  u = ${k},                     # numerator df = number of predictors
  f2 = ${f2.toFixed(4)},        # Cohen's f²${p.input_mode === "r2" ? ` = R²/(1-R²) = ${p.r2}/(1-${p.r2})` : ""}
  sig.level = ${p.alpha},
  power = ${p.power}
)
# Result gives v (denominator df); total n = v + u + 1`;
    },
    gpower(p) {
      let f2;
      if (p.input_mode === "r2") { f2 = Number(p.r2) / (1 - Number(p.r2)); }
      else { f2 = Number(p.f2); }
      return `G*Power Settings:
Test family: F tests
Statistical test: Linear multiple regression — Fixed model, R² deviation from zero
Type of power analysis: A priori

Input parameters:
  Effect size f²: ${f2.toFixed(4)}
  α err prob: ${p.alpha}
  Power (1-β): ${p.power}
  Number of predictors: ${p.k}`;
    },
    sap(p, result) {
      if (!result) return "";
      return `Sample size was calculated for a multiple linear regression model with ${p.k} predictors, ${p.input_mode === "r2" ? `expected R² = ${p.r2} (f² = ${result.f2})` : `Cohen's f² = ${p.f2}`}, α = ${p.alpha}, and ${Number(p.power)*100}% power. A minimum of ${result.n} subjects will be required.`;
    },
  },

  // ── 9. Logistic regression ──
  logistic_reg: {
    name: "Binary Logistic Regression",
    linkedTests: ["binary_logistic"],
    color: "indigo",
    parameters: [
      { id: "input_mode", label: "Approach", type: "radio", options: [
        { value: "hsieh", label: "Hsieh (1989) — based on expected OR" },
        { value: "epv", label: "Events per variable (EPV) rule" },
      ], default: "hsieh" },
      { id: "p_event", label: "Expected event proportion (smaller outcome)", type: "number", step: 0.01, placeholder: "e.g. 0.20" },
      { id: "or", label: "Expected odds ratio for key predictor", type: "number", step: 0.1, placeholder: "e.g. 2.0", show: (p) => p.input_mode === "hsieh" },
      { id: "k", label: "Number of predictors", type: "number", step: 1, placeholder: "e.g. 5" },
      { id: "epv_target", label: "Target EPV", type: "select", options: [
        { value: 10, label: "10 (minimum)" }, { value: 15, label: "15 (recommended)" }, { value: 20, label: "20 (conservative)" },
      ], default: 10, show: (p) => p.input_mode === "epv" },
      { id: "alpha", label: "Significance level (α)", type: "select", options: [
        { value: 0.05, label: "0.05" }, { value: 0.01, label: "0.01" },
      ], default: 0.05, show: (p) => p.input_mode === "hsieh" },
      { id: "power", label: "Power (1 − β)", type: "select", options: [
        { value: 0.80, label: "80%" }, { value: 0.90, label: "90%" },
      ], default: 0.80, show: (p) => p.input_mode === "hsieh" },
    ],
    compute(p) {
      const pe = Number(p.p_event), k = Number(p.k);
      if (!pe || pe <= 0 || pe >= 1 || !k || k < 1) return null;
      if (p.input_mode === "epv") {
        const epv = Number(p.epv_target) || 10;
        const events = epv * k;
        const n = ceil(events / pe);
        return { n, total: n, events,
          label: `${n} subjects (≥ ${events} events)`,
          formula: `Events needed = EPV × predictors = ${epv} × ${k} = ${events}\nn = events / proportion = ${events} / ${pe} = ${(events/pe).toFixed(1)} → ${n}`,
        };
      } else {
        const or = Number(p.or);
        if (!or || or <= 0 || or === 1) return null;
        const za = qnorm(1 - Number(p.alpha) / 2);
        const zb = qnorm(Number(p.power));
        // Hsieh (1989): n = (za + zb)² / (p × (1-p) × (ln(OR))²)
        const n = ceil((za + zb) ** 2 / (pe * (1 - pe) * Math.log(or) ** 2));
        return { n, total: n,
          label: `${n} subjects`,
          formula: `Hsieh (1989):\nn = (z_α + z_β)² / [p(1-p) × (ln OR)²]\n   = (${za.toFixed(3)} + ${zb.toFixed(3)})² / [${pe}×${(1-pe).toFixed(2)} × (ln ${or})²]\n   = ${((za+zb)**2).toFixed(2)} / [${(pe*(1-pe)).toFixed(4)} × ${(Math.log(or)**2).toFixed(4)}]\n   = ${((za+zb)**2/(pe*(1-pe)*Math.log(or)**2)).toFixed(1)} → ${n}\n\nAlso verify EPV: ${n} × ${pe} = ${Math.round(n*pe)} events / ${k} predictors = ${(n*pe/k).toFixed(1)} EPV ${(n*pe/k) >= 10 ? "✓" : "⚠ < 10, consider increasing n"}`,
        };
      }
    },
    rCode(p) {
      const pe = Number(p.p_event), k = Number(p.k);
      if (p.input_mode === "epv") {
        const epv = Number(p.epv_target) || 10;
        return `# Events-per-variable (EPV) rule
# EPV = ${epv}, predictors = ${k}, event proportion = ${pe}
events_needed <- ${epv} * ${k}   # = ${epv * k}
n <- ceiling(events_needed / ${pe})  # = ${ceil(epv * k / pe)}
cat("Minimum sample size:", n, "\\n")
cat("Expected events:", ceiling(n * ${pe}), "\\n")
cat("EPV:", round(n * ${pe} / ${k}, 1), "\\n")`;
      }
      return `# Hsieh (1989) formula for logistic regression
library(pwr)

# Using epiR package
library(epiR)
epi.sscc(
  OR = ${p.or},
  p1 = NA,
  p0 = ${pe},
  n = NA,
  power = ${p.power},
  r = 1,
  sided.test = 2,
  conf.level = 1 - ${p.alpha}
)

# Manual calculation
p <- ${pe}
OR <- ${p.or}
z_alpha <- qnorm(1 - ${p.alpha}/2)
z_beta <- qnorm(${p.power})
n <- ceiling((z_alpha + z_beta)^2 / (p * (1-p) * log(OR)^2))
cat("n =", n, "\\nEPV =", round(n * p / ${k}, 1), "\\n")`;
    },
    gpower(p) {
      if (p.input_mode === "epv") {
        return `EPV rule is a heuristic — G*Power does not have a direct equivalent.
Use the Hsieh formula approach in G*Power:

Test family: z tests
Statistical test: Logistic regression
Type of power analysis: A priori`;
      }
      return `G*Power Settings:
Test family: z tests
Statistical test: Logistic regression
Type of power analysis: A priori

Input parameters:
  Tail(s): Two
  Odds ratio: ${p.or}
  Pr(Y=1|X=1) H0: ${p.p_event}
  α err prob: ${p.alpha}
  Power (1-β): ${p.power}
  R² other X: 0`;
    },
    sap(p, result) {
      if (!result) return "";
      if (p.input_mode === "epv") {
        return `Sample size was estimated using the events-per-variable (EPV) rule, requiring a minimum of ${Number(p.epv_target) || 10} events per predictor variable. With ${p.k} predictors and an expected event proportion of ${p.p_event}, a minimum of ${result.n} subjects (yielding approximately ${result.events} events) will be required.`;
      }
      return `Sample size was calculated for a logistic regression model using the Hsieh (1989) formula, with an expected odds ratio of ${p.or} for the primary predictor, baseline event proportion of ${p.p_event}, α = ${p.alpha}, and ${Number(p.power)*100}% power. A minimum of ${result.n} subjects will be required. The model includes ${p.k} predictors.`;
    },
  },

  // ── 10. Log-rank test ──
  logrank: {
    name: "Log-rank Test (Survival Comparison)",
    linkedTests: ["log_rank"],
    color: "indigo",
    parameters: [
      { id: "hr", label: "Expected hazard ratio (HR)", type: "number", step: 0.05, placeholder: "e.g. 0.70 (treatment better) or 1.50 (treatment worse)" },
      { id: "p_event", label: "Expected overall event probability", type: "number", step: 0.05, placeholder: "e.g. 0.40 — proportion who experience event" },
      { id: "alpha", label: "Significance level (α)", type: "select", options: [
        { value: 0.05, label: "0.05" }, { value: 0.01, label: "0.01" },
      ], default: 0.05 },
      { id: "power", label: "Power (1 − β)", type: "select", options: [
        { value: 0.80, label: "80%" }, { value: 0.85, label: "85%" }, { value: 0.90, label: "90%" },
      ], default: 0.80 },
      { id: "ratio", label: "Allocation ratio (n₂/n₁)", type: "number", step: 0.1, default: 1, placeholder: "1 = equal" },
    ],
    compute(p) {
      const hr = Number(p.hr), pe = Number(p.p_event), r = Number(p.ratio) || 1;
      if (!hr || hr <= 0 || hr === 1 || !pe || pe <= 0 || pe > 1) return null;
      const za = qnorm(1 - Number(p.alpha) / 2);
      const zb = qnorm(Number(p.power));
      // Schoenfeld (1983): total events
      const p1 = 1 / (1 + r), p2 = r / (1 + r);
      const d = ceil((za + zb) ** 2 / (p1 * p2 * Math.log(hr) ** 2));
      const n = ceil(d / pe);
      const n1 = ceil(n * p1), n2 = ceil(n * p2);
      return { d, n, n1, n2, total: n,
        label: `${d} events needed → ${n} subjects total${r !== 1 ? ` (${n1} + ${n2})` : ""}`,
        formula: `Schoenfeld formula:\nd = (z_{α/2} + z_β)² / [p₁ × p₂ × (ln HR)²]\n   = (${za.toFixed(3)} + ${zb.toFixed(3)})² / [${p1.toFixed(2)} × ${p2.toFixed(2)} × (ln ${hr})²]\n   = ${((za+zb)**2).toFixed(2)} / ${(p1*p2*Math.log(hr)**2).toFixed(4)} = ${d} events\nn = events / P(event) = ${d} / ${pe} = ${n}`,
      };
    },
    rCode(p) {
      return `# Log-rank test sample size (Schoenfeld formula)
library(survival)

# Using ssizeCT.default from powerSurvEpi
library(powerSurvEpi)
ssizeCT.default(
  power = ${p.power},
  k = ${Number(p.ratio) || 1},          # allocation ratio
  pE = ${p.p_event},                     # overall event probability
  pA = NA,
  HR = ${p.hr},
  alpha = ${p.alpha}
)

# Manual Schoenfeld formula
HR <- ${p.hr}
p_event <- ${p.p_event}
z_alpha <- qnorm(1 - ${p.alpha}/2)
z_beta <- qnorm(${p.power})
r <- ${Number(p.ratio) || 1}
p1 <- 1/(1+r); p2 <- r/(1+r)
events <- ceiling((z_alpha + z_beta)^2 / (p1 * p2 * log(HR)^2))
n_total <- ceiling(events / p_event)
cat("Events needed:", events, "\\nTotal n:", n_total, "\\n")`;
    },
    gpower(p) {
      return `G*Power Settings:
Test family: z tests
Statistical test: Proportions — Cox regression (Wald test, two groups)
OR
Test family: t tests → Survival (log-rank)

For dedicated survival analysis, use the R code or:
  PASS software → Log-rank test
  nQuery → Survival analysis module

Key inputs:
  Hazard ratio: ${p.hr}
  Overall event probability: ${p.p_event}
  α: ${p.alpha}, Power: ${p.power}
  Allocation ratio: ${Number(p.ratio) || 1}`;
    },
    sap(p, result) {
      if (!result) return "";
      return `Sample size was calculated for a log-rank test using the Schoenfeld (1983) formula, with an expected hazard ratio of ${p.hr}, overall event probability of ${p.p_event}, α = ${p.alpha}, and ${Number(p.power)*100}% power. A total of ${result.d} events will be required, translating to ${result.total} participants with ${Number(p.ratio) === 1 ? "equal allocation" : `a ${Number(p.ratio)}:1 allocation ratio`}.`;
    },
  },

  // ── 11. Cox regression (EPV) ──
  cox: {
    name: "Cox Proportional Hazards Regression",
    linkedTests: ["cox_ph"],
    color: "indigo",
    parameters: [
      { id: "k", label: "Number of covariates in the model", type: "number", step: 1, placeholder: "e.g. 6" },
      { id: "p_event", label: "Expected event proportion", type: "number", step: 0.05, placeholder: "e.g. 0.30" },
      { id: "epv_target", label: "Target events per variable (EPV)", type: "select", options: [
        { value: 10, label: "10 (minimum — Peduzzi 1995)" }, { value: 15, label: "15 (recommended)" }, { value: 20, label: "20 (conservative — Vittinghoff 2007)" },
      ], default: 10 },
    ],
    compute(p) {
      const k = Number(p.k), pe = Number(p.p_event), epv = Number(p.epv_target) || 10;
      if (!k || k < 1 || !pe || pe <= 0 || pe > 1) return null;
      const events = epv * k;
      const n = ceil(events / pe);
      return { n, total: n, events,
        label: `${n} subjects (≥ ${events} events)`,
        formula: `Events = EPV × covariates = ${epv} × ${k} = ${events}\nn = events / P(event) = ${events} / ${pe} = ${(events/pe).toFixed(1)} → ${n}`,
      };
    },
    rCode(p) {
      const k = Number(p.k), pe = Number(p.p_event), epv = Number(p.epv_target) || 10;
      return `# Cox regression — events per variable rule
# Peduzzi et al. (1995), Vittinghoff & McCulloch (2007)
k <- ${k}           # number of covariates
p_event <- ${pe}    # event proportion
epv <- ${epv}       # events per variable target

events_needed <- epv * k
n <- ceiling(events_needed / p_event)
cat("Covariates:", k, "\\nEvents needed:", events_needed,
    "\\nTotal n:", n, "\\nActual EPV:", round(n * p_event / k, 1), "\\n")`;
    },
    gpower() {
      return `The EPV rule is a heuristic guideline, not a formal power calculation.
G*Power does not directly implement EPV-based sizing.

For formal Cox regression power analysis, use:
  • R: powerSurvEpi::ssizeCT.default()
  • PASS or nQuery software

Recommended reading:
  Peduzzi et al. (1995) J Clin Epidemiol 49:1373-79
  Vittinghoff & McCulloch (2007) Stat Med 26:2328-39`;
    },
    sap(p, result) {
      if (!result) return "";
      const epv = Number(p.epv_target) || 10;
      return `Sample size for the Cox regression model was estimated using the events-per-variable (EPV) rule, requiring ≥ ${epv} events per covariate. With ${p.k} covariates and an expected event proportion of ${p.p_event}, a minimum of ${result.n} subjects will be enrolled to accrue at least ${result.events} events.`;
    },
  },

  // ── 12. Diagnostic accuracy (Buderer's formula) ──
  sens_spec: {
    name: "Diagnostic Accuracy (Sensitivity / Specificity)",
    linkedTests: ["diagnostic_2x2"],
    color: "rose",
    parameters: [
      { id: "target", label: "Which metric to power for?", type: "radio", options: [
        { value: "sensitivity", label: "Sensitivity" }, { value: "specificity", label: "Specificity" }, { value: "both", label: "Both (take the larger)" },
      ], default: "both" },
      { id: "sn", label: "Expected sensitivity", type: "number", step: 0.01, placeholder: "e.g. 0.90" },
      { id: "sp", label: "Expected specificity", type: "number", step: 0.01, placeholder: "e.g. 0.85" },
      { id: "prev", label: "Expected disease prevalence", type: "number", step: 0.01, placeholder: "e.g. 0.30" },
      { id: "width", label: "Desired precision (half-width of 95% CI)", type: "number", step: 0.01, default: 0.05, placeholder: "e.g. 0.05 for ±5%" },
      { id: "conf", label: "Confidence level", type: "select", options: [
        { value: 0.95, label: "95%" }, { value: 0.90, label: "90%" }, { value: 0.99, label: "99%" },
      ], default: 0.95 },
    ],
    compute(p) {
      const sn = Number(p.sn), sp = Number(p.sp), prev = Number(p.prev), w = Number(p.width);
      const z = qnorm(1 - (1 - Number(p.conf)) / 2);
      if (!sn || !sp || !prev || !w) return null;
      const n_sn = ceil(z ** 2 * sn * (1 - sn) / (w ** 2));
      const n_sp = ceil(z ** 2 * sp * (1 - sp) / (w ** 2));
      const n_diseased = n_sn;
      const n_nondiseased = n_sp;
      const total_for_sn = ceil(n_diseased / prev);
      const total_for_sp = ceil(n_nondiseased / (1 - prev));
      const target = p.target;
      let n_final, explain;
      if (target === "sensitivity") {
        n_final = total_for_sn;
        explain = `For sensitivity: ${n_diseased} diseased / ${prev} prevalence = ${total_for_sn}`;
      } else if (target === "specificity") {
        n_final = total_for_sp;
        explain = `For specificity: ${n_nondiseased} non-diseased / ${(1-prev).toFixed(2)} = ${total_for_sp}`;
      } else {
        n_final = Math.max(total_for_sn, total_for_sp);
        explain = `For sensitivity: ${n_diseased} diseased → ${total_for_sn} total\nFor specificity: ${n_nondiseased} non-diseased → ${total_for_sp} total\nTake the larger: ${n_final}`;
      }
      return { n: n_final, total: n_final, n_diseased, n_nondiseased,
        label: `${n_final} subjects total`,
        formula: `Buderer's formula: n = Z² × P(1−P) / W²\nSensitivity: n_diseased = ${z.toFixed(2)}² × ${sn} × ${(1-sn).toFixed(2)} / ${w}² = ${n_diseased}\nSpecificity: n_nondiseased = ${z.toFixed(2)}² × ${sp} × ${(1-sp).toFixed(2)} / ${w}² = ${n_nondiseased}\n${explain}`,
      };
    },
    rCode(p) {
      return `# Buderer's formula for diagnostic accuracy studies
library(epiR)

# For sensitivity
epi.ssdxsesp(
  test = ${p.sn},                # expected sensitivity
  type = "se",
  Py = ${p.prev},               # disease prevalence
  epsilon = ${p.width},          # desired precision (half-width of CI)
  error = "absolute",
  conf.level = ${p.conf}
)

# For specificity
epi.ssdxsesp(
  test = ${p.sp},                # expected specificity
  type = "sp",
  Py = ${p.prev},
  epsilon = ${p.width},
  error = "absolute",
  conf.level = ${p.conf}
)`;
    },
    gpower() {
      return `G*Power does not have a dedicated diagnostic accuracy module.
Use the R epiR package (shown in R tab) or manual calculation.

Alternative software:
  • MedCalc → Diagnostic test → Sample size
  • PASS → Sensitivity and specificity confidence intervals`;
    },
    sap(p, result) {
      if (!result) return "";
      return `Sample size for the diagnostic accuracy study was calculated using Buderer's formula, assuming ${p.target === "sensitivity" ? `sensitivity of ${p.sn}` : p.target === "specificity" ? `specificity of ${p.sp}` : `sensitivity of ${p.sn} and specificity of ${p.sp}`}, disease prevalence of ${p.prev}, and a desired precision of ±${Number(p.width)*100}% for the ${Number(p.conf)*100}% confidence interval. A minimum of ${result.total} subjects will be required.`;
    },
  },

  // ── 13. AUC comparison ──
  auc: {
    name: "AUC Comparison (Two Diagnostic Tests)",
    linkedTests: ["roc_auc"],
    color: "rose",
    parameters: [
      { id: "auc1", label: "Expected AUC of test 1", type: "number", step: 0.01, placeholder: "e.g. 0.75" },
      { id: "auc2", label: "Expected AUC of test 2", type: "number", step: 0.01, placeholder: "e.g. 0.85" },
      { id: "ratio_cases", label: "Ratio of controls to cases", type: "number", step: 0.5, default: 1, placeholder: "1 = equal" },
      { id: "alpha", label: "Significance level (α)", type: "select", options: [
        { value: 0.05, label: "0.05" }, { value: 0.01, label: "0.01" },
      ], default: 0.05 },
      { id: "power", label: "Power (1 − β)", type: "select", options: [
        { value: 0.80, label: "80%" }, { value: 0.90, label: "90%" },
      ], default: 0.80 },
    ],
    compute(p) {
      const a1 = Number(p.auc1), a2 = Number(p.auc2), r = Number(p.ratio_cases) || 1;
      if (!a1 || !a2 || a1 <= 0.5 || a2 <= 0.5 || a1 >= 1 || a2 >= 1 || a1 === a2) return null;
      const za = qnorm(1 - Number(p.alpha) / 2);
      const zb = qnorm(Number(p.power));
      // Obuchowski (1998) approximation
      const v1 = a1 * (1 - a1) + (r - 1) * (a1 / (2 - a1) - a1 * a1);
      const v2 = a2 * (1 - a2) + (r - 1) * (a2 / (2 - a2) - a2 * a2);
      const n_cases = ceil((za + zb) ** 2 * (v1 + v2) / ((a1 - a2) ** 2));
      const n_controls = ceil(n_cases * r);
      return { n_cases, n_controls, total: n_cases + n_controls,
        label: `${n_cases} cases + ${n_controls} controls = ${n_cases + n_controls} total`,
        formula: `Obuchowski (1998) approximation:\nn_cases = (z_α + z_β)² × (V₁ + V₂) / (AUC₁ − AUC₂)²\nV₁ = ${v1.toFixed(4)}, V₂ = ${v2.toFixed(4)}\nn_cases = ${n_cases}`,
      };
    },
    rCode(p) {
      return `# AUC comparison sample size
# Obuchowski (1998) formula
library(pROC)

# Manual calculation
auc1 <- ${p.auc1}; auc2 <- ${p.auc2}
kappa <- ${Number(p.ratio_cases) || 1}  # controls:cases ratio
z_alpha <- qnorm(1 - ${p.alpha}/2)
z_beta <- qnorm(${p.power})

V1 <- auc1*(1-auc1) + (kappa-1)*(auc1/(2-auc1) - auc1^2)
V2 <- auc2*(1-auc2) + (kappa-1)*(auc2/(2-auc2) - auc2^2)
n_cases <- ceiling((z_alpha + z_beta)^2 * (V1 + V2) / (auc1 - auc2)^2)
n_controls <- ceiling(n_cases * kappa)
cat("Cases:", n_cases, "Controls:", n_controls,
    "Total:", n_cases + n_controls, "\\n")`;
    },
    gpower() {
      return `G*Power does not directly support AUC comparison.
Use the R code (pROC package) or MedCalc software.

Alternative: MedCalc → ROC curve analysis → Sample size`;
    },
    sap(p, result) {
      if (!result) return "";
      return `Sample size for comparing two ROC curves was calculated using the Obuchowski (1998) method, with expected AUCs of ${p.auc1} and ${p.auc2}, α = ${p.alpha}, and ${Number(p.power)*100}% power. A minimum of ${result.n_cases} cases and ${result.n_controls} controls (${result.total} total) will be required.`;
    },
  },

  // ── 14. Cohen's kappa ──
  kappa: {
    name: "Cohen's Kappa (Inter-rater Agreement)",
    linkedTests: ["cohen_kappa", "weighted_kappa"],
    color: "amber",
    parameters: [
      { id: "kappa1", label: "Expected kappa under alternative (κ₁)", type: "number", step: 0.05, placeholder: "e.g. 0.70" },
      { id: "kappa0", label: "Kappa under null hypothesis (κ₀)", type: "number", step: 0.05, default: 0, placeholder: "0 for testing against chance" },
      { id: "k", label: "Number of categories", type: "number", step: 1, default: 2, placeholder: "e.g. 2, 3, 5" },
      { id: "props", label: "Expected prevalence of positive category", type: "number", step: 0.05, default: 0.50, placeholder: "0.50 for balanced" },
      { id: "alpha", label: "Significance level (α)", type: "select", options: [
        { value: 0.05, label: "0.05" }, { value: 0.01, label: "0.01" },
      ], default: 0.05 },
      { id: "power", label: "Power (1 − β)", type: "select", options: [
        { value: 0.80, label: "80%" }, { value: 0.90, label: "90%" },
      ], default: 0.80 },
    ],
    compute(p) {
      const k1 = Number(p.kappa1), k0 = Number(p.kappa0), pi = Number(p.props);
      if (!k1 || k1 <= k0 || k1 > 1) return null;
      const za = qnorm(1 - Number(p.alpha) / 2);
      const zb = qnorm(Number(p.power));
      // Simplified Donner & Eliasziw (1992)
      const pe = 2 * pi * (1 - pi); // expected chance agreement for 2 categories
      const var0 = pe * (1 - (k0 * (1 - pe) + pe)) / ((1 - pe) ** 2);
      const var1 = pe * (1 - (k1 * (1 - pe) + pe)) / ((1 - pe) ** 2);
      const n = ceil((za * Math.sqrt(Math.max(var0, 0.001)) + zb * Math.sqrt(Math.max(var1, 0.001))) ** 2 / ((k1 - k0) ** 2));
      return { n, total: n,
        label: `${n} subjects rated by both raters`,
        formula: `Donner & Eliasziw (1992):\np_e = 2π(1−π) = ${pe.toFixed(4)}\nV₀ = ${var0.toFixed(4)}, V₁ = ${var1.toFixed(4)}\nn = (z_α√V₀ + z_β√V₁)² / (κ₁−κ₀)² = ${n}`,
      };
    },
    rCode(p) {
      return `# Cohen's kappa sample size
# Donner & Eliasziw (1992) / Sim & Wright (2005)
library(irr)

# Using kappaSize package
# install.packages("kappaSize")
library(kappaSize)
CIBinary(
  kappa0 = ${p.kappa0},         # null kappa
  kappaL = NA,                   # lower CI bound (alternative approach)
  kappaU = NA,
  props = c(${p.props}, ${(1-Number(p.props)).toFixed(2)}),
  raters = 2,
  alpha = ${p.alpha}
)

# Manual formula
kappa1 <- ${p.kappa1}; kappa0 <- ${p.kappa0}
pi_pos <- ${p.props}
pe <- 2 * pi_pos * (1 - pi_pos)
z_a <- qnorm(1 - ${p.alpha}/2)
z_b <- qnorm(${p.power})
V0 <- pe * (1 - (kappa0*(1-pe)+pe)) / (1-pe)^2
V1 <- pe * (1 - (kappa1*(1-pe)+pe)) / (1-pe)^2
n <- ceiling((z_a*sqrt(V0) + z_b*sqrt(V1))^2 / (kappa1-kappa0)^2)
cat("Sample size:", n, "subjects\\n")`;
    },
    gpower() {
      return `G*Power does not have a dedicated kappa module.
Use the R kappaSize package or manual formula (see R tab).

Alternative reference:
  Sim & Wright (2005) Physical Therapy 85:257-268
  Donner & Eliasziw (1992) Stat Med 11:1511-1519`;
    },
    sap(p, result) {
      if (!result) return "";
      return `Sample size for the inter-rater agreement study was calculated using the Donner & Eliasziw (1992) formula, with an expected kappa of ${p.kappa1}${Number(p.kappa0) > 0 ? ` vs null kappa of ${p.kappa0}` : " (testing against chance agreement)"}, α = ${p.alpha}, and ${Number(p.power)*100}% power. A minimum of ${result.n} subjects will be rated by both raters.`;
    },
  },

  // ── 15. ICC ──
  icc: {
    name: "Intraclass Correlation Coefficient (ICC)",
    linkedTests: ["icc"],
    color: "amber",
    parameters: [
      { id: "icc1", label: "Expected ICC under alternative (ρ₁)", type: "number", step: 0.05, placeholder: "e.g. 0.80" },
      { id: "icc0", label: "ICC under null hypothesis (ρ₀)", type: "number", step: 0.05, default: 0, placeholder: "0 for testing against no agreement" },
      { id: "k", label: "Number of raters / measurements per subject", type: "number", step: 1, default: 2, placeholder: "e.g. 2, 3" },
      { id: "alpha", label: "Significance level (α)", type: "select", options: [
        { value: 0.05, label: "0.05" }, { value: 0.01, label: "0.01" },
      ], default: 0.05 },
      { id: "power", label: "Power (1 − β)", type: "select", options: [
        { value: 0.80, label: "80%" }, { value: 0.90, label: "90%" },
      ], default: 0.80 },
    ],
    compute(p) {
      const r1 = Number(p.icc1), r0 = Number(p.icc0), k = Number(p.k) || 2;
      if (!r1 || r1 <= r0 || r1 >= 1 || r0 < 0) return null;
      const za = qnorm(1 - Number(p.alpha) / 2);
      const zb = qnorm(Number(p.power));
      // Walter, Eliasziw & Donner (1998)
      const n = ceil(1 + 2 * (za + zb) ** 2 * ((1 - r1 * r0) ** 2 * (1 + (k - 1) * r1) ** 2 * (1 + (k - 1) * r0) ** 2) / (k * (k - 1) * (r1 - r0) ** 2));
      return { n, total: n,
        label: `${n} subjects (each measured ${k} times)`,
        formula: `Walter, Eliasziw & Donner (1998):\nk = ${k} measurements per subject\nρ₁ = ${r1}, ρ₀ = ${r0}\nn = ${n}`,
      };
    },
    rCode(p) {
      const k = Number(p.k) || 2;
      return `# ICC sample size — Walter, Eliasziw & Donner (1998)
# install.packages("ICC.Sample.Size")
library(ICC.Sample.Size)

# Using the formula
rho1 <- ${p.icc1}; rho0 <- ${p.icc0}
k <- ${k}
z_a <- qnorm(1 - ${p.alpha}/2)
z_b <- qnorm(${p.power})

n <- ceiling(1 + 2*(z_a + z_b)^2 *
  ((1 - rho1*rho0)^2 * (1 + (k-1)*rho1)^2 * (1 + (k-1)*rho0)^2) /
  (k * (k-1) * (rho1 - rho0)^2))
cat("Subjects needed:", n, "\\n")
cat("Total measurements:", n * k, "\\n")`;
    },
    gpower() {
      return `G*Power does not have a dedicated ICC module.
Use the R formula (see R tab) or ICC.Sample.Size package.

Reference:
  Walter, Eliasziw & Donner (1998) J Clin Epidemiol 51:171-176`;
    },
    sap(p, result) {
      if (!result) return "";
      return `Sample size for the ICC study was calculated using the Walter, Eliasziw & Donner (1998) formula, with an expected ICC of ${p.icc1}${Number(p.icc0) > 0 ? ` vs ${p.icc0}` : ""}, ${Number(p.k) || 2} measurements per subject, α = ${p.alpha}, and ${Number(p.power)*100}% power. A minimum of ${result.n} subjects will be required.`;
    },
  },

  // ── 16. Bland-Altman ──
  bland_altman: {
    name: "Bland-Altman Limits of Agreement",
    linkedTests: ["bland_altman"],
    color: "amber",
    parameters: [
      { id: "sd_diff", label: "Expected SD of differences between methods", type: "number", step: 0.1, placeholder: "e.g. 5.0" },
      { id: "max_loa", label: "Maximum acceptable limit of agreement", type: "number", step: 0.1, placeholder: "e.g. 10.0 — clinically acceptable LoA width" },
      { id: "conf", label: "Confidence level for LoA CI", type: "select", options: [
        { value: 0.95, label: "95%" }, { value: 0.90, label: "90%" },
      ], default: 0.95 },
    ],
    compute(p) {
      const sd = Number(p.sd_diff), loa = Number(p.max_loa);
      if (!sd || sd <= 0 || !loa || loa <= 0) return null;
      // Lu et al. (2016) / Bland & Altman (1986):
      // For precise estimation of LoA with CI
      // n ≈ 3 × (z × SD / δ)² where δ is desired precision
      const z = qnorm(1 - (1 - Number(p.conf)) / 2);
      // LoA = mean_diff ± 1.96 × SD; precision of LoA = desired half-width of CI of the limit
      const precision = (loa - 1.96 * sd); // how much tighter than LoA itself
      // Alternative: use the rule that n ≈ 1.96² × 3 × SD² / δ² for 95% CI of LoA
      // Simpler approach: minimum n for acceptable CI around LoA
      const n = Math.max(ceil(3 * (z * sd / (loa / 4)) ** 2), 40);
      return { n, total: n,
        label: `${n} paired measurements`,
        formula: `For Bland-Altman analysis with SD of differences = ${sd}:\nExpected LoA = ±1.96 × ${sd} = ±${(1.96*sd).toFixed(1)}\nFor adequate precision of LoA CI: n ≈ ${n}\n\nNote: Minimum recommended n for Bland-Altman is 40 (Bland & Altman, 1986)`,
      };
    },
    rCode(p) {
      return `# Bland-Altman sample size
# For adequate precision of limits of agreement

sd_diff <- ${p.sd_diff}       # expected SD of differences
cat("Expected LoA: ±", round(1.96 * sd_diff, 2), "\\n")

# Rule of thumb: minimum 40 paired measurements
# For precise CI around LoA, use:
library(MethComp)
# or BlandAltmanLeh package

# Manual approach (Lu et al. 2016)
# n depends on desired precision of the LoA confidence interval
# A common recommendation is n ≥ 100 for narrow CIs,
# but ≥ 40 is acceptable for most clinical studies

# Simulation-based approach
library(SimplyAgree)
# SimplyAgree::agree_test() for agreement analysis`;
    },
    gpower() {
      return `Bland-Altman analysis is not a hypothesis test in the traditional sense —
it estimates limits of agreement. G*Power does not have a module for it.

Guidelines:
  • Minimum: 40 paired measurements (Bland & Altman, 1986)
  • Recommended: ≥ 100 for publication-quality CI widths
  • Use the R code for simulation-based approaches

Reference:
  Bland & Altman (1986) Lancet 1:307-310
  Lu et al. (2016) J Clin Epidemiol 75:93-100`;
    },
    sap(p, result) {
      if (!result) return "";
      return `For the Bland-Altman method comparison study, a minimum of ${result.n} paired measurements will be obtained. With an expected SD of differences of ${p.sd_diff}, the anticipated 95% limits of agreement are ±${(1.96*Number(p.sd_diff)).toFixed(1)}. This sample provides adequate precision for estimating the limits of agreement.`;
    },
  },
};

// ━━━ REVERSE LOOKUP: test key → calculator key ━━━━━━━━━━━
export function getCalcForTest(testKey) {
  for (const [calcId, calc] of Object.entries(CALCULATORS)) {
    if (calc.linkedTests.includes(testKey)) return calcId;
  }
  return null;
}

// ━━━ SENSITIVITY TABLE GENERATOR ━━━━━━━━━━━━━━━━━━━━━━━━

export function generateSensitivityTable(calcId, params) {
  const calc = CALCULATORS[calcId];
  if (!calc) return null;

  // Determine what to vary
  const powers = [0.80, 0.85, 0.90, 0.95];
  const effectSizes = [];

  // Build effect size variations based on calculator type
  if (calcId === "two_ind_cont" || calcId === "two_paired_cont" || calcId === "one_sample") {
    const d = params.input_mode === "cohen"
      ? Number(params.cohen_d)
      : calcId === "two_paired_cont"
        ? Number(params.mean_diff) / Number(params.sd_diff)
        : Number(params.mean_diff) / Number(params.sd);
    if (!d) return null;
    effectSizes.push(
      { label: `d = ${(d * 0.7).toFixed(2)}`, value: d * 0.7 },
      { label: `d = ${d.toFixed(2)}`, value: d, highlight: true },
      { label: `d = ${(d * 1.3).toFixed(2)}`, value: d * 1.3 },
    );
  } else if (calcId === "two_proportions") {
    const p1 = Number(params.p1), p2 = Number(params.p2);
    const diff = Math.abs(p2 - p1);
    effectSizes.push(
      { label: `Δp = ${(diff * 0.7).toFixed(2)}`, value: diff * 0.7 },
      { label: `Δp = ${diff.toFixed(2)}`, value: diff, highlight: true },
      { label: `Δp = ${(diff * 1.3).toFixed(2)}`, value: diff * 1.3 },
    );
  } else if (calcId === "correlation") {
    const r = Number(params.r);
    effectSizes.push(
      { label: `r = ${(r * 0.7).toFixed(2)}`, value: r * 0.7 },
      { label: `r = ${r.toFixed(2)}`, value: r, highlight: true },
      { label: `r = ${(r * 1.3 > 0.99 ? 0.95 : r * 1.3).toFixed(2)}`, value: Math.min(r * 1.3, 0.95) },
    );
  } else if (calcId === "logrank") {
    const hr = Number(params.hr);
    effectSizes.push(
      { label: `HR = ${(hr < 1 ? hr / 0.7 : hr * 0.7).toFixed(2)}`, value: hr < 1 ? hr / 0.7 : hr * 0.7 },
      { label: `HR = ${hr.toFixed(2)}`, value: hr, highlight: true },
      { label: `HR = ${(hr < 1 ? hr * 0.7 : hr * 1.3).toFixed(2)}`, value: hr < 1 ? hr * 0.7 : hr * 1.3 },
    );
  } else {
    return null; // No sensitivity table for some calculators
  }

  // Generate table
  const rows = effectSizes.map((es) => {
    const cols = powers.map((pw) => {
      const modParams = { ...params, power: pw };
      // Inject modified effect size
      if (calcId === "two_ind_cont" || calcId === "two_paired_cont" || calcId === "one_sample") {
        modParams.input_mode = "cohen";
        modParams.cohen_d = es.value;
      } else if (calcId === "two_proportions") {
        const base_p1 = Number(params.p1);
        modParams.p2 = base_p1 + es.value * Math.sign(Number(params.p2) - Number(params.p1));
      } else if (calcId === "correlation") {
        modParams.r = es.value;
      } else if (calcId === "logrank") {
        modParams.hr = es.value;
      }
      const result = calc.compute(modParams);
      return result ? result.total : "—";
    });
    return { label: es.label, highlight: es.highlight, cols };
  });

  return { headers: powers.map(pw => `${pw*100}%`), rows };
}
