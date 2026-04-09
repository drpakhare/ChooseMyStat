import { useState, useCallback } from "react";
import { TESTS, STEPS, recommend } from "./statTestsData";
import { Analytics } from "@vercel/analytics/react";

const DESCRIPTIVES = {
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

const DESCRIPTIVE_STEPS = [
  {
    id: "desc_goal",
    title: "What do you want to summarise?",
    subtitle: "Pick the type of descriptive analysis you need",
    options: [
      { value: "single_continuous", label: "A continuous variable", desc: "BP, HbA1c, weight, hospital stay, score", icon: "📏" },
      { value: "single_categorical", label: "A categorical / binary variable", desc: "Gender, disease stage, Yes/No outcome", icon: "📊" },
      { value: "table_one", label: "Baseline table (Table 1)", desc: "Summarise all variables by study group", icon: "📋" },
    ],
  },
  {
    id: "desc_distribution",
    title: "Is your continuous variable normally distributed?",
    subtitle: "Check with histogram, Q-Q plot, or Shapiro-Wilk test",
    show: (a) => a.desc_goal === "single_continuous",
    options: [
      { value: "normal", label: "Yes, Normal", desc: "Bell-shaped, Shapiro-Wilk p > 0.05", icon: "📐" },
      { value: "skewed", label: "No, Skewed", desc: "Non-normal, Shapiro-Wilk p < 0.05", icon: "📉" },
      { value: "not_sure", label: "Not sure yet", desc: "I'll check — show me how to assess normality", icon: "❓" },
    ],
  },
];

function recommendDescriptive(answers) {
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

// ─── UI Components ───

function OptionCard({ option, selected, onClick }) {
  const isSelected = selected === option.value;
  return (
    <button
      onClick={() => onClick(option.value)}
      className={`w-full text-left p-4 rounded-xl border-2 transition-all duration-200 ${
        isSelected
          ? "border-indigo-500 bg-indigo-50 shadow-md"
          : "border-gray-200 bg-white hover:border-indigo-300 hover:shadow-sm"
      }`}
    >
      <div className="flex items-start gap-3">
        <span className="text-2xl mt-0.5">{option.icon}</span>
        <div>
          <div className={`font-semibold text-base ${isSelected ? "text-indigo-700" : "text-gray-800"}`}>
            {option.label}
          </div>
          <div className="text-sm text-gray-500 mt-0.5">{option.desc}</div>
        </div>
      </div>
    </button>
  );
}

function ProgressDots({ total, current }) {
  return (
    <div className="flex justify-center gap-2 mb-6">
      {Array.from({ length: total }).map((_, i) => (
        <div
          key={i}
          className={`h-2 rounded-full transition-all duration-300 ${
            i === current ? "w-8 bg-indigo-500" : i < current ? "w-2 bg-indigo-300" : "w-2 bg-gray-300"
          }`}
        />
      ))}
    </div>
  );
}

// ─── Test Result Card with Tabs ───

function TestResult({ testKey, useTraditional }) {
  const t = TESTS[testKey];
  const [tab, setTab] = useState("sap");
  if (!t) return null;

  const tabs = [
    { id: "sap", label: "SAP Template" },
    { id: "example", label: "Results Example" },
    { id: "about", label: "About" },
    { id: "jasp", label: "In JASP" },
    { id: "r", label: "In R" },
  ];

  const sapText = useTraditional ? t.sapTraditional : t.sap;
  const exampleText = useTraditional ? t.exampleTraditional : t.example;

  return (
    <div className="bg-white rounded-2xl border border-gray-200 shadow-sm overflow-hidden mb-4">
      {/* Header */}
      <div className="bg-indigo-600 px-5 py-3">
        <h3 className="text-white font-bold text-lg">{t.name}</h3>
      </div>

      {/* Tab bar */}
      <div className="flex border-b border-gray-200 overflow-x-auto">
        {tabs.map((tb) => (
          <button
            key={tb.id}
            onClick={() => setTab(tb.id)}
            className={`flex-1 py-2.5 text-xs sm:text-sm font-medium transition-colors whitespace-nowrap px-2 ${
              tab === tb.id
                ? "text-indigo-600 border-b-2 border-indigo-600 bg-indigo-50"
                : "text-gray-500 hover:text-gray-700"
            }`}
          >
            {tb.label}
          </button>
        ))}
      </div>

      {/* Tab content */}
      <div className="p-5">
        {tab === "sap" && (
          <div>
            <p className="text-xs font-semibold text-indigo-600 uppercase tracking-wide mb-2">
              Copy and adapt for your protocol
              {useTraditional && (
                <span className="ml-2 text-amber-600 normal-case font-normal">(traditional style)</span>
              )}
            </p>
            <div className="bg-gray-50 border border-gray-200 rounded-lg p-4 text-sm text-gray-700 leading-relaxed font-serif italic whitespace-pre-line">
              {sapText}
            </div>
            <p className="text-xs text-gray-400 mt-3">
              Replace bracketed text [like this] with your specific variables.
            </p>
          </div>
        )}

        {tab === "example" && (
          <div>
            <p className="text-xs font-semibold text-green-600 uppercase tracking-wide mb-2">
              Model results paragraph
              {useTraditional && (
                <span className="ml-2 text-amber-600 normal-case font-normal">(traditional style)</span>
              )}
            </p>
            <div className="bg-green-50 border border-green-200 rounded-lg p-4 text-sm text-gray-700 leading-relaxed font-serif whitespace-pre-line">
              {exampleText}
            </div>
            <div className="mt-3 bg-gray-50 rounded-lg p-3">
              <p className="text-xs font-semibold text-gray-500 uppercase tracking-wide mb-1">What to report</p>
              <p className="text-sm text-gray-600">{t.report}</p>
            </div>
          </div>
        )}

        {tab === "about" && (
          <div className="space-y-3">
            <div>
              <p className="text-xs font-semibold text-gray-500 uppercase tracking-wide mb-1">When to use</p>
              <p className="text-sm text-gray-700">{t.when}</p>
            </div>
            <div>
              <p className="text-xs font-semibold text-gray-500 uppercase tracking-wide mb-1">Assumptions</p>
              <p className="text-sm text-gray-700">{t.assumptions}</p>
            </div>
          </div>
        )}

        {tab === "jasp" && (
          <div>
            <p className="text-xs font-semibold text-purple-600 uppercase tracking-wide mb-2">
              JASP Menu Path & Settings
            </p>
            <div className="bg-purple-50 border border-purple-200 rounded-lg p-4 text-sm text-gray-700 leading-relaxed whitespace-pre-line font-mono">
              {t.jasp}
            </div>
            <p className="text-xs text-gray-400 mt-3">
              JASP is free, open-source: <span className="text-indigo-500">jasp-stats.org</span>
            </p>
          </div>
        )}

        {tab === "r" && (
          <div>
            <p className="text-xs font-semibold text-blue-600 uppercase tracking-wide mb-2">
              R Code (tidyverse ecosystem)
            </p>
            <div className="bg-gray-900 rounded-lg p-4 text-sm text-green-300 leading-relaxed whitespace-pre-wrap font-mono overflow-x-auto">
              {t.r}
            </div>
            <p className="text-xs text-gray-400 mt-3">
              Adapt variable names (outcome, group, df) to your dataset. Key packages: gtsummary, sjPlot, finalfit, effectsize, lme4.
            </p>
          </div>
        )}
      </div>
    </div>
  );
}

// ─── Descriptive Result Card ───

function DescriptiveResult({ descKey, useTraditional }) {
  const d = DESCRIPTIVES[descKey];
  const [tab, setTab] = useState("sap");
  if (!d) return null;

  const tabs = [
    { id: "sap", label: "SAP Template" },
    { id: "summary", label: "How to Report" },
    { id: "jasp", label: "In JASP" },
    { id: "r", label: "In R" },
  ];

  const sapText = useTraditional ? d.sapTraditional : d.sap;

  return (
    <div className="bg-white rounded-2xl border border-gray-200 shadow-sm overflow-hidden mb-4">
      <div className="bg-teal-600 px-5 py-3">
        <h3 className="text-white font-bold text-lg">{d.name}</h3>
      </div>
      <div className="flex border-b border-gray-200 overflow-x-auto">
        {tabs.map((tb) => (
          <button
            key={tb.id}
            onClick={() => setTab(tb.id)}
            className={`flex-1 py-2.5 text-xs sm:text-sm font-medium transition-colors whitespace-nowrap px-2 ${
              tab === tb.id
                ? "text-teal-600 border-b-2 border-teal-600 bg-teal-50"
                : "text-gray-500 hover:text-gray-700"
            }`}
          >
            {tb.label}
          </button>
        ))}
      </div>
      <div className="p-5">
        {tab === "sap" && (
          <div>
            <p className="text-xs font-semibold text-teal-600 uppercase tracking-wide mb-2">
              Copy and adapt for your protocol
            </p>
            <div className="bg-gray-50 border border-gray-200 rounded-lg p-4 text-sm text-gray-700 leading-relaxed font-serif italic whitespace-pre-line">
              {sapText}
            </div>
          </div>
        )}
        {tab === "summary" && (
          <div className="space-y-3">
            <div>
              <p className="text-xs font-semibold text-gray-500 uppercase tracking-wide mb-1">Summary measure</p>
              <p className="text-sm text-gray-700 font-medium">{d.summary}</p>
            </div>
            <div>
              <p className="text-xs font-semibold text-gray-500 uppercase tracking-wide mb-1">Recommended visualisation</p>
              <p className="text-sm text-gray-700">{d.visual}</p>
            </div>
          </div>
        )}
        {tab === "jasp" && (
          <div>
            <p className="text-xs font-semibold text-purple-600 uppercase tracking-wide mb-2">JASP Menu Path</p>
            <div className="bg-purple-50 border border-purple-200 rounded-lg p-4 text-sm text-gray-700 leading-relaxed whitespace-pre-line font-mono">
              {d.jasp}
            </div>
          </div>
        )}
        {tab === "r" && (
          <div>
            <p className="text-xs font-semibold text-blue-600 uppercase tracking-wide mb-2">R Code (tidyverse)</p>
            <div className="bg-gray-900 rounded-lg p-4 text-sm text-green-300 leading-relaxed whitespace-pre-wrap font-mono overflow-x-auto">
              {d.r}
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

// ─── Main App ───

export default function ChooseMyStat() {
  // mode: null = home, "inferential" = test selection, "descriptive" = describe vars
  const [mode, setMode] = useState(null);
  const [answers, setAnswers] = useState({});
  const [stepIndex, setStepIndex] = useState(0);
  const [showResults, setShowResults] = useState(false);
  const [useTraditional, setUseTraditional] = useState(false);
  const [showContributors, setShowContributors] = useState(false);

  const activeSteps = mode === "descriptive" ? DESCRIPTIVE_STEPS : STEPS;
  const visibleSteps = activeSteps.filter((s) => !s.show || s.show(answers));
  const currentStep = visibleSteps[stepIndex];

  const handleSelect = useCallback(
    (value) => {
      const updated = { ...answers, [currentStep.id]: value };
      setAnswers(updated);

      setTimeout(() => {
        const nextVisible = activeSteps.filter((s) => !s.show || s.show(updated));
        const currentIdx = nextVisible.findIndex((s) => s.id === currentStep.id);
        if (currentIdx < nextVisible.length - 1) {
          setStepIndex(currentIdx + 1);
        } else {
          setShowResults(true);
        }
      }, 300);
    },
    [answers, currentStep, activeSteps]
  );

  const handleBack = () => {
    if (showResults) { setShowResults(false); return; }
    if (stepIndex > 0) { setStepIndex(stepIndex - 1); return; }
    // At first step → go back to home
    setMode(null);
    setAnswers({});
    setStepIndex(0);
  };

  const handleReset = () => {
    setMode(null);
    setAnswers({});
    setStepIndex(0);
    setShowResults(false);
  };

  const results = showResults
    ? (mode === "descriptive" ? recommendDescriptive(answers) : recommend(answers))
    : [];

  // ─── Home Screen ───
  if (mode === null) {
    return (
      <div className="min-h-screen bg-gradient-to-br from-slate-50 to-indigo-50 flex items-start justify-center p-4 pt-8">
        <div className="w-full max-w-xl">
          <div className="text-center mb-8">
            <h1 className="text-3xl font-bold text-gray-800">ChooseMyStat</h1>
            <p className="text-sm text-gray-500 mt-2">
              Your guide to descriptive summaries, statistical tests, and SAP templates
            </p>
          </div>

          <div className="space-y-4">
            <button
              onClick={() => setMode("descriptive")}
              className="w-full text-left bg-white rounded-2xl shadow-lg border-2 border-gray-100 hover:border-teal-400 hover:shadow-xl transition-all p-6 group"
            >
              <div className="flex items-start gap-4">
                <div className="text-3xl">📊</div>
                <div>
                  <div className="text-lg font-bold text-gray-800 group-hover:text-teal-700">
                    Describe My Variables
                  </div>
                  <div className="text-sm text-gray-500 mt-1">
                    Get the right summary measures, SAP text, and code for your descriptive analysis and Table 1
                  </div>
                </div>
              </div>
            </button>

            <button
              onClick={() => setMode("inferential")}
              className="w-full text-left bg-white rounded-2xl shadow-lg border-2 border-gray-100 hover:border-indigo-400 hover:shadow-xl transition-all p-6 group"
            >
              <div className="flex items-start gap-4">
                <div className="text-3xl">🧪</div>
                <div>
                  <div className="text-lg font-bold text-gray-800 group-hover:text-indigo-700">
                    Choose a Statistical Test
                  </div>
                  <div className="text-sm text-gray-500 mt-1">
                    Answer a few questions about your data and get the recommended test with SAP templates, JASP steps, and R code
                  </div>
                </div>
              </div>
            </button>
          </div>

          {/* Contributors */}
          <div className="mt-10 text-center">
            <button
              onClick={() => setShowContributors(!showContributors)}
              className="text-xs text-gray-400 hover:text-gray-600 transition-colors"
            >
              {showContributors ? "Hide contributors" : "Contributors"}
            </button>
            {showContributors && (
              <div className="mt-3 bg-white rounded-xl border border-gray-200 p-4 text-left">
                <p className="text-xs font-semibold text-gray-500 uppercase tracking-wide mb-3">Contributors</p>
                <div className="space-y-2">
                  <div className="flex items-center gap-3">
                    <div className="w-8 h-8 rounded-full bg-indigo-100 flex items-center justify-center text-indigo-600 font-bold text-sm">AP</div>
                    <div>
                      <p className="text-sm font-medium text-gray-800">Dr Abhijit Pakhare</p>
                      <p className="text-xs text-gray-400">Clinical Epidemiology Unit, AIIMS Bhopal</p>
                    </div>
                  </div>
                  <div className="flex items-center gap-3">
                    <div className="w-8 h-8 rounded-full bg-teal-100 flex items-center justify-center text-teal-600 font-bold text-sm">AJ</div>
                    <div>
                      <p className="text-sm font-medium text-gray-800">Dr Ankur Joshi</p>
                      <p className="text-xs text-gray-400">Clinical Epidemiology Unit, AIIMS Bhopal</p>
                    </div>
                  </div>
                  <div className="flex items-center gap-3">
                    <div className="w-8 h-8 rounded-full bg-purple-100 flex items-center justify-center text-purple-600 font-bold text-sm">AI</div>
                    <div>
                      <p className="text-sm font-medium text-gray-800">Claude (Anthropic)</p>
                      <p className="text-xs text-gray-400">AI assistant — decision tree, SAP templates, and code</p>
                    </div>
                  </div>
                </div>
              </div>
            )}
            <p className="text-xs text-gray-300 mt-3">
              AIIMS Bhopal · Clinical Epidemiology Unit
            </p>
          </div>
        </div>
      </div>
    );
  }

  // ─── Question / Results Flow (shared by both modes) ───
  const accentColor = mode === "descriptive" ? "teal" : "indigo";

  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-50 to-indigo-50 flex items-start justify-center p-4 pt-8">
      <div className="w-full max-w-xl">
        {/* Header */}
        <div className="text-center mb-6">
          <button onClick={handleReset} className="text-2xl font-bold text-gray-800 hover:text-indigo-600 transition-colors">
            ChooseMyStat
          </button>
          <p className="text-sm text-gray-500 mt-1">
            {mode === "descriptive" ? "Describe My Variables" : "Choose a Statistical Test"}
          </p>
        </div>

        {!showResults ? (
          <div>
            <ProgressDots total={visibleSteps.length} current={stepIndex} />

            {/* Question card */}
            <div className="bg-white rounded-2xl shadow-lg border border-gray-100 p-6 mb-4">
              <h2 className="text-lg font-bold text-gray-800 mb-1">{currentStep.title}</h2>
              <p className="text-sm text-gray-500 mb-5">{currentStep.subtitle}</p>
              <div className="space-y-3">
                {currentStep.options.map((opt) => (
                  <OptionCard
                    key={opt.value}
                    option={opt}
                    selected={answers[currentStep.id]}
                    onClick={handleSelect}
                  />
                ))}
              </div>
            </div>

            {/* Navigation */}
            <div className="flex justify-between items-center px-1">
              <button
                onClick={handleBack}
                className="text-sm text-indigo-600 font-medium hover:text-indigo-800"
              >
                ← {stepIndex === 0 ? "Home" : "Back"}
              </button>
              <span className="text-xs text-gray-400">
                Step {stepIndex + 1} of {visibleSteps.length}
              </span>
            </div>
          </div>
        ) : (
          <div>
            {/* Results header */}
            <div className="bg-white rounded-2xl shadow-lg border border-gray-100 p-5 mb-5">
              <h2 className="text-lg font-bold text-gray-800 mb-1">
                {mode === "descriptive" ? "Recommended Summary" : "Recommended Analysis"}
              </h2>
              <p className="text-sm text-gray-500 mb-1">Based on your inputs:</p>
              <div className="flex flex-wrap gap-2 mb-4">
                {Object.entries(answers).map(([k, v]) => (
                  <span key={k} className="bg-indigo-100 text-indigo-700 text-xs font-medium px-2.5 py-1 rounded-full">
                    {v.replace(/_/g, " ")}
                  </span>
                ))}
              </div>

              {mode === "inferential" && results.length > 1 && (
                <p className="text-xs text-gray-400 italic mb-3">
                  Multiple tests: use the unadjusted test first, then the regression model for adjusted analysis.
                </p>
              )}

              {/* Reporting style toggle */}
              <div className="flex items-center justify-between bg-gray-50 rounded-lg px-3 py-2">
                <span className="text-xs text-gray-600 font-medium">Reporting style:</span>
                <div className="flex items-center bg-white rounded-lg border border-gray-200 overflow-hidden">
                  <button
                    onClick={() => setUseTraditional(false)}
                    className={`px-3 py-1.5 text-xs font-medium transition-colors ${
                      !useTraditional ? "bg-indigo-600 text-white" : "text-gray-500 hover:text-gray-700"
                    }`}
                  >
                    Effect-first (ASA)
                  </button>
                  <button
                    onClick={() => setUseTraditional(true)}
                    className={`px-3 py-1.5 text-xs font-medium transition-colors ${
                      useTraditional ? "bg-amber-500 text-white" : "text-gray-500 hover:text-gray-700"
                    }`}
                  >
                    Traditional (p &lt; 0.05)
                  </button>
                </div>
              </div>

              {useTraditional && (
                <p className="text-xs text-amber-700 bg-amber-50 rounded-lg px-3 py-2 mt-2 leading-relaxed">
                  The ASA's 2016 statement discourages rigid reliance on p &lt; 0.05 as a bright-line threshold. Traditional templates are shown for reference, as many journals and IECs still expect this style.
                </p>
              )}
            </div>

            {/* Result cards */}
            {mode === "descriptive"
              ? results.map((key) => <DescriptiveResult key={key} descKey={key} useTraditional={useTraditional} />)
              : results.map((key) => <TestResult key={key} testKey={key} useTraditional={useTraditional} />)
            }

            {results.length === 0 && (
              <div className="bg-yellow-50 border border-yellow-200 rounded-xl p-5 text-center">
                <p className="text-yellow-800 font-medium">No recommendation could be determined.</p>
                <p className="text-sm text-yellow-600 mt-1">Try adjusting your inputs or consult your biostatistician.</p>
              </div>
            )}

            {/* Actions */}
            <div className="flex justify-between mt-5 px-1">
              <button onClick={handleBack} className="text-sm text-indigo-600 font-medium hover:text-indigo-800">
                ← Back
              </button>
              <button
                onClick={handleReset}
                className="bg-indigo-600 text-white text-sm font-medium px-5 py-2.5 rounded-xl hover:bg-indigo-700 transition-colors shadow-sm"
              >
                Start Over
              </button>
            </div>
          </div>
        )}

        {/* Footer */}
        <div className="text-center mt-8 mb-4">
          <p className="text-xs text-gray-400">AIIMS Bhopal · Clinical Epidemiology Unit</p>
        </div>
      </div>
      <Analytics />
    </div>
  );
}
