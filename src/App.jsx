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

function Breadcrumbs({ answers, steps }) {
  const answered = steps.filter((s) => answers[s.id] !== undefined);
  if (answered.length === 0) return null;
  return (
    <div className="flex flex-wrap items-center gap-1 mb-3 px-1">
      {answered.map((step, i) => {
        const chosen = step.options.find((o) => o.value === answers[step.id]);
        return (
          <span key={step.id} className="flex items-center gap-1">
            {i > 0 && <span className="text-gray-300 text-xs">›</span>}
            <span className="bg-indigo-100 text-indigo-700 text-xs font-medium px-2 py-0.5 rounded-full">
              {chosen?.label || answers[step.id]}
            </span>
          </span>
        );
      })}
    </div>
  );
}

function SlideTransition({ children, stepKey, direction }) {
  const [animClass, setAnimClass] = useState("opacity-100 translate-x-0");
  const prevKey = useRef(stepKey);

  useEffect(() => {
    if (prevKey.current !== stepKey) {
      const enter = direction === "forward" ? "translate-x-8" : "-translate-x-8";
      setAnimClass(`opacity-0 ${enter}`);
      const t = setTimeout(() => setAnimClass("opacity-100 translate-x-0"), 30);
      prevKey.current = stepKey;
      return () => clearTimeout(t);
    }
  }, [stepKey, direction]);

  return (
    <div className={`transform transition-all duration-300 ease-out ${animClass}`}>
      {children}
    </div>
  );
}

function OptionCard({ option, selected, onClick, dark }) {
  const isSelected = selected === option.value;
  return (
    <button
      onClick={() => onClick(option.value)}
      className={`w-full text-left p-4 rounded-xl border-2 transition-all duration-200 ${
        isSelected
          ? dark ? "border-indigo-500 bg-indigo-950 shadow-md" : "border-indigo-500 bg-indigo-50 shadow-md"
          : dark ? "border-gray-700 bg-gray-800 hover:border-indigo-500 hover:shadow-sm" : "border-gray-200 bg-white hover:border-indigo-300 hover:shadow-sm"
      }`}
    >
      <div className="flex items-start gap-3">
        <div className={`flex-shrink-0 mt-0.5 ${isSelected ? "text-indigo-400" : dark ? "text-gray-500" : "text-gray-400"}`}>
          {typeof option.icon === "string" ? ICONS[option.icon] || <span className="text-2xl">{option.icon}</span> : option.icon}
        </div>
        <div>
          <div className={`font-semibold text-base ${isSelected ? (dark ? "text-indigo-300" : "text-indigo-700") : (dark ? "text-gray-200" : "text-gray-800")}`}>
            {option.label}
          </div>
          <div className={`text-sm mt-0.5 ${dark ? "text-gray-400" : "text-gray-500"}`}>{option.desc}</div>
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
    { id: "jasp", label: "In JASP" },
    { id: "r", label: "In R" },
  ];

  const sapText = useTraditional ? t.sapTraditional : t.sap;
  const exampleText = useTraditional ? t.exampleTraditional : t.example;

  return (
    <div className="rounded-2xl border border-gray-200 dark:border-gray-700 shadow-sm overflow-hidden mb-4" style={{ background: "var(--card-bg, white)" }}>
      {/* Header */}
      <div className="bg-indigo-600 px-5 py-3">
        <h3 className="text-white font-bold text-lg">{t.name}</h3>
      </div>

      {/* When to use & Assumptions - always visible */}
      <div className="px-5 py-3 border-b border-gray-100" style={{ background: "var(--card-bg, white)" }}>
        <p className="text-sm text-gray-700 leading-relaxed">{t.when}</p>
        <p className="text-xs text-gray-500 mt-1.5"><span className="font-semibold">Assumptions:</span> {t.assumptions}</p>
      </div>

      {/* Tab bar */}
      <div className="flex border-b border-gray-200 overflow-x-auto scrollbar-hide" style={{ scrollbarWidth: "none", msOverflowStyle: "none", WebkitOverflowScrolling: "touch" }}>
        {tabs.map((tb) => (
          <button
            key={tb.id}
            onClick={() => setTab(tb.id)}
            className={`flex-1 min-w-0 py-3 text-xs sm:text-sm font-medium transition-colors whitespace-nowrap px-3 ${
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
              Statistical Analysis Plan (SAP) — copy and adapt for your protocol
              {useTraditional && (
                <span className="ml-2 text-amber-600 normal-case font-normal">(traditional style)</span>
              )}
            </p>
            <div className="relative">
              <CopyButton text={sapText} />
              <div className="bg-gray-50 border border-gray-200 rounded-lg p-4 pr-20 text-sm text-gray-700 leading-relaxed font-serif italic whitespace-pre-line">
                {sapText}
              </div>
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
            <div className="relative">
              <CopyButton text={exampleText} />
              <div className="bg-green-50 border border-green-200 rounded-lg p-4 pr-20 text-sm text-gray-700 leading-relaxed font-serif whitespace-pre-line">
                {exampleText}
              </div>
            </div>
            <div className="mt-3 bg-gray-50 rounded-lg p-3">
              <p className="text-xs font-semibold text-gray-500 uppercase tracking-wide mb-1">What to report</p>
              <p className="text-sm text-gray-600">{t.report}</p>
            </div>
          </div>
        )}


        {tab === "jasp" && (
          <div>
            <p className="text-xs font-semibold text-purple-600 uppercase tracking-wide mb-2">
              JASP Menu Path & Settings
            </p>
            <div className="relative">
              <CopyButton text={t.jasp} />
              <div className="bg-purple-50 border border-purple-200 rounded-lg p-4 pr-20 text-sm text-gray-700 leading-relaxed whitespace-pre-line font-mono">
                {t.jasp}
              </div>
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
            <div className="relative">
              <CopyButton text={t.r} />
              <div className="bg-gray-900 rounded-lg p-4 pr-20 text-sm text-green-300 leading-relaxed whitespace-pre-wrap font-mono overflow-x-auto">
                {t.r}
              </div>
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
    <div className="rounded-2xl border border-gray-200 dark:border-gray-700 shadow-sm overflow-hidden mb-4" style={{ background: "var(--card-bg, white)" }}>
      <div className="bg-teal-600 px-5 py-3">
        <h3 className="text-white font-bold text-lg">{d.name}</h3>
      </div>
      <div className="flex border-b border-gray-200 overflow-x-auto scrollbar-hide" style={{ scrollbarWidth: "none", msOverflowStyle: "none", WebkitOverflowScrolling: "touch" }}>
        {tabs.map((tb) => (
          <button
            key={tb.id}
            onClick={() => setTab(tb.id)}
            className={`flex-1 min-w-0 py-3 text-xs sm:text-sm font-medium transition-colors whitespace-nowrap px-3 ${
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
            <div className="relative">
              <CopyButton text={sapText} />
              <div className="bg-gray-50 border border-gray-200 rounded-lg p-4 pr-20 text-sm text-gray-700 leading-relaxed font-serif italic whitespace-pre-line">
                {sapText}
              </div>
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
            <div className="relative">
              <CopyButton text={d.jasp} />
              <div className="bg-purple-50 border border-purple-200 rounded-lg p-4 pr-20 text-sm text-gray-700 leading-relaxed whitespace-pre-line font-mono">
                {d.jasp}
              </div>
            </div>
          </div>
        )}
        {tab === "r" && (
          <div>
            <p className="text-xs font-semibold text-blue-600 uppercase tracking-wide mb-2">R Code (tidyverse)</p>
            <div className="relative">
              <CopyButton text={d.r} />
              <div className="bg-gray-900 rounded-lg p-4 pr-20 text-sm text-green-300 leading-relaxed whitespace-pre-wrap font-mono overflow-x-auto">
                {d.r}
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

// ─── Hex Logo (R-package style — Flowchart Diamond) ───
function HexLogo({ dark }) {
  return (
    <svg viewBox="0 0 200 230" className="w-28 h-32 mx-auto mb-4 drop-shadow-lg">
      <defs>
        <linearGradient id="hexGrad" x1="0%" y1="0%" x2="100%" y2="100%">
          <stop offset="0%" stopColor={dark ? "#6366f1" : "#4338ca"} />
          <stop offset="50%" stopColor={dark ? "#818cf8" : "#4f46e5"} />
          <stop offset="100%" stopColor={dark ? "#a78bfa" : "#7c3aed"} />
        </linearGradient>
      </defs>
      {/* Hex shape */}
      <polygon
        points="100,8 190,58 190,172 100,222 10,172 10,58"
        fill="url(#hexGrad)"
        stroke={dark ? "#a5b4fc" : "#312e81"}
        strokeWidth="4"
      />
      {/* Input pill */}
      <rect x="75" y="38" width="50" height="20" rx="10" fill="none" stroke="white" strokeWidth="2.5" />
      {/* Line to diamond */}
      <line x1="100" y1="58" x2="100" y2="68" stroke="white" strokeWidth="2" />
      {/* Decision diamond */}
      <polygon points="100,68 130,90 100,112 70,90" fill="none" stroke="white" strokeWidth="2.5" />
      <text x="100" y="94" textAnchor="middle" fill="white" fontSize="11" fontFamily="system-ui, sans-serif" fontWeight="700">?</text>
      {/* Yes path (right) */}
      <line x1="130" y1="90" x2="150" y2="90" stroke="white" strokeWidth="2" />
      <rect x="143" y="80" width="30" height="18" rx="4" fill="white" opacity="0.25" />
      <text x="158" y="93" textAnchor="middle" fill="white" fontSize="9" fontFamily="system-ui, sans-serif" fontWeight="700">Yes</text>
      {/* No path (left) */}
      <line x1="70" y1="90" x2="50" y2="90" stroke="white" strokeWidth="2" />
      <rect x="27" y="80" width="30" height="18" rx="4" fill="white" opacity="0.25" />
      <text x="42" y="93" textAnchor="middle" fill="white" fontSize="9" fontFamily="system-ui, sans-serif" fontWeight="700">No</text>
      {/* Down to result */}
      <line x1="100" y1="112" x2="100" y2="122" stroke="white" strokeWidth="2" />
      {/* Result box with checkmark */}
      <rect x="78" y="122" width="44" height="22" rx="4" fill="white" opacity="0.25" stroke="white" strokeWidth="2" />
      <path d="M90 133 l4 4 l10-10" stroke="white" strokeWidth="2.5" fill="none" strokeLinecap="round" strokeLinejoin="round" />
      {/* Text */}
      <text x="100" y="168" textAnchor="middle" fill="white" fontFamily="system-ui, sans-serif" fontWeight="800" fontSize="20" letterSpacing="1">Choose</text>
      <text x="100" y="191" textAnchor="middle" fill="white" fontFamily="system-ui, sans-serif" fontWeight="800" fontSize="20" letterSpacing="1">MyStat</text>
      <text x="100" y="210" textAnchor="middle" fill="rgba(255,255,255,0.4)" fontFamily="system-ui, sans-serif" fontSize="10">v1.0</text>
    </svg>
  );
}

// ─── Dark Mode Toggle Button ───
function DarkToggle({ dark, setDark }) {
  return (
    <button
      onClick={() => setDark(!dark)}
      className={`fixed top-4 right-4 z-50 p-2 rounded-full border transition-all duration-200 ${
        dark
          ? "bg-gray-800 border-gray-600 text-yellow-400 hover:bg-gray-700"
          : "bg-white border-gray-200 text-gray-500 hover:bg-gray-50 shadow-sm"
      }`}
      title={dark ? "Light mode" : "Dark mode"}
    >
      {dark ? (
        <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="w-5 h-5">
          <circle cx="12" cy="12" r="5" /><line x1="12" y1="1" x2="12" y2="3" /><line x1="12" y1="21" x2="12" y2="23" /><line x1="4.22" y1="4.22" x2="5.64" y2="5.64" /><line x1="18.36" y1="18.36" x2="19.78" y2="19.78" /><line x1="1" y1="12" x2="3" y2="12" /><line x1="21" y1="12" x2="23" y2="12" /><line x1="4.22" y1="19.78" x2="5.64" y2="18.36" /><line x1="18.36" y1="5.64" x2="19.78" y2="4.22" />
        </svg>
      ) : (
        <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="w-5 h-5">
          <path d="M21 12.79A9 9 0 1111.21 3 7 7 0 0021 12.79z" />
        </svg>
      )}
    </button>
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
  const [slideDirection, setSlideDirection] = useState("forward");
  // ─── Analysis Plan Builder (session memory) ───
  const [planItems, setPlanItems] = useState([]);
  const [showPlan, setShowPlan] = useState(false);
  const [planTitle, setPlanTitle] = useState("");
  const [planDesign, setPlanDesign] = useState("");
  const [showGlossary, setShowGlossary] = useState(false);
  const [dark, setDark] = useState(false);

  // ─── Apply dark mode CSS variable for result cards ───
  useEffect(() => {
    document.documentElement.style.setProperty("--card-bg", dark ? "#111827" : "white");
  }, [dark]);

  // ─── URL hash state: restore from hash on load, update hash on results ───
  useEffect(() => {
    const hash = window.location.hash.slice(1);
    if (!hash) return;
    try {
      const params = Object.fromEntries(new URLSearchParams(hash));
      if (params.mode) {
        setMode(params.mode);
        const restored = {};
        Object.entries(params).forEach(([k, v]) => { if (k !== "mode") restored[k] = v; });
        setAnswers(restored);
        setShowResults(true);
      }
    } catch {}
  }, []);

  useEffect(() => {
    if (showResults && mode) {
      const params = new URLSearchParams({ mode, ...answers });
      window.location.hash = params.toString();
    } else if (mode === null) {
      if (window.location.hash) window.history.replaceState(null, "", window.location.pathname);
    }
  }, [showResults, mode, answers]);

  const activeSteps = mode === "descriptive" ? DESCRIPTIVE_STEPS : STEPS;
  const visibleSteps = activeSteps.filter((s) => !s.show || s.show(answers));
  const currentStep = visibleSteps[stepIndex];

  const handleSelect = useCallback(
    (value) => {
      const updated = { ...answers, [currentStep.id]: value };
      setAnswers(updated);
      setSlideDirection("forward");

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
    setSlideDirection("back");
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

  // ─── Plan Builder helpers ───
  const addToPlan = () => {
    const curResults = mode === "descriptive" ? recommendDescriptive(answers) : recommend(answers);
    const item = {
      id: Date.now(),
      mode,
      answers: { ...answers },
      resultKeys: curResults,
      useTraditional,
      reasoning: mode === "inferential" ? explainReasoning(answers) : null,
    };
    setPlanItems((prev) => [...prev, item]);
    setShowPlan(true);
  };

  const removePlanItem = (id) => setPlanItems((prev) => prev.filter((p) => p.id !== id));

  const movePlanItem = (idx, dir) => {
    setPlanItems((prev) => {
      const a = [...prev];
      const ni = idx + dir;
      if (ni < 0 || ni >= a.length) return a;
      [a[idx], a[ni]] = [a[ni], a[idx]];
      return a;
    });
  };

  const generateConsolidatedSAP = () => {
    const sep = "═".repeat(50);
    const lines = [
      sep,
      "CONSOLIDATED STATISTICAL ANALYSIS PLAN",
      sep,
      "",
    ];
    if (planTitle) lines.push(`Study Title: ${planTitle}`, "");
    if (planDesign) lines.push(`Study Design: ${planDesign}`, "");
    lines.push(`Date: ${new Date().toLocaleDateString("en-IN", { day: "numeric", month: "long", year: "numeric" })}`, "");
    lines.push(`Total objectives/analyses: ${planItems.length}`, "");
    lines.push("─".repeat(50), "");

    planItems.forEach((item, i) => {
      const num = i + 1;
      const testData = item.mode === "descriptive"
        ? item.resultKeys.map((k) => DESCRIPTIVES[k])
        : item.resultKeys.map((k) => TESTS[k]);
      const inputStr = Object.entries(item.answers).map(([k, v]) => `${k.replace(/_/g, " ")}: ${v.replace(/_/g, " ")}`).join(", ");

      lines.push(`OBJECTIVE ${num}`, "─".repeat(30));
      lines.push(`Type: ${item.mode === "descriptive" ? "Descriptive Analysis" : "Inferential Test"}`);
      lines.push(`Inputs: ${inputStr}`);
      if (item.reasoning) lines.push(`Reasoning: ${item.reasoning.replace(/\*\*/g, "")}`);
      lines.push("");

      testData.forEach((t) => {
        if (!t) return;
        lines.push(`  Test/Method: ${t.name}`);
        lines.push(`  SAP Template:`);
        const sap = item.useTraditional ? (t.sapTraditional || t.sap) : t.sap;
        lines.push(`    ${sap}`);
        lines.push("");
        if (t.report) lines.push(`  Reporting: ${t.report}`, "");
        if (t.r) lines.push(`  R Code:`, `    ${t.r.split("\n").join("\n    ")}`, "");
        if (t.jasp) lines.push(`  JASP:`, `    ${t.jasp.split("\n").join("\n    ")}`, "");
      });
      lines.push("");
    });

    lines.push("─".repeat(50));
    lines.push("Generated by ChooseMyStat · Clinical Epidemiology Unit, AIIMS Bhopal");
    lines.push("https://choose-my-stat.vercel.app");

    const blob = new Blob([lines.join("\n")], { type: "text/plain" });
    const a = document.createElement("a");
    a.href = URL.createObjectURL(blob);
    a.download = `SAP_${planTitle ? planTitle.replace(/\s+/g, "_").slice(0, 30) : "ConsolidatedPlan"}.txt`;
    a.click();
  };

  const results = showResults
    ? (mode === "descriptive" ? recommendDescriptive(answers) : recommend(answers))
    : [];

  // ─── Shared dark-mode class helpers ───
  const bg = dark ? "bg-gray-950" : "bg-gradient-to-br from-slate-50 to-indigo-50";
  const card = dark ? "bg-gray-900 border-gray-700" : "bg-white border-gray-100";
  const cardHoverTeal = dark ? "hover:border-teal-500" : "hover:border-teal-400";
  const cardHoverIndigo = dark ? "hover:border-indigo-500" : "hover:border-indigo-400";
  const textPrimary = dark ? "text-gray-100" : "text-gray-800";
  const textSecondary = dark ? "text-gray-400" : "text-gray-500";
  const textMuted = dark ? "text-gray-500" : "text-gray-400";
  const textFaint = dark ? "text-gray-600" : "text-gray-300";

  // ─── Home Screen ───
  if (mode === null) {
    return (
      <div className={`min-h-screen ${bg} flex items-start justify-center p-4 pt-8 transition-colors duration-300`}>
        <DarkToggle dark={dark} setDark={setDark} />
        <div className="w-full max-w-xl">
          <div className="text-center mb-6">
            <HexLogo dark={dark} />
            <p className={`text-sm ${textSecondary} mt-1`}>
              Statistical test advisor for clinical researchers — with Statistical Analysis Plan (SAP) templates, results reporting, and ready-to-use code
            </p>
          </div>

          {/* ─── How to Use ─── */}
          <div className={`rounded-2xl border shadow-sm overflow-hidden mb-6 ${dark ? "bg-gray-900 border-gray-700" : "bg-white border-gray-200"}`}>
            <div className="px-5 pt-4 pb-1">
              <p className={`text-xs font-bold uppercase tracking-wide ${dark ? "text-gray-400" : "text-gray-500"}`}>How to use</p>
            </div>
            <div className="px-5 pb-4 pt-2">
              {[
                { num: "1", icon: "📋", title: "Pick a mode", desc: "Choose whether you need descriptive summaries (Table 1) or an inferential statistical test." },
                { num: "2", icon: "❓", title: "Answer a few questions", desc: "Tell us your outcome type, number of groups, pairing, and whether you need to adjust for confounders." },
                { num: "3", icon: "✅", title: "Get your recommendation", desc: "Receive the right test or summary — with a SAP template paragraph, results example, JASP steps, and R code." },
                { num: "4", icon: "📥", title: "Copy or download", desc: "Paste the SAP text into your protocol, use the R code in your script, or download everything as a text file." },
              ].map((s, i) => (
                <div key={i} className="flex items-start gap-3 py-2.5">
                  <span className={`text-base w-7 h-7 flex items-center justify-center rounded-full flex-shrink-0 font-bold ${dark ? "bg-indigo-900/50 text-indigo-300" : "bg-indigo-100 text-indigo-600"}`} style={{ fontSize: "13px" }}>{s.num}</span>
                  <div className="min-w-0">
                    <p className={`text-sm font-semibold ${dark ? "text-gray-200" : "text-gray-800"}`}>{s.title}</p>
                    <p className={`text-xs leading-relaxed mt-0.5 ${dark ? "text-gray-500" : "text-gray-500"}`}>{s.desc}</p>
                  </div>
                </div>
              ))}
            </div>
          </div>

          <div className="space-y-4">
            <button
              onClick={() => setMode("descriptive")}
              className={`w-full text-left rounded-2xl shadow-lg border-2 ${card} ${cardHoverTeal} hover:shadow-xl transition-all p-6 group`}
            >
              <div className="flex items-start gap-4">
                <div className="text-teal-500 group-hover:text-teal-400 transition-colors">{ICONS.describe}</div>
                <div>
                  <div className={`text-lg font-bold ${textPrimary} group-hover:text-teal-500`}>
                    Describe My Variables
                  </div>
                  <div className={`text-sm ${textSecondary} mt-1`}>
                    Summary measures, SAP text, and code for descriptive analysis and Table 1
                  </div>
                </div>
              </div>
            </button>

            <button
              onClick={() => setMode("inferential")}
              className={`w-full text-left rounded-2xl shadow-lg border-2 ${card} ${cardHoverIndigo} hover:shadow-xl transition-all p-6 group`}
            >
              <div className="flex items-start gap-4">
                <div className="text-indigo-400 group-hover:text-indigo-300 transition-colors">{ICONS.test}</div>
                <div>
                  <div className={`text-lg font-bold ${textPrimary} group-hover:text-indigo-400`}>
                    Choose a Statistical Test
                  </div>
                  <div className={`text-sm ${textSecondary} mt-1`}>
                    Answer a few questions about your study design and get the right test with SAP template, results text, JASP steps, and R code
                  </div>
                </div>
              </div>
            </button>
          </div>

          {/* ─── Core Concepts ─── */}
          <div className={`mt-6 rounded-2xl border shadow-sm overflow-hidden ${dark ? "bg-gray-900 border-gray-700" : "bg-white border-gray-200"}`}>
            <div className="px-5 pt-4 pb-1">
              <p className={`text-xs font-bold uppercase tracking-wide ${dark ? "text-gray-400" : "text-gray-500"}`}>Core concepts</p>
              <p className={`text-xs mt-1 ${dark ? "text-gray-600" : "text-gray-400"}`}>Key terms you'll encounter while using this tool</p>
            </div>
            <div className="px-5 pb-4 pt-2 grid grid-cols-1 sm:grid-cols-2 gap-x-5 gap-y-3">
              {[
                { term: "Outcome (Dependent Variable)", def: "The variable you're measuring or trying to explain — e.g., blood pressure, mortality, recovery time." },
                { term: "Continuous vs Categorical", def: "Continuous data can take any numeric value (weight, BP). Categorical data falls into groups (gender, disease stage, yes/no)." },
                { term: "Paired vs Independent", def: "Paired: same subjects measured twice (before/after). Independent: different subjects in each group." },
                { term: "Parametric vs Non-parametric", def: "Parametric tests assume normally distributed data (t-test, ANOVA). Non-parametric tests don't (Mann-Whitney, Kruskal-Wallis)." },
                { term: "Confounders / Covariates", def: "Variables that could distort the relationship you're studying. Adjusting for them requires regression or stratified analysis." },
                { term: "Normality", def: "Whether your data follows a bell-shaped distribution. Check with Shapiro-Wilk test or Q-Q plots — matters for choosing parametric vs non-parametric." },
                { term: "p-value", def: "Probability of seeing your result (or more extreme) if the null hypothesis were true. Smaller p = stronger evidence against the null — but not a measure of effect size." },
                { term: "Effect Size", def: "How large the difference or association is (e.g., Cohen's d, odds ratio, correlation r). Always report alongside p-values." },
              ].map((c, i) => (
                <div key={i} className="py-1.5">
                  <p className={`text-sm font-semibold ${dark ? "text-indigo-300" : "text-indigo-700"}`}>{c.term}</p>
                  <p className={`text-xs leading-relaxed mt-0.5 ${dark ? "text-gray-400" : "text-gray-600"}`}>{c.def}</p>
                </div>
              ))}
            </div>
          </div>

          {/* ─── Plan Builder Panel ─── */}
          {planItems.length > 0 && (
            <div className={`mt-6 rounded-2xl border-2 shadow-lg p-5 ${dark ? "bg-emerald-950 border-emerald-800" : "bg-emerald-50 border-emerald-200"}`}>
              <button
                onClick={() => setShowPlan(!showPlan)}
                className="w-full flex items-center justify-between"
              >
                <div className="flex items-center gap-2">
                  <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className={`w-5 h-5 ${dark ? "text-emerald-400" : "text-emerald-600"}`}>
                    <path d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2" />
                    <rect x="9" y="3" width="6" height="4" rx="1" />
                    <path d="M9 14l2 2 4-4" />
                  </svg>
                  <span className={`text-sm font-bold ${dark ? "text-emerald-300" : "text-emerald-800"}`}>
                    Analysis Plan ({planItems.length} {planItems.length === 1 ? "item" : "items"})
                  </span>
                </div>
                <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" className={`w-4 h-4 transition-transform ${showPlan ? "rotate-180" : ""} ${dark ? "text-emerald-400" : "text-emerald-600"}`}>
                  <polyline points="6 9 12 15 18 9" />
                </svg>
              </button>

              {showPlan && (
                <div className="mt-4 space-y-3">
                  {/* Study header inputs */}
                  <div className="space-y-2">
                    <input
                      type="text"
                      value={planTitle}
                      onChange={(e) => setPlanTitle(e.target.value)}
                      placeholder="Study title (optional)"
                      className={`w-full text-sm rounded-lg px-3 py-2 border outline-none focus:ring-2 focus:ring-emerald-400 ${dark ? "bg-gray-900 border-gray-700 text-gray-200 placeholder-gray-600" : "bg-white border-gray-200 text-gray-800 placeholder-gray-400"}`}
                    />
                    <input
                      type="text"
                      value={planDesign}
                      onChange={(e) => setPlanDesign(e.target.value)}
                      placeholder="Study design, e.g. Cross-sectional, RCT (optional)"
                      className={`w-full text-sm rounded-lg px-3 py-2 border outline-none focus:ring-2 focus:ring-emerald-400 ${dark ? "bg-gray-900 border-gray-700 text-gray-200 placeholder-gray-600" : "bg-white border-gray-200 text-gray-800 placeholder-gray-400"}`}
                    />
                  </div>

                  {/* Saved items list */}
                  <div className="space-y-2">
                    {planItems.map((item, idx) => {
                      const testNames = item.mode === "descriptive"
                        ? item.resultKeys.map((k) => DESCRIPTIVES[k]?.name || k)
                        : item.resultKeys.map((k) => TESTS[k]?.name || k);
                      return (
                        <div key={item.id} className={`flex items-center gap-2 rounded-lg px-3 py-2 ${dark ? "bg-gray-900" : "bg-white"}`}>
                          <span className={`text-xs font-bold w-6 h-6 flex items-center justify-center rounded-full ${dark ? "bg-emerald-900 text-emerald-300" : "bg-emerald-200 text-emerald-700"}`}>{idx + 1}</span>
                          <div className="flex-1 min-w-0">
                            <p className={`text-sm font-medium truncate ${dark ? "text-gray-200" : "text-gray-800"}`}>{testNames.join(", ")}</p>
                            <p className={`text-xs truncate ${dark ? "text-gray-500" : "text-gray-400"}`}>
                              {item.mode === "descriptive" ? "Descriptive" : "Inferential"} · {Object.values(item.answers).join(" → ")}
                            </p>
                          </div>
                          <div className="flex gap-1 flex-shrink-0">
                            <button onClick={() => movePlanItem(idx, -1)} disabled={idx === 0} className={`p-1 rounded text-xs ${idx === 0 ? "opacity-30" : "hover:bg-gray-200"} ${dark ? "text-gray-400" : "text-gray-500"}`}>↑</button>
                            <button onClick={() => movePlanItem(idx, 1)} disabled={idx === planItems.length - 1} className={`p-1 rounded text-xs ${idx === planItems.length - 1 ? "opacity-30" : "hover:bg-gray-200"} ${dark ? "text-gray-400" : "text-gray-500"}`}>↓</button>
                            <button onClick={() => removePlanItem(item.id)} className="p-1 rounded text-xs text-red-400 hover:text-red-600 hover:bg-red-50">✕</button>
                          </div>
                        </div>
                      );
                    })}
                  </div>

                  {/* Download consolidated plan */}
                  <button
                    onClick={generateConsolidatedSAP}
                    className={`w-full flex items-center justify-center gap-2 text-sm font-semibold px-4 py-3 rounded-xl transition-colors shadow-sm ${dark ? "bg-emerald-700 hover:bg-emerald-600 text-white" : "bg-emerald-600 hover:bg-emerald-700 text-white"}`}
                  >
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="w-4 h-4">
                      <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4" /><polyline points="7 10 12 15 17 10" /><line x1="12" y1="15" x2="12" y2="3" />
                    </svg>
                    Download Consolidated SAP
                  </button>

                  <button
                    onClick={() => { setPlanItems([]); setShowPlan(false); setPlanTitle(""); setPlanDesign(""); }}
                    className={`w-full text-xs font-medium py-2 rounded-lg transition-colors ${dark ? "text-gray-500 hover:text-red-400" : "text-gray-400 hover:text-red-500"}`}
                  >
                    Clear entire plan
                  </button>
                </div>
              )}
            </div>
          )}

          {/* ─── Glossary & References ─── */}
          <div className={`mt-8 rounded-2xl border shadow-sm overflow-hidden ${dark ? "bg-gray-900 border-gray-700" : "bg-white border-gray-200"}`}>
            {/* Glossary */}
            <button
              onClick={() => setShowGlossary(!showGlossary)}
              className={`w-full flex items-center justify-between px-5 py-4 ${dark ? "hover:bg-gray-800" : "hover:bg-gray-50"} transition-colors`}
            >
              <div className="flex items-center gap-3">
                <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round" className={`w-5 h-5 ${dark ? "text-amber-400" : "text-amber-600"}`}>
                  <path d="M4 19.5A2.5 2.5 0 016.5 17H20" /><path d="M6.5 2H20v20H6.5A2.5 2.5 0 014 19.5v-15A2.5 2.5 0 016.5 2z" /><line x1="8" y1="7" x2="16" y2="7" /><line x1="8" y1="11" x2="14" y2="11" />
                </svg>
                <span className={`text-sm font-bold ${dark ? "text-amber-300" : "text-amber-800"}`}>
                  Glossary of Key Terms
                </span>
              </div>
              <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" className={`w-4 h-4 transition-transform ${showGlossary ? "rotate-180" : ""} ${dark ? "text-gray-500" : "text-gray-400"}`}>
                <polyline points="6 9 12 15 18 9" />
              </svg>
            </button>
            {showGlossary && (
              <div className={`px-5 pb-5 border-t ${dark ? "border-gray-700" : "border-gray-100"}`}>
                <div className="mt-3 space-y-3">
                  {Object.entries(GLOSSARY).sort(([a], [b]) => a.localeCompare(b)).map(([term, def]) => (
                    <div key={term}>
                      <p className={`text-sm font-semibold ${dark ? "text-indigo-300" : "text-indigo-700"}`}>{term}</p>
                      <p className={`text-xs leading-relaxed mt-0.5 ${dark ? "text-gray-400" : "text-gray-600"}`}>{def}</p>
                    </div>
                  ))}
                </div>
              </div>
            )}

            {/* Further Reading / References */}
            <div className={`px-5 py-4 border-t ${dark ? "border-gray-700" : "border-gray-100"}`}>
              <div className="flex items-center gap-3 mb-3">
                <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round" className={`w-5 h-5 ${dark ? "text-blue-400" : "text-blue-600"}`}>
                  <path d="M2 3h6a4 4 0 014 4v14a3 3 0 00-3-3H2z" /><path d="M22 3h-6a4 4 0 00-4 4v14a3 3 0 013-3h7z" />
                </svg>
                <span className={`text-sm font-bold ${dark ? "text-blue-300" : "text-blue-800"}`}>
                  Further Reading
                </span>
              </div>
              <div className="space-y-2.5">
                {[
                  { authors: "Wasserstein RL, Lazar NA", year: "2016", title: "The ASA Statement on p-Values: Context, Process, and Purpose", journal: "The American Statistician", note: "The foundational statement on moving beyond p < 0.05" },
                  { authors: "Lang TA, Altman DG", year: "2015", title: "Basic Statistical Reporting for Articles Published in Biomedical Journals: The SAMPL Guidelines", journal: "Science Editors' Handbook", note: "How to report statistical results in medical papers" },
                  { authors: "Altman DG", year: "1991", title: "Practical Statistics for Medical Research", journal: "Chapman & Hall/CRC", note: "Classic reference for choosing and applying statistical tests" },
                  { authors: "Motulsky HJ", year: "2014", title: "Intuitive Biostatistics", journal: "Oxford University Press (4th ed)", note: "Accessible guide to choosing the right test — highly recommended for residents" },
                  { authors: "Greenhalgh T", year: "2019", title: "How to Read a Paper: The Basics of Evidence-Based Medicine and Healthcare", journal: "Wiley (6th ed)", note: "Understanding statistics in the context of clinical research" },
                  { authors: "Sullivan GM, Feinn R", year: "2012", title: "Using Effect Size — or Why the P Value Is Not Enough", journal: "J Grad Med Educ", note: "Why effect sizes matter more than p-values" },
                  { authors: "Kim HY", year: "2017", title: "Statistical notes for clinical researchers: Chi-squared test and Fisher's exact test", journal: "Restor Dent Endod", note: "Part of an excellent series on choosing between specific tests" },
                ].map((ref, i) => (
                  <div key={i} className={`text-xs leading-relaxed ${dark ? "text-gray-400" : "text-gray-600"}`}>
                    <span className={`font-medium ${dark ? "text-gray-300" : "text-gray-700"}`}>{ref.authors} ({ref.year}).</span>{" "}
                    <span className="italic">{ref.title}.</span>{" "}
                    <span>{ref.journal}.</span>
                    <span className={`block mt-0.5 ${dark ? "text-gray-500" : "text-gray-400"}`}>→ {ref.note}</span>
                  </div>
                ))}
              </div>
            </div>
          </div>

          {/* Contributors */}
          <div className="mt-10 text-center">
            <button
              onClick={() => setShowContributors(!showContributors)}
              className={`text-xs ${textMuted} hover:text-gray-600 transition-colors`}
            >
              {showContributors ? "Hide contributors" : "Contributors"}
            </button>
            {showContributors && (
              <div className={`mt-3 rounded-xl border p-4 text-left ${dark ? "bg-gray-900 border-gray-700" : "bg-white border-gray-200"}`}>
                <p className={`text-xs font-semibold ${textSecondary} uppercase tracking-wide mb-3`}>Contributors</p>
                <div className="space-y-2">
                  <div className="flex items-center gap-3">
                    <div className={`w-8 h-8 rounded-full flex items-center justify-center font-bold text-sm ${dark ? "bg-indigo-900 text-indigo-300" : "bg-indigo-100 text-indigo-600"}`}>AP</div>
                    <div>
                      <p className={`text-sm font-medium ${textPrimary}`}>Dr Abhijit Pakhare</p>
                      <p className={`text-xs ${textMuted}`}>Clinical Epidemiology Unit, AIIMS Bhopal</p>
                    </div>
                  </div>
                  <div className="flex items-center gap-3">
                    <div className={`w-8 h-8 rounded-full flex items-center justify-center font-bold text-sm ${dark ? "bg-teal-900 text-teal-300" : "bg-teal-100 text-teal-600"}`}>AJ</div>
                    <div>
                      <p className={`text-sm font-medium ${textPrimary}`}>Dr Ankur Joshi</p>
                      <p className={`text-xs ${textMuted}`}>Clinical Epidemiology Unit, AIIMS Bhopal</p>
                    </div>
                  </div>
                  <div className="flex items-center gap-3">
                    <div className={`w-8 h-8 rounded-full flex items-center justify-center font-bold text-sm ${dark ? "bg-purple-900 text-purple-300" : "bg-purple-100 text-purple-600"}`}>AI</div>
                    <div>
                      <p className={`text-sm font-medium ${textPrimary}`}>Claude (Anthropic)</p>
                      <p className={`text-xs ${textMuted}`}>AI assistant — decision tree, SAP templates, and code</p>
                    </div>
                  </div>
                </div>
              </div>
            )}
            <p className={`text-xs ${textFaint} mt-3`}>
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
    <div className={`min-h-screen ${bg} flex items-start justify-center p-4 pt-8 transition-colors duration-300`}>
      <DarkToggle dark={dark} setDark={setDark} />
      <div className="w-full max-w-xl">
        {/* Header */}
        <div className="text-center mb-6">
          <button onClick={handleReset} className={`text-2xl font-bold ${textPrimary} hover:text-indigo-500 transition-colors`}>
            ChooseMyStat
          </button>
          <p className={`text-sm ${textSecondary} mt-1`}>
            {mode === "descriptive" ? "Describe My Variables" : "Choose a Statistical Test"}
          </p>
          {planItems.length > 0 && (
            <button
              onClick={() => { handleReset(); setShowPlan(true); }}
              className={`inline-flex items-center gap-1.5 mt-2 text-xs font-medium px-3 py-1.5 rounded-full transition-colors ${dark ? "bg-emerald-900 text-emerald-300 hover:bg-emerald-800" : "bg-emerald-100 text-emerald-700 hover:bg-emerald-200"}`}
            >
              <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="w-3.5 h-3.5">
                <path d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2" />
                <rect x="9" y="3" width="6" height="4" rx="1" />
              </svg>
              Plan: {planItems.length} {planItems.length === 1 ? "item" : "items"}
            </button>
          )}
        </div>

        {!showResults ? (
          <div>
            <Breadcrumbs answers={answers} steps={activeSteps.filter((s) => !s.show || s.show(answers))} />
            <ProgressDots total={visibleSteps.length} current={stepIndex} />

            {/* Question card */}
            <SlideTransition stepKey={currentStep.id} direction={slideDirection}>
              <div className={`rounded-2xl shadow-lg border p-6 mb-4 ${card}`}>
                <h2 className={`text-lg font-bold ${textPrimary} mb-1`}>{currentStep.title}</h2>
                <p className={`text-sm ${textSecondary} mb-5`}>{currentStep.subtitle}</p>
                <div className="space-y-3">
                  {currentStep.options
                    .filter((opt) => !opt.showWhen || opt.showWhen(answers))
                    .map((opt) => (
                    <OptionCard
                      key={opt.value}
                      option={opt}
                      selected={answers[currentStep.id]}
                      onClick={handleSelect}
                      dark={dark}
                    />
                  ))}
                </div>
              </div>
            </SlideTransition>

            {/* Navigation */}
            <div className="flex justify-between items-center px-1">
              <button
                onClick={handleBack}
                className="text-sm text-indigo-500 font-medium hover:text-indigo-400"
              >
                ← {stepIndex === 0 ? "Home" : "Back"}
              </button>
              <span className={`text-xs ${textMuted}`}>
                Step {stepIndex + 1} of {visibleSteps.length}
              </span>
            </div>
          </div>
        ) : (
          <div>
            {/* Results header */}
            <div className={`rounded-2xl shadow-lg border p-5 mb-5 ${card}`}>
              <h2 className={`text-lg font-bold ${textPrimary} mb-1`}>
                {mode === "descriptive" ? "Recommended Summary" : "Recommended Analysis"}
              </h2>
              <p className={`text-sm ${textSecondary} mb-1`}>Based on your inputs:</p>
              <div className="flex flex-wrap gap-2 mb-4">
                {Object.entries(answers).map(([k, v]) => (
                  <span key={k} className={`text-xs font-medium px-2.5 py-1 rounded-full ${dark ? "bg-indigo-900 text-indigo-300" : "bg-indigo-100 text-indigo-700"}`}>
                    {v.replace(/_/g, " ")}
                  </span>
                ))}
              </div>

              {/* Share link */}
              <button
                onClick={() => {
                  navigator.clipboard.writeText(window.location.href);
                  alert("Link copied! Share it with your collaborators.");
                }}
                className={`inline-flex items-center gap-1.5 text-xs font-medium px-2.5 py-1 rounded-lg mb-3 transition-colors ${dark ? "text-indigo-300 bg-indigo-950 hover:bg-indigo-900 border border-indigo-800" : "text-indigo-600 bg-indigo-50 hover:bg-indigo-100 border border-indigo-100"}`}
              >
                <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="w-3.5 h-3.5">
                  <path d="M10 13a5 5 0 007.54.54l3-3a5 5 0 00-7.07-7.07l-1.72 1.71" /><path d="M14 11a5 5 0 00-7.54-.54l-3 3a5 5 0 007.07 7.07l1.71-1.71" />
                </svg>
                Share this recommendation
              </button>

              {/* Why this test? */}
              {mode === "inferential" && (
                <div className={`rounded-lg px-4 py-3 mb-3 ${dark ? "bg-indigo-950 border border-indigo-800" : "bg-indigo-50 border border-indigo-100"}`}>
                  <p className={`text-xs font-semibold uppercase tracking-wide mb-1 ${dark ? "text-indigo-400" : "text-indigo-600"}`}>Why this test?</p>
                  <p className={`text-sm leading-relaxed ${dark ? "text-indigo-200" : "text-indigo-900"}`}
                     dangerouslySetInnerHTML={{ __html: explainReasoning(answers).replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>') }}
                  />
                </div>
              )}

              {mode === "inferential" && results.length > 1 && (
                <p className={`text-xs ${textMuted} italic mb-3`}>
                  Multiple tests: use the unadjusted test first, then the regression model for adjusted analysis.
                </p>
              )}

              {/* Reporting style toggle */}
              <div className={`flex items-center justify-between rounded-lg px-3 py-2 ${dark ? "bg-gray-800" : "bg-gray-50"}`}>
                <span className={`text-xs font-medium ${dark ? "text-gray-300" : "text-gray-600"}`}>Reporting style:</span>
                <div className={`flex items-center rounded-lg border overflow-hidden ${dark ? "bg-gray-900 border-gray-600" : "bg-white border-gray-200"}`}>
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
            <div className="mt-5 px-1 space-y-3">
              {/* Add to Plan — primary action */}
              <button
                onClick={() => { addToPlan(); handleReset(); }}
                className={`w-full flex items-center justify-center gap-2 text-sm font-semibold px-4 py-3 rounded-xl transition-colors shadow-sm ${dark ? "bg-emerald-700 hover:bg-emerald-600 text-white" : "bg-emerald-600 hover:bg-emerald-700 text-white"}`}
              >
                <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="w-4 h-4"><path d="M12 5v14" /><path d="M5 12h14" /></svg>
                Add to Analysis Plan & Continue
                {planItems.length > 0 && (
                  <span className="ml-1 bg-white/20 text-xs font-bold px-2 py-0.5 rounded-full">{planItems.length} saved</span>
                )}
              </button>

              <div className="flex justify-between items-center">
                <button onClick={handleBack} className="text-sm text-indigo-600 font-medium hover:text-indigo-800">
                  ← Back
                </button>
                <div className="flex gap-2">
                  <button
                    onClick={() => {
                      const testData = mode === "descriptive"
                        ? results.map((k) => DESCRIPTIVES[k])
                        : results.map((k) => TESTS[k]);
                      const lines = [
                        "ChooseMyStat — Recommendation",
                        "=" .repeat(40),
                        "",
                        "Your inputs: " + Object.values(answers).join(" → "),
                        "",
                        ...testData.flatMap((t) => [
                          `TEST: ${t.name}`,
                          "-".repeat(30),
                          "SAP Template:",
                          useTraditional ? (t.sapTraditional || t.sap) : t.sap,
                          "",
                          ...(t.r ? ["R Code:", t.r, ""] : []),
                          ...(t.jasp ? ["JASP Path:", t.jasp, ""] : []),
                          "",
                        ]),
                        "Generated by ChooseMyStat · Clinical Epidemiology Unit, AIIMS Bhopal",
                      ];
                      const blob = new Blob([lines.join("\n")], { type: "text/plain" });
                      const a = document.createElement("a");
                      a.href = URL.createObjectURL(blob);
                      a.download = `ChooseMyStat_${results[0] || "recommendation"}.txt`;
                      a.click();
                    }}
                    className={`flex items-center gap-1.5 text-sm font-medium px-3 py-2.5 rounded-xl border transition-colors ${dark ? "text-gray-400 border-gray-700 hover:bg-gray-800 hover:text-gray-200" : "text-gray-500 border-gray-200 hover:bg-gray-50 hover:text-gray-700"}`}
                  >
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round" className="w-4 h-4">
                      <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4" /><polyline points="7 10 12 15 17 10" /><line x1="12" y1="15" x2="12" y2="3" />
                    </svg>
                    Save this only
                  </button>
                  <button
                    onClick={handleReset}
                    className={`text-sm font-medium px-5 py-2.5 rounded-xl transition-colors shadow-sm ${dark ? "bg-gray-700 text-gray-200 hover:bg-gray-600" : "bg-gray-200 text-gray-700 hover:bg-gray-300"}`}
                  >
                    Start Over
                  </button>
                </div>
              </div>
            </div>
          </div>
        )}

        {/* Footer */}
        <div className="text-center mt-8 mb-4">
          <p className={`text-xs ${textMuted}`}>AIIMS Bhopal · Clinical Epidemiology Unit</p>
        </div>
      </div>
      <Analytics />
    </div>
  );
}
