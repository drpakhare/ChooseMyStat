# ChooseMyStat

Interactive statistical analysis advisor for PG thesis research. Guides medical residents and clinical researchers through choosing the right descriptive summaries, statistical tests and sample size calculations, with ready-to-use Statistical Analysis Plan (SAP) templates, results-reporting examples, JASP menu paths and R code.

Live app: https://choose-my-stat.vercel.app

## Features

- **Describe My Variables** — recommends summary measures (mean ± SD vs median/IQR vs n/%), normality assessment and Table 1 generation
- **Choose a Statistical Test** — step-by-step decision tree covering 19 tests, from t-tests and chi-square to regression, survival analysis, mixed-effects models and GEE
- **Diagnostic Accuracy** — sensitivity, specificity, predictive values and ROC/AUC analysis
- **Agreement & Reliability** — Cohen's and weighted kappa, ICC and Bland-Altman analysis
- **Sample Size Calculator** — 19 calculators linked to the recommended tests, with sensitivity tables, dropout adjustment and SAP text
- **Dual reporting styles** — ASA 2016 effect-first templates and traditional p < 0.05 templates with disclaimer
- **JASP instructions** — point-and-click menu paths for the free JASP software
- **R code** — tidyverse ecosystem (gtsummary, sjPlot, finalfit, effectsize, lme4)
- **Analysis Plan Builder** — collect recommendations for each study objective into one consolidated plan; export the parts you need (SAP text, R code, JASP steps, results examples and more) as text, Markdown or an R script

## Contributors

- Dr Abhijit Pakhare — Clinical Epidemiology Unit, AIIMS Bhopal
- Dr Ankur Joshi — Clinical Epidemiology Unit, AIIMS Bhopal
- Claude (Anthropic) — AI assistant

## How to Cite

See [`CITATION.cff`](CITATION.cff), or use the "Cite this repository" button on GitHub. Each release is archived on Zenodo with a DOI.

## License

Released under the [MIT License](LICENSE).
