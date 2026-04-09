# ChooseMyStat

Interactive statistical analysis advisor for PG thesis research. Guides medical residents through choosing the right descriptive summaries and inferential tests, with ready-to-use SAP templates, JASP menu paths, and R code.

## Features

- **Describe My Variables** — recommends summary measures (mean±SD vs median/IQR vs n/%), normality assessment, and Table 1 generation
- **Choose a Statistical Test** — step-by-step decision tree covering 18 tests from t-tests to Cox regression
- **Dual reporting styles** — ASA 2016 effect-first templates and traditional p < 0.05 templates with disclaimer
- **JASP instructions** — point-and-click menu paths for the free JASP software
- **R code** — tidyverse ecosystem (gtsummary, sjPlot, finalfit, effectsize, lme4)

## Getting Started

```bash
npm install
npm run dev
```

## Deployment

Built for deployment on Vercel:

```bash
npm run build
```

## Contributors

- Dr Abhijit Pakhare — Clinical Epidemiology Unit, AIIMS Bhopal
- Dr Ankur Joshi — Clinical Epidemiology Unit, AIIMS Bhopal
- Claude (Anthropic) — AI assistant
