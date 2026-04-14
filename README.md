# Experimental Design with R — Statistical Testing Toolkit

A comprehensive R toolkit for analyzing experimental data using statistical tests — from visual exploration to assumptions checking, hypothesis testing, and post-hoc analysis with both **parametric** and **non-parametric** methods.

## Datasets Used

| Dataset | Description | Source |
|---------|-------------|--------|
| ToothGrowth | Tooth length by supplement and dose | `datasets` |
| PlantGrowth | Plant weights under different conditions | `datasets` |
| mice2 | Mouse weights before/after treatment | `datarium` |
| selfesteem | Self-esteem scores at 3 time points | `datarium` |
| anxiety | Anxiety scores across activity levels and time | `datarium` |
| poison.data | Survival time under poisons and treatments | `BHH2` |
| dental, rat, respiration | Advanced non-parametric analysis data | `nparLD` |

## Key Topics Covered

### Parametric Tests
- One-sample, two-sample (independent & paired) t-tests
- One-way and two-way ANOVA
- Repeated measures ANOVA
- Mixed ANOVA (between and within factors)
- Post-hoc tests (Tukey HSD, Bonferroni-adjusted t-tests)

### Non-Parametric Alternatives
- Wilcoxon signed-rank test
- Mann-Whitney U test
- Kruskal-Wallis test
- Friedman test
- Non-parametric mixed ANOVA (`nparLD`)

### Assumptions Testing
- **Normality:** Shapiro-Wilk, Jarque-Bera, Anderson-Darling, Lilliefors
- **Homogeneity of variances:** Bartlett, Fligner-Killeen, Levene
- **Outliers:** Z-scores and IQR
- **Sphericity:** Mauchly's test (repeated measures)

## Getting Started

### Prerequisites

- R 4.0+
- RStudio (recommended)

### Installation

```bash
git clone https://github.com/Wobbly1212/statistical-tests-experimental-design-R.git
cd statistical-tests-experimental-design-R
```

Install all required packages:

```r
source("install_packages.R")
```

### Usage

1. Open `experimental_design.R` in RStudio
2. Run section by section based on your use case
3. Ensure `sonno-long.csv` (if used) is in your working directory

## Project Structure

```
statistical-tests-experimental-design-R/
├── experimental_design.R   # Main analysis script
├── install_packages.R      # Dependency installer
├── LICENSE
└── README.md
```

## Author

**Diako Darabi**

## License

This project is licensed under the [MIT License](LICENSE).
