# Great Lakes Ice Cover: 50-Year Trend Analysis
50-year trend analysis of Great Lakes ice cover using daily percentage data. Includes missing data imputation, AUC metrics, visualizations, and trend modeling via Mann-Kendall tests and polynomial regression.

This project analyzes long-term trends in ice cover across the Great Lakes over a 50-year period (1973–2024). Using daily ice cover percentage data, the study investigates changes in seasonality, extent, and duration of ice coverage, with a focus on identifying systemic climatic patterns.

## 📊 Objectives

- Address missing values using domain-informed imputation.
- Quantify seasonal ice coverage using Area Under the Curve (AUC).
- Visualize year-to-year variability and trends in ice cover.
- Test for monotonic trends using the Mann-Kendall test.
- Model AUC trends over time with regression techniques.

## 🧹 Data Cleaning & Imputation

- Leading NAs were filled with gradual buildup from 5% to the first observed value.
- Trailing NAs were filled by gradual decline from the last value to 5%.
- Internal gaps were handled using linear interpolation (`zoo::na.approx`).
- Leap day (Feb 29) was replaced with 0% to maintain uniform structure.

## 📐 Methodology

- AUC calculated using `PKNCA::auc()` to represent seasonal ice extent and duration.
- Visualizations:
  - Heatmap of ice cover across days and years
  - Annual AUC time series plots
  - Polynomial trend lines
- Statistical Tests:
  - Polynomial regression (linear and quadratic)
  - Mann-Kendall trend test (non-parametric)

## 📈 Key Findings

- Statistically significant decline in AUC over the past 50 years (p = 0.00155).
- Strong negative trend in ice cover extent, especially post-1990.
- Seasonal variability persists, but with reduced ice formation and earlier melt.
- Mann-Kendall test confirms a downward trend likely associated with climate change.

## 🛠 Tools & Packages Used

- R (`ggplot2`, `zoo`, `dplyr`, `PKNCA`, `trend`)
- AUC Computation: [`PKNCA`](https://cran.r-project.org/web/packages/PKNCA/vignettes/v05-auc-calculation-with-PKNCA.html)
- Mann-Kendall Test: [`trend`](https://cran.r-project.org/web/packages/trend/index.html)

## Resource
- https://www.glerl.noaa.gov/data/ice/


## 📌 Authors

- **Michael Okanta**  
- **Oliver Yawlui**  
Miami University (2024)


