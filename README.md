# Predictive Analytics: Time Series Analysis in R

Time-series analysis and forecasting project implemented in R, covering AR/ARIMA/SARIMA modelling, stationarity diagnostics, residual analysis, seasonality, trend decomposition, and multi-step forecasting.

## Project scope

The repository contains three exercises / case-study groups:

1. **Spanish unemployment (2022–2025)** — monthly unemployment-rate analysis, differencing, ARIMA model comparison, residual diagnostics, and 24-month forecasting.
2. **Rainfall in Zevgolatio, Corinthia (2016–2025)** — monthly rainfall aggregation from Open-Meteo data, seasonal differencing, SARIMA modelling, diagnostics, and 24-month forecasting.
3. **Additional time-series applications** — AR(2) modelling of `cmort`, ARIMA modelling of global land temperature, and decomposition of quarterly beer sales into seasonal, trend, and cyclical components.

## Repository structure

```text
.
├── scripts/
│   ├── exercise1_unemployment.R
│   ├── exercise2_rainfall.R
│   └── exercise3_time_series_models.R
├── data/
│   ├── open-meteo-37.93N22.83E37m.csv
│   └── unemployment_spain.txt
├── docs/
│   └── report.pdf
├── ask1.txt                         # original uploaded file (preserved)
├── ask2.txt                         # original uploaded file (preserved)
├── ask3.txt                         # original uploaded file (preserved)
├── open-meteo-37.93N22.83E37m.csv  # original uploaded file (preserved)
├── report r.pdf                     # original uploaded file (preserved)
└── unemployment_spain.txt           # original uploaded file (preserved)
```

The files in `scripts/`, `data/`, and `docs/` are organized copies of the original uploads. The original root files are intentionally retained unchanged.

## Methods

- Time-series visualization and decomposition
- ACF / PACF analysis
- First and seasonal differencing
- AR and ARMA modelling
- ARIMA and SARIMA modelling
- AIC-based model comparison
- Ljung–Box residual diagnostics
- Forecast intervals and multi-step forecasting
- Seasonal indices, deseasonalization, trend and cyclical analysis

## Data sources

- Spanish unemployment: Eurostat / INE data used in the coursework analysis.
- Rainfall: Open-Meteo Historical Weather data for Zevgolatio, Corinthia.
- `cmort` and global temperature series: datasets used through the `astsa` R package.
- Beer-sales data: coursework dataset supplied in the assignment.

## Running the analyses

Open R or RStudio from the repository root and run the relevant script in `scripts/`. Some parts use the `astsa` package; the remaining analyses primarily rely on base R time-series functions.

## Notes

This repository preserves the original uploaded coursework files while also providing a cleaner structure for browsing the code, data, and final report.
