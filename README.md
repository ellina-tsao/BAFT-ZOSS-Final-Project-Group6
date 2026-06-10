# BAFT ZOSS Final Project - Group 6

This repository contains the reproducible forecasting code and finalized output artifacts for the BAFT ZOSS final project.

## Project Objective

The project builds monthly forecasting baselines for ZOSS business KPIs, including Average Order Value, New Customer Volume, Returning Customer Volume, and Retail Product Revenue Share. The workflow supports proactive monitoring and early warning for strategic decision-making.

## Repository Contents

- `zoss/forecast_outputs_standardized/`: standardized forecasting scripts, helper functions, metrics tables, model reports, and forecast outputs for AOV, New Customer Volume, and Returning Customer Volume.
- `zoss/zoss_retail_revenue_share_calendar_adjusted.R`: finalized Retail Product Revenue Share script with calendar-adjusted ARIMA modeling.
- `zoss/retail_revenue_share_calendar_outputs/`: selected Retail Product Revenue Share model outputs and report artifacts.

## Data Availability

The original raw transaction file is not included in this public repository because it contains member-level transaction fields. The repository includes the finalized reproducibility scripts and output artifacts used in the written report and presentation.

## How to Reproduce

If the raw transaction file is available locally as `zoss/zoss_data.csv`, run:

```r
Rscript zoss/forecast_outputs_standardized/run_all_standardized_forecasts.R
```

This regenerates the standardized validation metrics, performance charts, error plots, final 2025 forecast plots, forecast CSVs, and model reports.

## Final Selected Models

| Target KPI | Final selected model |
|---|---|
| Average Order Value | ARIMA(1,0,1)(1,1,0)[12] |
| New Customer Volume | SES |
| Returning Customer Volume | ARIMA(0,1,2)(0,0,2)[12] |
| Retail Product Revenue Share | ARIMA(1,0,0)(0,0,0)[12] with Jan-Feb dummy |
