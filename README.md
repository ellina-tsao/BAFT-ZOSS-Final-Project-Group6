# BAFT ZOSS Final Project - Group 6

This repository contains the reproducible forecasting code and finalized output artifacts for the BAFT ZOSS final project.

## Project Objective

The project builds monthly forecasting baselines for ZOSS business KPIs, including Average Order Value, New Customer Volume, Returning Customer Volume, and Retail Product Revenue Share. The workflow supports proactive monitoring and early warning for strategic decision-making.

## Forecasting Workflow

The project follows the same workflow across target series:

1. Aggregate raw transaction records into monthly KPI series.
2. Fit candidate time-series models on the training period.
3. Evaluate models using 2024 roll-forward validation forecasts.
4. Select the final model using validation metrics and visual performance charts.
5. Refit the selected model on the full historical series through 2024 Dec.
6. Generate monthly forecasts for 2025.

Standard time split:

| Period | Date range | Purpose |
|---|---|---|
| Training | 2018 Jul - 2023 Dec | Fit candidate models |
| Validation | 2024 Jan - 2024 Dec | Simulate future performance and compare model accuracy |
| Future forecast | 2025 Jan - 2025 Dec | Produce final planning baselines |

## Repository Contents

- `zoss/forecast_outputs_standardized/`: standardized forecasting scripts, helper functions, metrics tables, model reports, and forecast outputs for AOV, New Customer Volume, and Returning Customer Volume.
- `zoss/zoss_retail_revenue_share_calendar_adjusted.R`: finalized Retail Product Revenue Share script with calendar-adjusted ARIMA modeling.
- `zoss/retail_revenue_share_calendar_outputs/`: selected Retail Product Revenue Share model outputs and report artifacts.
- `zoss/forecast_outputs_standardized/APPENDIX_A_R_CODE_AND_REPRODUCIBILITY.md`: report-ready reproducibility appendix text.
- `zoss/forecast_outputs_standardized/CODE_INVENTORY.md`: inventory of official scripts, outputs, and intermediate files excluded from the final workflow.

## Data Availability

The original raw transaction file is not included in this public repository because it contains member-level transaction fields. The repository includes the finalized reproducibility scripts and output artifacts used in the written report and presentation.

Expected local raw data path for full reruns:

```text
zoss/zoss_data.csv
```

## How to Reproduce

If the raw transaction file is available locally as `zoss/zoss_data.csv`, run:

```r
Rscript zoss/forecast_outputs_standardized/run_all_standardized_forecasts.R
```

This regenerates the standardized validation metrics, performance charts, error plots, final 2025 forecast plots, forecast CSVs, and model reports.

Required R packages:

```r
tidyverse
lubridate
tsibble
fable
feasts
forecast
distributional
scales
glue
patchwork
```

## Final Selected Models

| Target KPI | Final selected model | Business use |
|---|---|---|
| Average Order Value | ARIMA(1,0,1)(1,1,0)[12] | Customer spending baseline and revenue planning |
| New Customer Volume | SES | Customer acquisition planning |
| Returning Customer Volume | ARIMA(0,1,2)(0,0,2)[12] | Retention and loyal customer traffic monitoring |
| Retail Product Revenue Share | ARIMA(1,0,0)(0,0,0)[12] with Jan-Feb dummy | Retail product strategy and product-service mix planning |

## Key Output Files

Each standardized KPI folder contains:

| Output type | Filename pattern |
|---|---|
| Full model comparison table | `<target>_model_results.csv` |
| Slide-ready metrics table | `<target>_model_results_slide_ready.csv` |
| Model report | `<target>_model_report.txt` |
| Main validation performance chart | `<target>_validation_selected_models.png` |
| Forecast error plot | `<target>_validation_errors.png` |
| Appendix full model comparison chart | `<target>_validation_all_candidate_models.png` |
| Final 2025 forecast chart | `<target>_2025_final_forecast.png` |
| Final 2025 forecast values | `<target>_2025_final_forecast.csv` |

Retail Product Revenue Share uses equivalent output files under:

```text
zoss/retail_revenue_share_calendar_outputs/
```

## Modeling Notes

- Validation metrics are calculated from 2024 roll-forward forecasts, not from training fitted values.
- AOV follows the original group-member workflow and uses `tsclean()` for the AOV modeling series.
- Retail Product Revenue Share does not use `tsclean()`; it uses explicit Jan-Feb calendar dummy variables for the selected ARIMA regression model.
- The main presentation charts intentionally show a small set of benchmark and final models. Full candidate comparisons are included as appendix outputs.
