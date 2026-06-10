# ZOSS Forecasting Code Inventory and Reproducibility Notes

This folder contains the cleaned, standardized forecasting workflow used for the BAFT ZOSS final project.

## Official Scripts to Use

| Target KPI | Official script | Final selected model | Output folder |
|---|---|---|---|
| Average Order Value | `aov/zoss_aov_forecast_standardized.R` | `ARIMA(1,0,1)(1,1,0)[12]` | `forecast_outputs_standardized/aov/` |
| New Customer Volume | `new_customer_volume/zoss_new_customer_volume_forecast_standardized.R` | `SES` | `forecast_outputs_standardized/new_customer_volume/` |
| Returning Customer Volume | `returning_customer_volume/zoss_returning_customer_volume_forecast_standardized.R` | `ARIMA(auto)` | `forecast_outputs_standardized/returning_customer_volume/` |
| Retail Product Revenue Share | `../zoss_retail_revenue_share_calendar_adjusted.R` | `ARIMA(1,0,0)(0,0,0)[12] with Jan-Feb dummy` | `../retail_revenue_share_calendar_outputs/` |

## Shared Helper

`zoss_forecast_helpers.R` contains shared plotting, validation, forecast-table, and model-fitting helper functions used by the standardized AOV, New Customer, and Returning Customer scripts.

Retail Product Revenue Share remains in its own reference script because it has target-specific calendar dummy variables and was already reviewed separately.

## Report Appendix Text

`APPENDIX_A_R_CODE_AND_REPRODUCIBILITY.md` contains a report-ready version of the R code and reproducibility appendix table, including the main scripts, key packages, run command, workflow, and final model summary.

## How to Regenerate Outputs

From the project root:

```r
Rscript zoss/forecast_outputs_standardized/run_all_standardized_forecasts.R
```

Or run each official script individually:

```r
Rscript zoss/forecast_outputs_standardized/aov/zoss_aov_forecast_standardized.R
Rscript zoss/forecast_outputs_standardized/new_customer_volume/zoss_new_customer_volume_forecast_standardized.R
Rscript zoss/forecast_outputs_standardized/returning_customer_volume/zoss_returning_customer_volume_forecast_standardized.R
Rscript zoss/zoss_retail_revenue_share_calendar_adjusted.R
```

## Standard Time Split

All standardized scripts use the same time-series split:

| Period | Dates | Purpose |
|---|---|---|
| Training | 2018 Jul - 2023 Dec | Fit candidate models |
| Validation | 2024 Jan - 2024 Dec | Roll-forward forecast evaluation |
| Future forecast | 2025 Jan - 2025 Dec | Final forecast after refitting selected model |

Validation metrics are calculated from 2024 roll-forward validation forecasts, not from training fitted values.

## Official Output Types

Each standardized KPI folder contains:

| Output type | Filename pattern |
|---|---|
| Full model comparison table | `<target>_model_results.csv` |
| Slide-ready metric table | `<target>_model_results_slide_ready.csv` |
| Model report | `<target>_model_report.txt` |
| Main validation performance chart | `<target>_validation_selected_models.png` |
| Top-panel actual/forecast chart | `<target>_validation_selected_models_actual_forecast_only.png` |
| Top-three actual/forecast chart | `<target>_validation_top3_actual_forecast_only.png` |
| Forecast error plot | `<target>_validation_errors.png` |
| Appendix all-model comparison plot | `<target>_validation_all_candidate_models.png` |
| Final 2025 forecast plot | `<target>_2025_final_forecast.png` |
| Final 2025 forecast CSV | `<target>_2025_final_forecast.csv` |

Retail Product Revenue Share uses equivalent output names in `../retail_revenue_share_calendar_outputs/`.

## Data Construction Notes

- AOV is aggregated monthly as mean transaction amount and uses the original group-member `tsclean()` adjusted AOV series.
- New Customer Volume counts monthly records where `New.Customer..Yes.No.` is `"是"`.
- Returning Customer Volume counts distinct monthly transaction IDs where `New.Customer..Yes.No.` is `"否"`.
- Retail Product Revenue Share is monthly retail product revenue divided by total revenue. Its final model uses a Jan-Feb calendar dummy.

## Cleaning / Outlier Treatment

`tsclean()` is used only for AOV, following the original group member script.

Customer volume models and Retail Product Revenue Share do not use automatic `tsclean()` adjustment.

## Candidate Model Notes

- AOV compares `Naive`, `SNaive`, `SES`, `ETS`, `Base ARIMA`, selected seasonal `ARIMA`, `TSLM`, and `NNet`.
- New Customer Volume compares `Naive`, `SNaive`, `SES`, `ETS`, `Base ARIMA`, `TSLM`, and `NNet`.
- Returning Customer Volume compares `Naive`, `SNaive`, `SES`, `ETS`, `ARIMA`, and `TSLM`.
- Retail Product Revenue Share compares baseline ARIMA and calendar-adjusted ARIMA candidates.

## Old / Intermediate Scripts

The following files are useful for provenance but should not be treated as the final reproducible workflow:

- `../zoss_AOV.R`
- `../zoss_customer_volume_diagnostics.R`
- `../zoss_retail_revenue_share_diagnostics.R`
- `../zoss_retail_0512.R`
- old output folders such as `../customer_volume_outputs/`, `../retail_revenue_share_outputs/`

Use the official scripts listed at the top of this document for report and presentation regeneration.
