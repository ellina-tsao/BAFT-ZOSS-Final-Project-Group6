# Appendix A. R Code and Reproducibility

| Item | Content |
|---|---|
| GitHub repository | [Insert GitHub URL] |
| Main scripts | `zoss/forecast_outputs_standardized/run_all_standardized_forecasts.R`; `zoss/forecast_outputs_standardized/zoss_forecast_helpers.R`; `zoss/forecast_outputs_standardized/aov/zoss_aov_forecast_standardized.R`; `zoss/forecast_outputs_standardized/new_customer_volume/zoss_new_customer_volume_forecast_standardized.R`; `zoss/forecast_outputs_standardized/returning_customer_volume/zoss_returning_customer_volume_forecast_standardized.R`; `zoss/zoss_retail_revenue_share_calendar_adjusted.R` |
| Key packages | `tidyverse`, `lubridate`, `tsibble`, `fable`, `feasts`, `forecast`, `distributional`, `scales`, `glue`, `patchwork` |
| How to run | From the project root, run `Rscript zoss/forecast_outputs_standardized/run_all_standardized_forecasts.R`. This regenerates the standardized validation metrics, performance charts, error plots, 2025 forecast plots, forecast CSVs, and model reports for AOV, New Customer Volume, Returning Customer Volume, and Retail Product Revenue Share. |

## Reproducibility Workflow

1. Raw transaction data are read from `zoss/zoss_data.csv`.
2. Monthly KPI series are constructed for each target: Average Order Value, New Customer Volume, Returning Customer Volume, and Retail Product Revenue Share.
3. The standardized time split is applied: training from 2018 Jul to 2023 Dec, validation from 2024 Jan to 2024 Dec, and future forecast from 2025 Jan to 2025 Dec.
4. Candidate forecasting models are fit on the training window and evaluated using 2024 roll-forward validation forecasts.
5. Validation metrics are computed from actual 2024 observations minus validation forecasts, not from training fitted values.
6. The final selected model for each KPI is refit on the full historical series through 2024 Dec, then used to generate 2025 monthly forecasts.
7. Each script exports model comparison tables, slide-ready tables, performance charts, error plots, final forecast plots, forecast CSVs, and text reports.

## Final Model Summary

| Target KPI | Final selected model | Business use |
|---|---|---|
| Average Order Value | `ARIMA(1,0,1)(1,1,0)[12]` | Customer spending baseline and revenue planning |
| New Customer Volume | `SES` | Customer acquisition planning |
| Returning Customer Volume | `ARIMA(0,1,2)(0,0,2)[12]` | Retention and loyal customer traffic monitoring |
| Retail Product Revenue Share | `ARIMA(1,0,0)(0,0,0)[12] with Jan-Feb dummy` | Retail product strategy and product-service mix planning |

## Notes

- `tsclean()` is used only for the AOV series, following the original group member script.
- Retail Product Revenue Share does not use `tsclean()`; it uses explicit calendar dummy variables for January and February.
- The main presentation plots intentionally show only a small set of benchmark and selected models. Full candidate comparisons are kept in appendix output files.
