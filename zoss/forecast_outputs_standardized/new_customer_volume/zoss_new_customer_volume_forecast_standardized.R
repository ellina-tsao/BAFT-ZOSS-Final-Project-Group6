# ============================================================
# ZOSS New Customer Volume Forecasting - Standardized Output Version
#
# 0. Setup
# 1. Load Data
# 2. Build Monthly Target Series
# 3. Add Optional Calendar / Business Features
# 4. Train / Validation / Future Split
# 5. Fit Candidate Models
# 6. Validation Evaluation
# 7. Final Model Selection
# 8. Refit Final Model on Full Historical Data
# 9. Generate 2025 Forecast
# 10. Plot Outputs
# 11. Export Tables and Reports
# ============================================================


# ============================================================
# 0. Setup
# ============================================================

library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(feasts)
library(distributional)
library(scales)
library(glue)
library(patchwork)
library(forecast)

set.seed(123)

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cmd_args[grepl("^--file=", cmd_args)])
script_file <- if (length(file_arg) > 0) {
  normalizePath(file_arg[1], mustWork = FALSE)
} else {
  tryCatch({
    frame_file <- sys.frame(1)$ofile
    if (is.null(frame_file) || length(frame_file) == 0) {
      NA_character_
    } else {
      normalizePath(frame_file[1], mustWork = FALSE)
    }
  }, error = function(e) NA_character_)
}

script_dir <- if (length(script_file) == 1 && !is.na(script_file) && nzchar(script_file)) {
  dirname(script_file)
} else {
  getwd()
}

helper_candidates <- unique(c(
  file.path(dirname(script_dir), "zoss_forecast_helpers.R"),
  file.path(script_dir, "zoss_forecast_helpers.R"),
  file.path(script_dir, "forecast_outputs_standardized", "zoss_forecast_helpers.R"),
  file.path(getwd(), "zoss_forecast_helpers.R"),
  file.path(getwd(), "forecast_outputs_standardized", "zoss_forecast_helpers.R"),
  file.path(getwd(), "zoss", "forecast_outputs_standardized", "zoss_forecast_helpers.R")
))
helper_path <- helper_candidates[file.exists(helper_candidates)][1]
if (is.na(helper_path) || !file.exists(helper_path)) {
  stop("Cannot find helper script. Checked: ", paste(helper_candidates, collapse = ", "))
}
source(helper_path)

input_dir <- dirname(resolve_zoss_data_path(script_dir))
data_path <- file.path(input_dir, "zoss_data.csv")
output_root <- file.path(input_dir, "forecast_outputs_standardized")
output_dir <- file.path(output_root, "new_customer_volume")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

training_start <- yearmonth("2018 Jul")
training_end <- yearmonth("2023 Dec")
validation_start <- yearmonth("2024 Jan")
validation_end <- yearmonth("2024 Dec")
future_start <- yearmonth("2025 Jan")
future_end <- yearmonth("2025 Dec")

target_name <- "New Customer Volume"
target_file_stem <- "new_customer_volume"
target_business_meaning <- "Customer acquisition planning and marketing reach monitoring."

model_display <- c(
  "Naive" = "Naive",
  "SNaive" = "SNaive",
  "SES" = "SES",
  "ETS" = "ETS",
  "Base_ARIMA" = "Base ARIMA",
  "TSLM" = "TSLM",
  "NNet" = "NNet"
)

model_colors <- c(
  "Actual" = "black",
  "Naive" = "#8C8C8C",
  "SNaive" = "#B58900",
  "SES" = "#009E73",
  "ETS" = "#2CA02C",
  "Base ARIMA" = "#1F9EEA",
  "TSLM" = "#D55E00",
  "NNet" = "#7B3FB2"
)

model_reasons <- c(
  "Naive" = "Simple one-step benchmark using the most recent observation.",
  "SNaive" = "Seasonal naive benchmark from the same monthly pattern.",
  "SES" = "Simple exponential smoothing benchmark without trend or seasonality.",
  "ETS" = "Exponential smoothing model selected automatically by ETS.",
  "Base_ARIMA" = "Automatic ARIMA benchmark from the original model set.",
  "TSLM" = "Trend and monthly seasonality regression benchmark.",
  "NNet" = "Neural-network autoregression included in the original New Customer script."
)

all_candidate_models_internal <- names(model_display)
y_labeler <- scales::label_comma(accuracy = 1)
model_notation_lookup <- c(
  "Naive" = "Naive",
  "SNaive" = "SNaive",
  "SES" = "SES",
  "ETS" = "ETS(auto)",
  "Base_ARIMA" = "ARIMA(auto)",
  "TSLM" = "TSLM(trend + season)",
  "NNet" = "NNETAR"
)


# ============================================================
# 1. Load Data
# ============================================================

raw_data <- read.csv(data_path, stringsAsFactors = FALSE)
raw_data <- raw_data[-1, ]

required_data <- raw_data %>%
  transmute(
    Total_Amount = as.numeric(Total.Amount),
    New_Customer_Raw = trimws(as.character(New.Customer..Yes.No.)),
    Year = as.numeric(Year),
    Month = as.numeric(Month)
  ) %>%
  mutate(
    New_Customer = case_when(
      New_Customer_Raw == "是" ~ 1,
      New_Customer_Raw == "否" ~ 0,
      TRUE ~ NA_real_
    )
  )


# ============================================================
# 2. Build Monthly Target Series
# ============================================================

# Preserve the group script logic: monthly new customer volume counts rows where
# New.Customer..Yes.No. is "是".
monthly_kpi <- required_data %>%
  filter(!is.na(Year), !is.na(Month)) %>%
  group_by(Year, Month) %>%
  summarise(
    New_Customer_Vol = sum(New_Customer == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year, Month) %>%
  mutate(
    Month_Index = yearmonth(paste(Year, Month, sep = "-")),
    Month_Date = as.Date(Month_Index),
    Value = New_Customer_Vol
  ) %>%
  filter(Month_Index >= training_start, Month_Index <= validation_end) %>%
  select(Month_Index, Month_Date, Value) %>%
  as_tsibble(index = Month_Index)


# ============================================================
# 3. Add Optional Calendar / Business Features
# ============================================================

# No external calendar predictor is used for New Customer Volume in the group
# script. The standardized version keeps that modeling scope unchanged.


# ============================================================
# 4. Train / Validation / Future Split
# ============================================================

train_data <- monthly_kpi %>% filter_index("2018-07" ~ "2023-12")
validation_data <- monthly_kpi %>% filter_index("2024-01" ~ "2024-12")

cv_data <- monthly_kpi %>%
  filter_index("2018-07" ~ "2024-11") %>%
  stretch_tsibble(.init = nrow(train_data), .step = 1)


# ============================================================
# 5. Fit Candidate Models
# ============================================================

fit_roll <- fit_candidate_models(
  cv_data,
  include_nnet = TRUE,
  include_selected_arima = FALSE,
  snaive_lag_year = FALSE
)

fc_roll <- fit_roll %>% forecast(h = 1)
validation_forecast_tbl <- forecast_tbl_from_fable(fc_roll)

fit_train <- fit_candidate_models(
  train_data,
  include_nnet = TRUE,
  include_selected_arima = FALSE,
  snaive_lag_year = FALSE
)

train_fitted_tbl <- fitted_tbl_from_mable(fit_train)


# ============================================================
# 6. Validation Evaluation
# ============================================================

raw_validation_rank <- accuracy(fc_roll, monthly_kpi) %>%
  as_tibble() %>%
  arrange(RMSE)


# ============================================================
# 7. Final Model Selection
# ============================================================

# The downloaded group script does not hard-code one final model for New
# Customer Volume. It prints the lowest 2024 roll-forward RMSE. This standardized
# version makes that rule explicit and reproducible.
final_model_internal <- raw_validation_rank %>%
  slice(1) %>%
  pull(.model)

final_model_name <- display_model(final_model_internal, model_display)
final_model_notation <- unname(model_notation_lookup[final_model_internal])
final_model_reason <- "The downloaded group script selects by the lowest 2024 roll-forward validation RMSE; this script makes that selection rule explicit."

validation_metrics <- make_validation_metrics(
  fc_roll = fc_roll,
  target_ts = monthly_kpi,
  model_display = model_display,
  model_reasons = model_reasons,
  final_model_internal = final_model_internal
)

presentation_models_internal <- unique(c("Naive", "Base_ARIMA", final_model_internal))
if (length(presentation_models_internal) < 3) {
  presentation_models_internal <- unique(c(presentation_models_internal, "SES", "ETS"))
}

slide_ready_metrics <- validation_metrics %>%
  filter(internal_model %in% unique(c(presentation_models_internal, validation_metrics$internal_model[1]))) %>%
  select(Model, MAE, RMSE, MAPE, MASE, RMSSE, Selected_Final_Model)

top3_models_internal <- validation_metrics %>%
  slice_head(n = 3) %>%
  pull(internal_model)

final_validation_row <- validation_metrics %>%
  filter(internal_model == final_model_internal)


# ============================================================
# 8. Refit Final Model on Full Historical Data
# ============================================================

fit_final <- fit_final_model(
  monthly_kpi,
  final_model_internal = final_model_internal,
  snaive_lag_year = FALSE
)

final_fitted_tbl <- augment(fit_final) %>%
  mutate(Month_Date = as.Date(Month_Index)) %>%
  transmute(Month_Index, Month_Date, Fitted = .fitted) %>%
  filter(!is.na(Fitted))


# ============================================================
# 9. Generate 2025 Forecast
# ============================================================

fc_final <- fit_final %>% forecast(h = 12)

final_forecast_tbl <- final_forecast_tbl_from_fable(
  fc_obj = fc_final,
  target_name = target_name,
  final_model_name = final_model_name,
  round_digits = 0,
  nonnegative = TRUE
)


# ============================================================
# 10. Plot Outputs
# ============================================================

actual_forecast_plot <- plot_actual_validation_future(
  target_ts = monthly_kpi,
  train_fitted_tbl = train_fitted_tbl,
  validation_forecast_tbl = validation_forecast_tbl,
  final_forecast_tbl = final_forecast_tbl,
  model_display = model_display,
  model_colors = model_colors,
  presentation_models_internal = presentation_models_internal,
  final_model_internal = final_model_internal,
  final_model_name = final_model_name,
  y_labeler = y_labeler,
  training_start = training_start,
  training_end = training_end,
  validation_start = validation_start,
  validation_end = validation_end,
  future_start = future_start,
  future_end = future_end
)

top3_actual_forecast_plot <- plot_actual_validation_future(
  target_ts = monthly_kpi,
  train_fitted_tbl = train_fitted_tbl,
  validation_forecast_tbl = validation_forecast_tbl,
  final_forecast_tbl = final_forecast_tbl,
  model_display = model_display,
  model_colors = model_colors,
  presentation_models_internal = top3_models_internal,
  final_model_internal = final_model_internal,
  final_model_name = final_model_name,
  y_labeler = y_labeler,
  training_start = training_start,
  training_end = training_end,
  validation_start = validation_start,
  validation_end = validation_end,
  future_start = future_start,
  future_end = future_end
)

error_plot <- plot_forecast_errors(
  target_ts = monthly_kpi,
  train_fitted_tbl = train_fitted_tbl,
  validation_forecast_tbl = validation_forecast_tbl,
  model_display = model_display,
  model_colors = model_colors,
  presentation_models_internal = presentation_models_internal,
  y_labeler = y_labeler,
  validation_start = validation_start,
  validation_end = validation_end
)

validation_selected_plot <- plot_combined_performance(actual_forecast_plot, error_plot)

validation_all_candidate_plot <- plot_validation_comparison(
  target_ts = monthly_kpi,
  train_fitted_tbl = train_fitted_tbl,
  validation_forecast_tbl = validation_forecast_tbl,
  model_display = model_display,
  model_colors = model_colors,
  models_to_plot_internal = all_candidate_models_internal,
  y_labeler = y_labeler,
  title = "New Customer Volume: Candidate Model Validation",
  subtitle = "Full appendix view of all original New Customer candidate models.",
  validation_start = validation_start,
  validation_end = validation_end
)

final_forecast_plot <- plot_final_forecast(
  target_ts = monthly_kpi,
  final_fitted_tbl = final_fitted_tbl,
  final_forecast_tbl = final_forecast_tbl,
  final_model_name = final_model_name,
  final_model_notation = final_model_notation,
  readable_target = target_name,
  y_labeler = y_labeler,
  y_axis_title = "New customer transactions",
  future_start = future_start,
  future_end = future_end,
  color = model_colors[final_model_name]
)


# ============================================================
# 11. Export Tables and Reports
# ============================================================

write.csv(
  validation_metrics %>% select(-internal_model),
  file.path(output_dir, "new_customer_volume_model_results.csv"),
  row.names = FALSE
)

write.csv(
  slide_ready_metrics,
  file.path(output_dir, "new_customer_volume_model_results_slide_ready.csv"),
  row.names = FALSE
)

write.csv(
  final_forecast_tbl,
  file.path(output_dir, "new_customer_volume_2025_final_forecast.csv"),
  row.names = FALSE
)

model_report <- c(
  "Target: New Customer Volume",
  glue("Final selected model: {final_model_name}"),
  glue("Slide notation: {final_model_notation}"),
  glue("Training period: {training_start} - {training_end}"),
  glue("Validation period: {validation_start} - {validation_end}"),
  glue("Forecast period: {future_start} - {future_end}"),
  "",
  "Models compared: Naive, SNaive, SES, ETS, Base ARIMA, TSLM, NNet",
  "",
  "Validation metrics summary:",
  capture.output(print(validation_metrics %>% select(-internal_model), n = Inf)),
  "",
  glue("Why selected: {final_model_reason}"),
  glue("Business interpretation: {target_business_meaning}"),
  "",
  "Notes / caveats:",
  "- The downloaded New Customer script did not hard-code a final 2025 single-model choice; it printed the lowest validation-RMSE model.",
  "- This standardized version turns that rule into a clear final_model_name for reproducibility.",
  "- Validation metrics are based on 2024 roll-forward forecasts, not training fitted values.",
  "",
  "Final model report:",
  capture.output(report(fit_final))
)

writeLines(model_report, file.path(output_dir, "new_customer_volume_model_report.txt"))

ggsave(file.path(output_dir, "new_customer_volume_validation_selected_models.png"), validation_selected_plot, width = 11, height = 8.5, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "new_customer_volume_validation_selected_models_actual_forecast_only.png"), actual_forecast_plot, width = 11, height = 6.5, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "new_customer_volume_validation_top3_actual_forecast_only.png"), top3_actual_forecast_plot, width = 11, height = 6.5, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "new_customer_volume_validation_errors.png"), error_plot, width = 11, height = 5.8, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "new_customer_volume_validation_all_candidate_models.png"), validation_all_candidate_plot, width = 12, height = 6.5, dpi = 300, bg = "white")
ggsave(file.path(output_dir, "new_customer_volume_2025_final_forecast.png"), final_forecast_plot, width = 11, height = 6.5, dpi = 300, bg = "white")

print("=== NEW CUSTOMER VOLUME STANDARDIZED MODEL RESULTS ===")
print(validation_metrics %>% select(-internal_model), n = Inf)
print(glue("Final selected model: {final_model_name}"))
print("=== OUTPUT DIRECTORY ===")
print(output_dir)
