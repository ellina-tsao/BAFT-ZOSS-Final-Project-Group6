# ============================================================
# ZOSS Retail Product Revenue Share Forecasting
#
# 0. Setup
# 1. Load Data
# 2. Build Monthly Retail Product Revenue Share Series
# 3. Add Calendar Dummy Variables
# 4. Train / Validation / Future Split
# 5. Fit Candidate Models
# 6. Validation Evaluation
# 7. Final Model Refit on Full Historical Data
# 8. 2025 Forecast Generation
# 9. Plot Outputs
# 10. Export Tables and Reports
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

candidate_data_paths <- unique(c(
  file.path(script_dir, "zoss_data.csv"),
  file.path(getwd(), "zoss", "zoss_data.csv"),
  file.path(getwd(), "zoss_data.csv")
))

data_path <- candidate_data_paths[file.exists(candidate_data_paths)][1]

if (is.na(data_path) || !file.exists(data_path)) {
  stop("Cannot find zoss_data.csv. Checked: ", paste(candidate_data_paths, collapse = ", "))
}

output_dir <- file.path(dirname(data_path), "retail_revenue_share_calendar_outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

training_start <- yearmonth("2018 Jul")
training_end <- yearmonth("2023 Dec")
validation_start <- yearmonth("2024 Jan")
validation_end <- yearmonth("2024 Dec")
future_start <- yearmonth("2025 Jan")
future_end <- yearmonth("2025 Dec")

covid_months <- yearmonth(c(
  "2021 May",
  "2021 Jun",
  "2021 Jul",
  "2021 Aug",
  "2021 Sep"
))

covid_start_date <- as.Date(min(covid_months))
covid_end_date <- as.Date(max(covid_months)) %m+% months(1) - days(1)

final_model_internal <- "ARIMA_Jan_Feb"
final_model_name <- "ARIMA + Jan-Feb"
final_model_notation <- "ARIMA(1,0,0)(0,0,0)[12] with Jan-Feb dummy"

model_display <- c(
  "SNaive" = "SNaive",
  "Base_ARIMA" = "Base ARIMA",
  "ARIMA_Jan" = "ARIMA + Jan",
  "ARIMA_Jan_Feb" = "ARIMA + Jan-Feb",
  "ARIMA_CNY_Pre" = "ARIMA + pre-CNY",
  "ARIMA_CNY_Season" = "ARIMA + CNY season"
)

model_colors <- c(
  "Actual" = "black",
  "SNaive" = "#B58900",
  "Base ARIMA" = "#1F9EEA",
  "ARIMA + Jan" = "#D55E00",
  "ARIMA + Jan-Feb" = "#CC79A7",
  "ARIMA + pre-CNY" = "#009E73",
  "ARIMA + CNY season" = "#6A3D9A"
)

model_reasons <- c(
  "SNaive" = "Seasonal naive benchmark using the same month last year.",
  "Base_ARIMA" = "Baseline ARIMA benchmark without external calendar predictors.",
  "ARIMA_Jan" = "Tests whether the early-year lift is concentrated in January only.",
  "ARIMA_Jan_Feb" = "Final selected model; tests a broader January-February early-year lift.",
  "ARIMA_CNY_Pre" = "Tests the month before Lunar New Year as a business-timed retail lift.",
  "ARIMA_CNY_Season" = "Tests Lunar New Year month plus the previous month as a business-timed retail season."
)

display_model <- function(model_id) {
  recode(model_id, !!!model_display, .default = model_id)
}

benchmark_model_internal <- "Base_ARIMA"
benchmark_model_name <- display_model(benchmark_model_internal)
evaluation_models_internal <- c("SNaive", "Base_ARIMA", final_model_internal)
evaluation_models_display <- display_model(evaluation_models_internal)

percent_axis <- scales::label_percent(accuracy = 0.1)


# ============================================================
# 1. Load Data
# ============================================================

raw_data <- read.csv(data_path, stringsAsFactors = FALSE)

# Preserve the existing project treatment: the first row is an invalid/test row
# outside the 2018 Jul analysis window.
raw_data <- raw_data[-1, ]

required_data <- raw_data %>%
  transmute(
    total_revenue = as.numeric(Total.Amount),
    retail_product_revenue = as.numeric(Retail.Product.Spending.Amount),
    year = as.numeric(Year),
    month = as.numeric(Month)
  ) %>%
  mutate(
    retail_product_revenue = coalesce(retail_product_revenue, 0)
  )


# ============================================================
# 2. Build Monthly Retail Product Revenue Share Series
# ============================================================

monthly_retail_share <- required_data %>%
  filter(!is.na(year), !is.na(month), !is.na(total_revenue)) %>%
  group_by(year, month) %>%
  summarise(
    retail_product_revenue = sum(retail_product_revenue, na.rm = TRUE),
    total_revenue = sum(total_revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    month_index = yearmonth(paste(year, month, sep = "-")),
    month_date = as.Date(month_index),
    retail_product_revenue_share = if_else(
      total_revenue > 0,
      retail_product_revenue / total_revenue,
      NA_real_
    )
  ) %>%
  filter(
    !is.na(retail_product_revenue_share),
    month_index >= training_start,
    month_index <= validation_end
  ) %>%
  arrange(month_index)


# ============================================================
# 3. Add Calendar Dummy Variables
# ============================================================

cny_calendar <- tibble(
  year = c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
  cny_date = as.Date(c(
    "2018-02-16",
    "2019-02-05",
    "2020-01-25",
    "2021-02-12",
    "2022-02-01",
    "2023-01-22",
    "2024-02-10",
    "2025-01-29"
  ))
) %>%
  mutate(
    cny_month = yearmonth(cny_date),
    cny_pre_month = yearmonth(cny_date %m-% months(1))
  )

cny_pre_months <- unique(cny_calendar$cny_pre_month)
cny_season_months <- unique(c(cny_calendar$cny_pre_month, cny_calendar$cny_month))

add_calendar_dummies <- function(data_tbl) {
  data_tbl %>%
    mutate(
      month_date = as.Date(month_index),
      month_num = month(month_date),
      jan_dummy = if_else(month_num == 1, 1, 0),
      jan_feb_dummy = if_else(month_num %in% c(1, 2), 1, 0),
      cny_pre_dummy = if_else(month_index %in% cny_pre_months, 1, 0),
      cny_season_dummy = if_else(month_index %in% cny_season_months, 1, 0),
      covid_modeling_period = month_index %in% covid_months
    )
}

retail_share_ts <- monthly_retail_share %>%
  add_calendar_dummies() %>%
  as_tsibble(index = month_index)


# ============================================================
# 4. Train / Validation / Future Split
# ============================================================

train_data <- retail_share_ts %>%
  filter(month_index >= training_start, month_index <= training_end)

validation_actuals <- retail_share_ts %>%
  filter(month_index >= validation_start, month_index <= validation_end)

full_history <- retail_share_ts %>%
  filter(month_index >= training_start, month_index <= validation_end)

# Roll-forward validation:
# Fit each model on data available up to each month, then forecast the next
# month. This creates 12 one-step-ahead validation forecasts for 2024.
cv_data <- retail_share_ts %>%
  filter(month_index >= training_start, month_index <= yearmonth("2024 Nov")) %>%
  stretch_tsibble(.init = nrow(train_data), .step = 1)

make_roll_forward_future_data <- function(cv_tbl) {
  cv_tbl %>%
    as_tibble() %>%
    group_by(.id) %>%
    summarise(month_index = max(month_index) + 1, .groups = "drop") %>%
    as_tsibble(key = .id, index = month_index) %>%
    add_calendar_dummies()
}

make_future_data <- function(history_tbl, h = 12) {
  new_data(history_tbl, h) %>%
    add_calendar_dummies()
}


# ============================================================
# 5. Fit Candidate Models
# ============================================================

fit_candidate_models <- function(data_tbl) {
  data_tbl %>%
    model(
      SNaive = SNAIVE(retail_product_revenue_share ~ lag("year")),
      Base_ARIMA = ARIMA(retail_product_revenue_share),
      ARIMA_Jan = ARIMA(retail_product_revenue_share ~ jan_dummy + pdq() + PDQ()),
      ARIMA_Jan_Feb = ARIMA(retail_product_revenue_share ~ jan_feb_dummy + pdq() + PDQ()),
      ARIMA_CNY_Pre = ARIMA(retail_product_revenue_share ~ cny_pre_dummy + pdq() + PDQ()),
      ARIMA_CNY_Season = ARIMA(retail_product_revenue_share ~ cny_season_dummy + pdq() + PDQ())
    )
}

fit_single_model <- function(data_tbl, model_id) {
  switch(
    model_id,
    SNaive = data_tbl %>% model(Selected = SNAIVE(retail_product_revenue_share ~ lag("year"))),
    Base_ARIMA = data_tbl %>% model(Selected = ARIMA(retail_product_revenue_share)),
    ARIMA_Jan = data_tbl %>% model(Selected = ARIMA(retail_product_revenue_share ~ jan_dummy + pdq() + PDQ())),
    ARIMA_Jan_Feb = data_tbl %>% model(Selected = ARIMA(retail_product_revenue_share ~ jan_feb_dummy + pdq() + PDQ())),
    ARIMA_CNY_Pre = data_tbl %>% model(Selected = ARIMA(retail_product_revenue_share ~ cny_pre_dummy + pdq() + PDQ())),
    ARIMA_CNY_Season = data_tbl %>% model(Selected = ARIMA(retail_product_revenue_share ~ cny_season_dummy + pdq() + PDQ())),
    stop("Unsupported model_id: ", model_id)
  )
}

candidate_roll_models <- fit_candidate_models(cv_data)
roll_future_data <- make_roll_forward_future_data(cv_data)
roll_forecasts <- candidate_roll_models %>%
  forecast(new_data = roll_future_data)

candidate_train_models <- fit_candidate_models(train_data)


# ============================================================
# 6. Validation Evaluation
# ============================================================

validation_forecasts <- roll_forecasts %>%
  as_tibble() %>%
  transmute(
    fold_id = .id,
    internal_model = .model,
    model = display_model(.model),
    month_index,
    month_date = as.Date(month_index),
    forecast = .mean
  )

training_fitted_values <- candidate_train_models %>%
  augment() %>%
  as_tibble() %>%
  transmute(
    internal_model = .model,
    model = display_model(.model),
    month_index,
    month_date = as.Date(month_index),
    fitted = .fitted
  ) %>%
  filter(!is.na(fitted))

training_model_info_raw <- candidate_train_models %>%
  glance() %>%
  as_tibble()

training_model_info <- training_model_info_raw %>%
  transmute(
    internal_model = .model,
    AICc = if ("AICc" %in% names(training_model_info_raw)) round(AICc, 2) else NA_real_,
    Model_Summary_Info = if ("ar_roots" %in% names(training_model_info_raw)) {
      "AICc from model fit on 2018 Jul-2023 Dec"
    } else {
      "AICc from model fit on 2018 Jul-2023 Dec where available"
    }
  )

validation_metrics <- accuracy(roll_forecasts, retail_share_ts) %>%
  as_tibble() %>%
  transmute(
    internal_model = .model,
    Model = display_model(.model),
    RMSE = round(RMSE, 4),
    MAE = round(MAE, 4),
    MAPE = round(MAPE, 2)
  ) %>%
  left_join(training_model_info, by = "internal_model") %>%
  mutate(
    `Reason for inclusion` = unname(model_reasons[internal_model]),
    Selected_Final_Model = internal_model == final_model_internal
  ) %>%
  select(
    Model,
    RMSE,
    MAE,
    MAPE,
    AICc,
    Model_Summary_Info,
    `Reason for inclusion`,
    Selected_Final_Model,
    internal_model
  ) %>%
  arrange(RMSE)


# ============================================================
# 7. Final Model Refit on Full Historical Data
# ============================================================

# Final model selected from validation:
# ARIMA(1,0,0)(0,0,0)[12] with Jan-Feb dummy.
# The model is refit on the full historical window, 2018 Jul-2024 Dec, before
# forecasting 2025.
final_fit <- fit_single_model(full_history, final_model_internal)
final_report_lines <- capture.output(report(final_fit))

final_fitted_values <- final_fit %>%
  augment() %>%
  as_tibble() %>%
  transmute(
    month_index,
    month_date = as.Date(month_index),
    fitted = .fitted
  ) %>%
  filter(!is.na(fitted))


# ============================================================
# 8. 2025 Forecast Generation
# ============================================================

future_data <- make_future_data(full_history, h = 12)
final_forecast_raw <- final_fit %>%
  forecast(new_data = future_data)

final_forecast <- final_forecast_raw %>%
  hilo(level = c(80, 95)) %>%
  unpack_hilo(c(`80%`, `95%`)) %>%
  as_tibble() %>%
  transmute(
    Month = month_index,
    Month_Date = as.Date(month_index),
    Target = "Retail Product Revenue Share",
    Selected_Model = final_model_name,
    Model_Notation = final_model_notation,
    Forecast = round(.mean, 4),
    Lower_80 = round(`80%_lower`, 4),
    Upper_80 = round(`80%_upper`, 4),
    Lower_95 = round(`95%_lower`, 4),
    Upper_95 = round(`95%_upper`, 4)
  )


# ============================================================
# 9. Plot Outputs
# ============================================================

plot_final_forecast <- function() {
  ggplot() +
    annotate(
      "rect",
      xmin = covid_start_date,
      xmax = covid_end_date,
      ymin = -Inf,
      ymax = Inf,
      fill = "gray80",
      alpha = 0.35
    ) +
    geom_line(
      data = full_history,
      aes(x = month_date, y = retail_product_revenue_share, color = "Actual"),
      linewidth = 1.05
    ) +
    geom_line(
      data = final_fitted_values,
      aes(x = month_date, y = fitted, color = "Fitted ARIMA + Jan-Feb"),
      linewidth = 0.65,
      linetype = "dashed",
      alpha = 0.65
    ) +
    geom_ribbon(
      data = final_forecast,
      aes(x = Month_Date, ymin = Lower_95, ymax = Upper_95),
      fill = "#1F9EEA",
      alpha = 0.12
    ) +
    geom_ribbon(
      data = final_forecast,
      aes(x = Month_Date, ymin = Lower_80, ymax = Upper_80),
      fill = "#1F9EEA",
      alpha = 0.22
    ) +
    geom_line(
      data = final_forecast,
      aes(x = Month_Date, y = Forecast, color = "2025 ARIMA + Jan-Feb Forecast"),
      linewidth = 1.2
    ) +
    geom_point(
      data = final_forecast,
      aes(x = Month_Date, y = Forecast, color = "2025 ARIMA + Jan-Feb Forecast"),
      size = 2
    ) +
    geom_vline(
      xintercept = as.Date(future_start),
      color = "gray45",
      linewidth = 0.8,
      linetype = "dotted"
    ) +
    annotate(
      "text",
      x = as.Date(yearmonth("2024 Oct")),
      y = max(final_forecast$Upper_95, full_history$retail_product_revenue_share, na.rm = TRUE),
      label = "Forecast starts\n2025 Jan",
      hjust = 1,
      vjust = 1,
      color = "gray45",
      size = 3.8
    ) +
    scale_color_manual(
      values = c(
        "Actual" = "black",
        "Fitted ARIMA + Jan-Feb" = "#E69F00",
        "2025 ARIMA + Jan-Feb Forecast" = "#1F9EEA"
      )
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(min(full_history$month_date), as.Date(future_end)),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(labels = percent_axis) +
    labs(
      title = "Retail Product Revenue Share Forecast for 2025 using ARIMA(1,0,0)(0,0,0)[12] with Jan-Feb dummy",
      subtitle = "Shaded bands show 80% and 95% forecast intervals",
      x = NULL,
      y = NULL,
      color = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "gray35", size = 12),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      panel.grid.minor = element_blank()
    )
}

plot_validation_comparison <- function(models_to_plot, title, subtitle) {
  selected_forecasts <- validation_forecasts %>%
    filter(internal_model %in% models_to_plot) %>%
    mutate(model = factor(model, levels = display_model(models_to_plot)))

  ggplot() +
    geom_line(
      data = full_history,
      aes(x = month_date, y = retail_product_revenue_share, color = "Actual"),
      linewidth = 1.05
    ) +
    geom_line(
      data = selected_forecasts,
      aes(x = month_date, y = forecast, color = model),
      linewidth = 1.05,
      alpha = 0.95
    ) +
    geom_point(
      data = selected_forecasts,
      aes(x = month_date, y = forecast, color = model),
      size = 1.8,
      alpha = 0.95
    ) +
    geom_vline(
      xintercept = as.Date(validation_start),
      color = "gray45",
      linewidth = 0.8,
      linetype = "dotted"
    ) +
    annotate(
      "text",
      x = as.Date(yearmonth("2023 Sep")),
      y = max(full_history$retail_product_revenue_share, selected_forecasts$forecast, na.rm = TRUE),
      label = "Training\n2018-2023",
      hjust = 1,
      vjust = 1,
      color = "gray35",
      size = 4.4,
      lineheight = 0.92
    ) +
    annotate(
      "text",
      x = as.Date(yearmonth("2024 Feb")),
      y = max(full_history$retail_product_revenue_share, selected_forecasts$forecast, na.rm = TRUE),
      label = "Validation\n2024",
      hjust = 0,
      vjust = 1,
      color = "gray35",
      size = 4.4,
      lineheight = 0.92
    ) +
    scale_color_manual(
      values = model_colors,
      breaks = c("Actual", display_model(models_to_plot))
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(min(full_history$month_date), max(full_history$month_date)),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(labels = percent_axis) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      color = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "gray35", size = 12),
      legend.position = "right",
      legend.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
}

plot_evaluation_actual_forecast <- function() {
  evaluation_training_fitted <- training_fitted_values %>%
    filter(internal_model %in% evaluation_models_internal) %>%
    mutate(model = factor(model, levels = evaluation_models_display)) %>%
    mutate(series = "Training fitted")

  evaluation_validation_forecast <- validation_forecasts %>%
    filter(internal_model %in% evaluation_models_internal) %>%
    mutate(model = factor(model, levels = evaluation_models_display)) %>%
    mutate(series = "Benchmark validation forecast")

  future_forecast_line <- final_forecast %>%
    transmute(
      month_date = Month_Date,
      forecast = Forecast,
      Lower_80,
      Upper_80,
      Lower_95,
      Upper_95,
      series = "Future forecast"
    )

  y_top <- max(
    full_history$retail_product_revenue_share,
    evaluation_training_fitted$fitted,
    evaluation_validation_forecast$forecast,
    final_forecast$Upper_95,
    na.rm = TRUE
  )

  ggplot() +
    annotate(
      "rect",
      xmin = covid_start_date,
      xmax = covid_end_date,
      ymin = -Inf,
      ymax = Inf,
      fill = "gray80",
      alpha = 0.28
    ) +
    geom_ribbon(
      data = future_forecast_line,
      aes(x = month_date, ymin = Lower_95, ymax = Upper_95),
      fill = "#CC79A7",
      alpha = 0.10
    ) +
    geom_ribbon(
      data = future_forecast_line,
      aes(x = month_date, ymin = Lower_80, ymax = Upper_80),
      fill = "#CC79A7",
      alpha = 0.20
    ) +
    geom_line(
      data = full_history,
      aes(x = month_date, y = retail_product_revenue_share, color = "Actual", linetype = "Actual"),
      linewidth = 1.05
    ) +
    geom_line(
      data = evaluation_training_fitted,
      aes(x = month_date, y = fitted, color = model, linetype = "Training fitted"),
      linewidth = 0.75,
      alpha = 0.75
    ) +
    geom_line(
      data = evaluation_validation_forecast,
      aes(x = month_date, y = forecast, color = model, linetype = "Validation forecast"),
      linewidth = 1.1,
      alpha = 0.95
    ) +
    geom_point(
      data = evaluation_validation_forecast,
      aes(x = month_date, y = forecast, color = model),
      size = 1.9,
      alpha = 0.95
    ) +
    geom_line(
      data = future_forecast_line,
      aes(x = month_date, y = forecast, color = final_model_name, linetype = "Future forecast"),
      linewidth = 1.15,
      alpha = 0.98
    ) +
    geom_point(
      data = future_forecast_line,
      aes(x = month_date, y = forecast, color = final_model_name),
      size = 1.9,
      alpha = 0.98
    ) +
    geom_vline(
      xintercept = as.Date(validation_start),
      color = "gray45",
      linewidth = 0.85,
      linetype = "dotted"
    ) +
    geom_vline(
      xintercept = as.Date(future_start),
      color = "gray45",
      linewidth = 0.85,
      linetype = "dotted"
    ) +
    annotate(
      "text",
      x = as.Date(yearmonth("2023 Sep")),
      y = y_top,
      label = "Training\n2018-2023",
      hjust = 1,
      vjust = 1,
      color = "gray35",
      size = 4.4,
      lineheight = 0.92
    ) +
    annotate(
      "text",
      x = as.Date(yearmonth("2024 Feb")),
      y = y_top,
      label = "Validation\n2024",
      hjust = 0,
      vjust = 1,
      color = "gray35",
      size = 4.4,
      lineheight = 0.92
    ) +
    annotate(
      "text",
      x = as.Date(yearmonth("2025 Feb")),
      y = y_top,
      label = "Future\n2025",
      hjust = 0,
      vjust = 1,
      color = "gray35",
      size = 4.4,
      lineheight = 0.92
    ) +
    scale_color_manual(
      values = c(
        "Actual" = "black",
        model_colors[evaluation_models_display]
      ),
      breaks = c("Actual", evaluation_models_display)
    ) +
    scale_linetype_manual(
      values = c(
        "Actual" = "solid",
        "Training fitted" = "dashed",
        "Validation forecast" = "solid",
        "Future forecast" = "longdash"
      ),
      breaks = c("Actual", "Training fitted", "Validation forecast", "Future forecast")
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(min(full_history$month_date), as.Date(future_end)),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(labels = percent_axis) +
    labs(
      title = "Retail Product Revenue Share: Actual, Validation Forecast, and 2025 Forecast",
      subtitle = "Training fitted values, 2024 roll-forward validation forecasts, and the final 2025 forecast after refitting on full history.",
      x = NULL,
      y = NULL,
      color = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "gray35", size = 11.5),
      legend.position = "right",
      legend.text = element_text(size = 12),
      legend.spacing.y = grid::unit(0.12, "cm"),
      panel.grid.minor = element_blank()
    )
}

plot_validation_forecast_errors <- function() {
  actual_lookup <- full_history %>%
    as_tibble() %>%
    select(month_index, actual = retail_product_revenue_share)

  training_errors <- training_fitted_values %>%
    filter(internal_model %in% evaluation_models_internal) %>%
    left_join(actual_lookup, by = "month_index") %>%
    transmute(
      month_index,
      month_date,
      model = factor(model, levels = evaluation_models_display),
      error_type = "Training fitted error",
      forecast_error = actual - fitted
    )

  validation_errors <- validation_forecasts %>%
    filter(internal_model %in% evaluation_models_internal) %>%
    left_join(actual_lookup, by = "month_index") %>%
    transmute(
      month_index,
      month_date,
      model = factor(model, levels = evaluation_models_display),
      error_type = "Validation forecast error",
      forecast_error = actual - forecast
    )

  error_tbl <- bind_rows(training_errors, validation_errors) %>%
    filter(!is.na(forecast_error))

  ggplot(error_tbl, aes(x = month_date, y = forecast_error, color = model, linetype = error_type)) +
    geom_hline(yintercept = 0, color = "gray35", linewidth = 0.85) +
    annotate(
      "rect",
      xmin = as.Date(validation_start),
      xmax = as.Date(validation_end),
      ymin = -Inf,
      ymax = Inf,
      fill = "#EAF4FF",
      alpha = 0.32
    ) +
    geom_vline(
      xintercept = as.Date(validation_start),
      color = "gray45",
      linewidth = 0.85,
      linetype = "dotted"
    ) +
    geom_line(linewidth = 0.95, alpha = 0.9) +
    geom_point(
      data = error_tbl %>% filter(error_type == "Validation forecast error"),
      size = 2.1,
      alpha = 0.95
    ) +
    annotate(
      "text",
      x = as.Date(yearmonth("2024 Jun")),
      y = max(error_tbl$forecast_error, na.rm = TRUE),
      label = "Validation errors",
      hjust = 0.5,
      vjust = 1,
      color = "gray35",
      size = 4.4,
      lineheight = 0.92
    ) +
    scale_color_manual(
      values = model_colors[evaluation_models_display],
      breaks = evaluation_models_display
    ) +
    scale_linetype_manual(
      values = c(
        "Training fitted error" = "dashed",
        "Validation forecast error" = "solid"
      ),
      breaks = c("Training fitted error", "Validation forecast error")
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(min(full_history$month_date), as.Date(validation_end)),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(labels = percent_axis) +
    labs(
      title = "Retail Product Revenue Share: Validation Forecast Errors",
      subtitle = "Errors are actual minus forecast. Future errors are not plotted because 2025 actuals are unknown.",
      x = NULL,
      y = NULL,
      color = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "gray35", size = 11.5),
      legend.position = "right",
      legend.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
}

final_forecast_plot <- plot_final_forecast()

evaluation_actual_forecast_plot <- plot_evaluation_actual_forecast()
validation_forecast_error_plot <- plot_validation_forecast_errors()

methods_evaluation_combined_plot <- (
  (
    evaluation_actual_forecast_plot +
      labs(title = NULL, subtitle = NULL) +
      theme(legend.position = "right")
  ) /
    (
      validation_forecast_error_plot +
        labs(title = NULL, subtitle = NULL) +
        guides(color = "none") +
        theme(legend.position = "right")
    )
) +
  plot_layout(heights = c(2.05, 1), guides = "collect") &
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.spacing.y = grid::unit(0.1, "cm")
  )

validation_selected_plot <- plot_validation_comparison(
  models_to_plot = c("SNaive", "Base_ARIMA", "ARIMA_Jan_Feb"),
  title = "Retail Product Revenue Share: Why Use Calendar-Adjusted ARIMA?",
  subtitle = "Compared with SNaive and Base ARIMA, the Jan-Feb dummy improves validation accuracy and captures the early-year lift."
)

validation_all_calendar_plot <- plot_validation_comparison(
  models_to_plot = c(
    "SNaive",
    "Base_ARIMA",
    "ARIMA_Jan",
    "ARIMA_Jan_Feb",
    "ARIMA_CNY_Pre",
    "ARIMA_CNY_Season"
  ),
  title = "Retail Product Revenue Share: Calendar Model Comparison",
  subtitle = "Appendix view comparing all calendar-adjusted ARIMA candidates against SNaive and Base ARIMA."
)


# ============================================================
# 10. Export Tables and Reports
# ============================================================

write.csv(
  validation_metrics %>% select(-internal_model),
  file.path(output_dir, "retail_product_revenue_share_calendar_model_results.csv"),
  row.names = FALSE
)

write.csv(
  final_forecast,
  file.path(output_dir, "retail_product_revenue_share_2025_final_forecast.csv"),
  row.names = FALSE
)

write.csv(
  retail_share_ts %>%
    as_tibble() %>%
    select(
      month_index,
      month_date,
      retail_product_revenue,
      total_revenue,
      retail_product_revenue_share,
      jan_dummy,
      jan_feb_dummy,
      cny_pre_dummy,
      cny_season_dummy,
      covid_modeling_period
    ),
  file.path(output_dir, "retail_product_revenue_share_modeling_series_audit.csv"),
  row.names = FALSE
)

model_report <- c(
  "Retail Product Revenue Share Forecasting Report",
  "",
  glue("Training period: {training_start} - {training_end}"),
  glue("Validation period: {validation_start} - {validation_end}"),
  glue("Future forecast period: {future_start} - {future_end}"),
  glue("Final selected model: {final_model_name}"),
  glue("Final model notation: {final_model_notation}"),
  "",
  "Validation metrics are based on 2024 roll-forward one-step-ahead forecasts.",
  "",
  "Validation comparison table:",
  capture.output(print(validation_metrics %>% select(-internal_model), n = Inf)),
  "",
  "Final model report:",
  final_report_lines
)

writeLines(
  model_report,
  file.path(output_dir, "retail_product_revenue_share_model_report.txt")
)

ggsave(
  file.path(output_dir, "retail_product_revenue_share_2025_final_arima_jan_feb_forecast.png"),
  final_forecast_plot,
  width = 11,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(output_dir, "retail_product_revenue_share_validation_selected_models.png"),
  methods_evaluation_combined_plot,
  width = 11,
  height = 8.5,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(output_dir, "retail_product_revenue_share_methods_evaluation_performance_chart.png"),
  methods_evaluation_combined_plot,
  width = 11,
  height = 8.5,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(output_dir, "retail_product_revenue_share_methods_actual_forecast.png"),
  evaluation_actual_forecast_plot,
  width = 12,
  height = 6.8,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(output_dir, "retail_product_revenue_share_validation_forecast_errors.png"),
  validation_forecast_error_plot,
  width = 11,
  height = 5.8,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(output_dir, "retail_product_revenue_share_validation_all_calendar_models.png"),
  validation_all_calendar_plot,
  width = 12,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

print("=== RETAIL PRODUCT REVENUE SHARE CALENDAR MODEL RESULTS ===")
print(validation_metrics %>% select(-internal_model), n = Inf)
print("=== FINAL FORECAST OUTPUT DIRECTORY ===")
print(output_dir)
