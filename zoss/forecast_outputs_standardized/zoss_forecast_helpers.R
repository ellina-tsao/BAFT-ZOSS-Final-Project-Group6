# Shared helpers for standardized ZOSS forecasting outputs.

resolve_zoss_data_path <- function(script_dir) {
  candidate_data_paths <- unique(c(
    file.path(script_dir, "zoss_data.csv"),
    file.path(dirname(script_dir), "zoss_data.csv"),
    file.path(dirname(dirname(script_dir)), "zoss_data.csv"),
    file.path(getwd(), "zoss", "zoss_data.csv"),
    file.path(getwd(), "zoss_data.csv")
  ))

  data_path <- candidate_data_paths[file.exists(candidate_data_paths)][1]

  if (is.na(data_path) || !file.exists(data_path)) {
    stop("Cannot find zoss_data.csv. Checked: ", paste(candidate_data_paths, collapse = ", "))
  }

  data_path
}

source_script_dir <- function() {
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

  if (length(script_file) == 1 && !is.na(script_file) && nzchar(script_file)) {
    dirname(script_file)
  } else {
    getwd()
  }
}

display_model <- function(model_id, model_display) {
  dplyr::recode(model_id, !!!model_display, .default = model_id)
}

fit_candidate_models <- function(data_tbl,
                                 include_nnet = FALSE,
                                 include_selected_arima = FALSE,
                                 snaive_lag_year = FALSE) {
  if (include_nnet && include_selected_arima && !snaive_lag_year) {
    return(
      data_tbl %>%
        model(
          Naive = NAIVE(Value),
          SNaive = SNAIVE(Value),
          SES = ETS(Value ~ error("A") + trend("N") + season("N")),
          ETS = ETS(Value),
          Base_ARIMA = ARIMA(Value),
          ARIMA_Selected = ARIMA(Value ~ pdq(1, 0, 1) + PDQ(1, 1, 0)),
          TSLM = TSLM(Value ~ trend() + season()),
          NNet = NNETAR(Value)
        )
    )
  }

  if (include_nnet && !include_selected_arima && !snaive_lag_year) {
    return(
      data_tbl %>%
        model(
          Naive = NAIVE(Value),
          SNaive = SNAIVE(Value),
          SES = ETS(Value ~ error("A") + trend("N") + season("N")),
          ETS = ETS(Value),
          Base_ARIMA = ARIMA(Value),
          TSLM = TSLM(Value ~ trend() + season()),
          NNet = NNETAR(Value)
        )
    )
  }

  if (!include_nnet && !include_selected_arima && snaive_lag_year) {
    return(
      data_tbl %>%
        model(
          Naive = NAIVE(Value),
          SNaive = SNAIVE(Value ~ lag("year")),
          SES = ETS(Value ~ error("A") + trend("N") + season("N")),
          ETS = ETS(Value),
          Base_ARIMA = ARIMA(Value),
          TSLM = TSLM(Value ~ trend() + season())
        )
    )
  }

  data_tbl %>%
    model(
      Naive = NAIVE(Value),
      SNaive = SNAIVE(Value),
      SES = ETS(Value ~ error("A") + trend("N") + season("N")),
      ETS = ETS(Value),
      Base_ARIMA = ARIMA(Value),
      TSLM = TSLM(Value ~ trend() + season())
    )
}

fit_final_model <- function(data_tbl, final_model_internal, snaive_lag_year = FALSE) {
  switch(
    final_model_internal,
    Naive = data_tbl %>% model(Selected = NAIVE(Value)),
    SNaive = if (snaive_lag_year) {
      data_tbl %>% model(Selected = SNAIVE(Value ~ lag("year")))
    } else {
      data_tbl %>% model(Selected = SNAIVE(Value))
    },
    SES = data_tbl %>% model(Selected = ETS(Value ~ error("A") + trend("N") + season("N"))),
    ETS = data_tbl %>% model(Selected = ETS(Value)),
    Base_ARIMA = data_tbl %>% model(Selected = ARIMA(Value)),
    ARIMA_Selected = data_tbl %>% model(Selected = ARIMA(Value ~ pdq(1, 0, 1) + PDQ(1, 1, 0))),
    TSLM = data_tbl %>% model(Selected = TSLM(Value ~ trend() + season())),
    NNet = data_tbl %>% model(Selected = NNETAR(Value)),
    stop("Unsupported final model: ", final_model_internal)
  )
}

make_validation_metrics <- function(fc_roll,
                                    target_ts,
                                    model_display,
                                    model_reasons,
                                    final_model_internal) {
  accuracy(fc_roll, target_ts) %>%
    as_tibble() %>%
    transmute(
      internal_model = .model,
      Model = display_model(.model, model_display),
      MAE = round(MAE, 2),
      RMSE = round(RMSE, 2),
      MAPE = round(MAPE, 2),
      MASE = round(MASE, 2),
      RMSSE = round(RMSSE, 2),
      `Reason for inclusion` = unname(model_reasons[.model]),
      Selected_Final_Model = .model == final_model_internal
    ) %>%
    arrange(RMSE)
}

forecast_tbl_from_fable <- function(fc_obj) {
  as_tibble(fc_obj) %>%
    mutate(Month_Date = as.Date(Month_Index)) %>%
    transmute(
      Fold_ID = .id,
      internal_model = .model,
      Model = .model,
      Month_Index,
      Month_Date,
      Forecast = .mean
    )
}

fitted_tbl_from_mable <- function(fit_obj) {
  augment(fit_obj) %>%
    mutate(Month_Date = as.Date(Month_Index)) %>%
    transmute(
      internal_model = .model,
      Model = .model,
      Month_Index,
      Month_Date,
      Fitted = .fitted
    ) %>%
    filter(!is.na(Fitted))
}

final_forecast_tbl_from_fable <- function(fc_obj,
                                          target_name,
                                          final_model_name,
                                          round_digits = 2,
                                          nonnegative = FALSE) {
  out <- fc_obj %>%
    hilo(level = c(80, 95)) %>%
    unpack_hilo(c(`80%`, `95%`)) %>%
    as_tibble() %>%
    mutate(Month_Date = as.Date(Month_Index))

  value_cols <- out %>%
    transmute(
      Month = Month_Index,
      Month_Date,
      Target = target_name,
      Selected_Model = final_model_name,
      Forecast = .mean,
      Lower_80 = `80%_lower`,
      Upper_80 = `80%_upper`,
      Lower_95 = `95%_lower`,
      Upper_95 = `95%_upper`
    )

  if (nonnegative) {
    value_cols <- value_cols %>%
      mutate(across(c(Forecast, Lower_80, Upper_80, Lower_95, Upper_95), ~ pmax(.x, 0)))
  }

  value_cols %>%
    mutate(across(c(Forecast, Lower_80, Upper_80, Lower_95, Upper_95), ~ round(.x, round_digits)))
}

plot_actual_validation_future <- function(target_ts,
                                          train_fitted_tbl,
                                          validation_forecast_tbl,
                                          final_forecast_tbl,
                                          model_display,
                                          model_colors,
                                          presentation_models_internal,
                                          final_model_internal,
                                          final_model_name,
                                          y_labeler,
                                          training_start,
                                          training_end,
                                          validation_start,
                                          validation_end,
                                          future_start,
                                          future_end,
                                          covid_start_date = NULL,
                                          covid_end_date = NULL) {
  presentation_models_display <- display_model(presentation_models_internal, model_display)

  plot_train <- train_fitted_tbl %>%
    filter(internal_model %in% presentation_models_internal) %>%
    mutate(Model = factor(display_model(internal_model, model_display), levels = presentation_models_display))

  plot_valid <- validation_forecast_tbl %>%
    filter(internal_model %in% presentation_models_internal) %>%
    mutate(Model = factor(display_model(internal_model, model_display), levels = presentation_models_display))

  future_line <- final_forecast_tbl %>%
    transmute(
      Month_Date,
      Forecast,
      Lower_80,
      Upper_80,
      Lower_95,
      Upper_95,
      Model = final_model_name
    )

  y_top <- max(
    target_ts$Value,
    plot_train$Fitted,
    plot_valid$Forecast,
    future_line$Upper_95,
    na.rm = TRUE
  )

  p <- ggplot()

  if (!is.null(covid_start_date) && !is.null(covid_end_date)) {
    p <- p +
      annotate(
        "rect",
        xmin = covid_start_date,
        xmax = covid_end_date,
        ymin = -Inf,
        ymax = Inf,
        fill = "gray80",
        alpha = 0.25
      )
  }

  p +
    geom_ribbon(
      data = future_line,
      aes(x = Month_Date, ymin = Lower_95, ymax = Upper_95),
      fill = unname(model_colors[final_model_name]),
      alpha = 0.10
    ) +
    geom_ribbon(
      data = future_line,
      aes(x = Month_Date, ymin = Lower_80, ymax = Upper_80),
      fill = unname(model_colors[final_model_name]),
      alpha = 0.20
    ) +
    geom_line(
      data = target_ts,
      aes(x = Month_Date, y = Value, color = "Actual", linetype = "Actual"),
      linewidth = 1.05
    ) +
    geom_line(
      data = plot_train,
      aes(x = Month_Date, y = Fitted, color = Model, linetype = "Training fitted"),
      linewidth = 0.75,
      alpha = 0.75
    ) +
    geom_line(
      data = plot_valid,
      aes(x = Month_Date, y = Forecast, color = Model, linetype = "Validation forecast"),
      linewidth = 1.1,
      alpha = 0.95
    ) +
    geom_point(
      data = plot_valid,
      aes(x = Month_Date, y = Forecast, color = Model),
      size = 1.9,
      alpha = 0.95
    ) +
    geom_line(
      data = future_line,
      aes(x = Month_Date, y = Forecast, color = Model, linetype = "Future forecast"),
      linewidth = 1.15
    ) +
    geom_point(
      data = future_line,
      aes(x = Month_Date, y = Forecast, color = Model),
      size = 1.9
    ) +
    geom_vline(xintercept = as.Date(validation_start), color = "gray45", linewidth = 0.85, linetype = "dotted") +
    geom_vline(xintercept = as.Date(future_start), color = "gray45", linewidth = 0.85, linetype = "dotted") +
    annotate("text", x = as.Date(yearmonth("2023 Sep")), y = y_top, label = "Training\n2018-2023", hjust = 1, vjust = 1, color = "gray35", size = 4.4, lineheight = 0.92) +
    annotate("text", x = as.Date(yearmonth("2024 Feb")), y = y_top, label = "Validation\n2024", hjust = 0, vjust = 1, color = "gray35", size = 4.4, lineheight = 0.92) +
    annotate("text", x = as.Date(yearmonth("2025 Feb")), y = y_top, label = "Future\n2025", hjust = 0, vjust = 1, color = "gray35", size = 4.4, lineheight = 0.92) +
    scale_color_manual(
      values = c("Actual" = "black", model_colors[presentation_models_display]),
      breaks = c("Actual", presentation_models_display)
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
      limits = c(min(target_ts$Month_Date), as.Date(future_end)),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(labels = y_labeler) +
    labs(x = NULL, y = NULL, color = NULL, linetype = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 12),
      legend.spacing.y = grid::unit(0.12, "cm"),
      panel.grid.minor = element_blank()
    )
}

plot_forecast_errors <- function(target_ts,
                                 train_fitted_tbl,
                                 validation_forecast_tbl,
                                 model_display,
                                 model_colors,
                                 presentation_models_internal,
                                 y_labeler,
                                 validation_start,
                                 validation_end) {
  presentation_models_display <- display_model(presentation_models_internal, model_display)

  actual_lookup <- target_ts %>%
    as_tibble() %>%
    select(Month_Index, Actual = Value)

  training_errors <- train_fitted_tbl %>%
    as_tibble() %>%
    filter(internal_model %in% presentation_models_internal) %>%
    left_join(actual_lookup, by = "Month_Index") %>%
    transmute(
      Month_Date,
      Model = factor(display_model(internal_model, model_display), levels = presentation_models_display),
      Error_Type = "Training fitted error",
      Forecast_Error = Actual - Fitted
    )

  validation_errors <- validation_forecast_tbl %>%
    as_tibble() %>%
    filter(internal_model %in% presentation_models_internal) %>%
    left_join(actual_lookup, by = "Month_Index") %>%
    transmute(
      Month_Date,
      Model = factor(display_model(internal_model, model_display), levels = presentation_models_display),
      Error_Type = "Validation forecast error",
      Forecast_Error = Actual - Forecast
    )

  error_tbl <- bind_rows(training_errors, validation_errors) %>%
    filter(!is.na(Forecast_Error))

  ggplot(error_tbl, aes(x = Month_Date, y = Forecast_Error, color = Model, linetype = Error_Type)) +
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
    geom_vline(xintercept = as.Date(validation_start), color = "gray45", linewidth = 0.85, linetype = "dotted") +
    geom_line(linewidth = 0.95, alpha = 0.9) +
    geom_point(data = error_tbl %>% filter(Error_Type == "Validation forecast error"), size = 2.1, alpha = 0.95) +
    annotate("text", x = as.Date(yearmonth("2024 Jun")), y = max(error_tbl$Forecast_Error, na.rm = TRUE), label = "Validation errors", hjust = 0.5, vjust = 1, color = "gray35", size = 4.4) +
    scale_color_manual(values = model_colors[presentation_models_display], breaks = presentation_models_display) +
    scale_linetype_manual(
      values = c("Training fitted error" = "dashed", "Validation forecast error" = "solid"),
      breaks = c("Training fitted error", "Validation forecast error")
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(min(target_ts$Month_Date), as.Date(validation_end)),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(labels = y_labeler) +
    labs(x = NULL, y = NULL, color = NULL, linetype = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
}

plot_combined_performance <- function(actual_forecast_plot, error_plot) {
  (
    (
      actual_forecast_plot +
        labs(title = NULL, subtitle = NULL) +
        theme(legend.position = "right")
    ) /
      (
        error_plot +
          labs(title = NULL, subtitle = NULL) +
          guides(color = "none") +
          theme(legend.position = "right")
      )
  ) +
    patchwork::plot_layout(heights = c(2.05, 1), guides = "collect") &
    theme(
      legend.position = "right",
      legend.text = element_text(size = 12),
      legend.spacing.y = grid::unit(0.1, "cm")
    )
}

plot_validation_comparison <- function(target_ts,
                                       train_fitted_tbl,
                                       validation_forecast_tbl,
                                       model_display,
                                       model_colors,
                                       models_to_plot_internal,
                                       y_labeler,
                                       title,
                                       subtitle,
                                       validation_start,
                                       validation_end) {
  models_to_plot_display <- display_model(models_to_plot_internal, model_display)

  plot_train <- train_fitted_tbl %>%
    filter(internal_model %in% models_to_plot_internal) %>%
    mutate(Model = factor(display_model(internal_model, model_display), levels = models_to_plot_display))

  plot_valid <- validation_forecast_tbl %>%
    filter(internal_model %in% models_to_plot_internal) %>%
    mutate(Model = factor(display_model(internal_model, model_display), levels = models_to_plot_display))

  y_top <- max(target_ts$Value, plot_train$Fitted, plot_valid$Forecast, na.rm = TRUE)

  ggplot() +
    geom_line(data = target_ts, aes(x = Month_Date, y = Value, color = "Actual"), linewidth = 1.05) +
    geom_line(data = plot_train, aes(x = Month_Date, y = Fitted, color = Model), linewidth = 0.55, alpha = 0.45, linetype = "dashed") +
    geom_line(data = plot_valid, aes(x = Month_Date, y = Forecast, color = Model), linewidth = 1.05, alpha = 0.88) +
    geom_point(data = plot_valid, aes(x = Month_Date, y = Forecast, color = Model), size = 1.7, alpha = 0.88) +
    geom_vline(xintercept = as.Date(validation_start), color = "gray45", linewidth = 0.85, linetype = "dotted") +
    annotate("text", x = as.Date(yearmonth("2023 Sep")), y = y_top, label = "Training\n2018-2023", hjust = 1, vjust = 1, color = "gray35", size = 4.4, lineheight = 0.92) +
    annotate("text", x = as.Date(yearmonth("2024 Feb")), y = y_top, label = "Validation\n2024", hjust = 0, vjust = 1, color = "gray35", size = 4.4, lineheight = 0.92) +
    scale_color_manual(
      values = c("Actual" = "black", model_colors[models_to_plot_display]),
      breaks = c("Actual", models_to_plot_display)
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(min(target_ts$Month_Date), as.Date(validation_end)),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(labels = y_labeler) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL, color = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "gray35", size = 11.5),
      legend.position = "right",
      legend.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
}

plot_final_forecast <- function(target_ts,
                                final_fitted_tbl,
                                final_forecast_tbl,
                                final_model_name,
                                final_model_notation,
                                readable_target,
                                y_labeler,
                                y_axis_title,
                                future_start,
                                future_end,
                                covid_start_date = NULL,
                                covid_end_date = NULL,
                                color = "#1F9EEA") {
  color <- unname(color)

  p <- ggplot()

  if (!is.null(covid_start_date) && !is.null(covid_end_date)) {
    p <- p +
      annotate(
        "rect",
        xmin = covid_start_date,
        xmax = covid_end_date,
        ymin = -Inf,
        ymax = Inf,
        fill = "gray80",
        alpha = 0.25
      )
  }

  p +
    geom_line(data = target_ts, aes(x = Month_Date, y = Value, color = "Actual"), linewidth = 1.05) +
    geom_line(data = final_fitted_tbl, aes(x = Month_Date, y = Fitted, color = "Training fitted"), linewidth = 0.65, alpha = 0.65, linetype = "dashed") +
    geom_ribbon(data = final_forecast_tbl, aes(x = Month_Date, ymin = Lower_95, ymax = Upper_95), fill = color, alpha = 0.12) +
    geom_ribbon(data = final_forecast_tbl, aes(x = Month_Date, ymin = Lower_80, ymax = Upper_80), fill = color, alpha = 0.22) +
    geom_line(data = final_forecast_tbl, aes(x = Month_Date, y = Forecast, color = "2025 forecast"), linewidth = 1.2) +
    geom_point(data = final_forecast_tbl, aes(x = Month_Date, y = Forecast, color = "2025 forecast"), size = 2) +
    geom_vline(xintercept = as.Date(future_start), color = "gray45", linewidth = 0.85, linetype = "dotted") +
    annotate(
      "text",
      x = as.Date(yearmonth("2024 Oct")),
      y = max(final_forecast_tbl$Upper_95, target_ts$Value, na.rm = TRUE),
      label = "Forecast starts\n2025 Jan",
      hjust = 1,
      vjust = 1,
      color = "gray35",
      size = 4.2
    ) +
    scale_color_manual(
      values = c("Actual" = "black", "Training fitted" = "#E69F00", "2025 forecast" = color),
      breaks = c("Actual", "Training fitted", "2025 forecast")
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(min(target_ts$Month_Date), as.Date(future_end)),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(labels = y_labeler) +
    labs(
      title = paste0(readable_target, " Forecast for 2025 using ", final_model_notation),
      subtitle = "Shaded bands show 80% and 95% forecast intervals",
      x = NULL,
      y = y_axis_title,
      color = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "gray35", size = 11.5),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
}
