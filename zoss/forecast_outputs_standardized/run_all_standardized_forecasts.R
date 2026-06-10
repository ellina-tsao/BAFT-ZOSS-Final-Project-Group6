# ============================================================
# ZOSS Forecasting Reproducibility Runner
# ============================================================
# Run this script to regenerate the standardized forecasting outputs used in
# the final report and presentation.

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

standardized_root <- script_dir
zoss_root <- dirname(standardized_root)

scripts_to_run <- c(
  file.path(standardized_root, "aov", "zoss_aov_forecast_standardized.R"),
  file.path(standardized_root, "new_customer_volume", "zoss_new_customer_volume_forecast_standardized.R"),
  file.path(standardized_root, "returning_customer_volume", "zoss_returning_customer_volume_forecast_standardized.R"),
  file.path(zoss_root, "zoss_retail_revenue_share_calendar_adjusted.R")
)

missing_scripts <- scripts_to_run[!file.exists(scripts_to_run)]
if (length(missing_scripts) > 0) {
  stop("Missing scripts:\n", paste(missing_scripts, collapse = "\n"))
}

for (script_path in scripts_to_run) {
  message("\n============================================================")
  message("Running: ", script_path)
  message("============================================================")

  status <- system2("Rscript", shQuote(script_path))
  if (!identical(status, 0L)) {
    stop("Script failed: ", script_path)
  }
}

message("\nAll standardized ZOSS forecasting scripts completed.")
