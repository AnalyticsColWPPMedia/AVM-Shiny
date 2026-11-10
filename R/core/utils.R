#=================================================#
#### 1. Statistics (Model Performance Metrics) ####
#=================================================#

# Function to calculate R-Squared and Adjusted R-Squared
# R2 is computed as the squared correlation between observed and fitted values
# in the reported scale (levels). For multiplicative (log) models this corrects
# the R2 reported by AWB, which is computed in the transformed scale.
# Arguments:
#   y_obs:  Vector with observed values (Actual).
#   y_pred: Vector with predicted values (Model/Fitted).
#   k:      Number of model parameters (excluding the intercept/DepVar),
#           see n_model_params().
r2 <- function(y_obs, y_pred, k) {
  ok <- !is.na(y_obs) & !is.na(y_pred)
  y_obs <- y_obs[ok]
  y_pred <- y_pred[ok]
  n <- length(y_obs)

  # Correlation is undefined with fewer than 2 points or zero variance
  if (n < 2 || sd(y_obs) == 0 || sd(y_pred) == 0) {
    return(list(R2 = NA_real_, R2_Adj = NA_real_))
  }

  metric_r2 <- cor(y_obs, y_pred)^2

  # Strict validation:
  # 1. Insufficient degrees of freedom (n <= k + 1) -> NA
  # 2. Negative adjusted R2 -> NA (avoids visual confusion)
  if (n <= (k + 1)) {
    metric_r2_adj <- NA_real_
  } else {
    metric_r2_adj <- 1 - (((1 - metric_r2) * (n - 1)) / (n - k - 1))
    if (metric_r2_adj < 0) metric_r2_adj <- NA_real_
  }

  list(R2 = metric_r2, R2_Adj = metric_r2_adj)
}

# Function to calculate MAPE (Mean Absolute Percentage Error), in percent.
# Observations with Actual == 0 (or NA) are excluded to avoid Inf values.
calcMAPE <- function(y_obs, y_pred) {
  ok <- !is.na(y_obs) & !is.na(y_pred) & y_obs != 0
  if (!any(ok)) return(NA_real_)
  mean(abs((y_obs[ok] - y_pred[ok]) / y_obs[ok])) * 100
}

# Number of model parameters used for the Adjusted R2 degrees of freedom
# (all variables in Variables Statistics except the dependent variable).
n_model_params <- function(vstats) {
  length(unique(vstats$VariableName)) - 1
}

# Wrapper returning all fit metrics in a single call, so the same rules
# (screen, entity table, download) are applied everywhere.
# Returns a one-row tibble: R2, R2_Adj, MAPE (percent) and N (observations used).
fit_metrics <- function(y_obs, y_pred, k) {
  r2_res <- r2(y_obs, y_pred, k)
  tibble::tibble(
    R2     = r2_res$R2,
    R2_Adj = r2_res$R2_Adj,
    MAPE   = calcMAPE(y_obs, y_pred),
    N      = sum(!is.na(y_obs) & !is.na(y_pred))
  )
}

#=========================================#
#### 2. KPI Transformation & Aggregation ####
#=========================================#

# Function to bring the dependent variable and the model back to the
# reporting scale.
# Arguments:
#   data:          Model AVM dataframe (DepVar, Model, DepVarAvg columns).
#   transform:     "Log" (multiplicative model) or "Linear" (additive model).
#   normalisation: Normalisation of the dependent variable in Variables
#                  Statistics ("AROUND 0" = SUB, anything else = none/DIV).
# Returns the same dataframe with two new columns: Actual and Fitted.
transform_kpi <- function(data, transform = c("Log", "Linear"), normalisation = NA) {
  transform <- match.arg(transform)

  # SUB normalisation: the average was subtracted, so it is added back
  offset <- if (isTRUE(normalisation == "AROUND 0")) data$DepVarAvg else 0
  inv <- if (transform == "Log") exp else identity

  data$Actual <- inv(data$DepVar + offset)
  data$Fitted <- inv(data$Model + offset)
  data
}

# Function to aggregate Actual and Fitted by period (and optional dimensions).
# Periods with Actual == 0 are dropped: they are excluded from every chart,
# metric and download so all outputs are computed on the same observations.
aggregate_avm <- function(data, group_cols = character(0)) {
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "Period")))) %>%
    dplyr::summarise(Actual = sum(Actual, na.rm = TRUE),
                     Fitted = sum(Fitted, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::filter(Actual != 0) %>%
    dplyr::mutate(Residual = Actual - Fitted)
}

#=======================================#
#### 3. File Reading (I/O Utilities) ####
#=======================================#

# Function to unzip the AWB export into a unique folder and read the two
# required files. Using a unique folder per upload avoids reading stale files
# from a previous upload and removes any dependency on reactive evaluation order.
# Arguments:
#   zip_path: Path to the uploaded .zip file.
# Returns a list with: avm (Model AVM.csv) and var_stats (Variables Statistics.csv).
read_awb_zip <- function(zip_path) {
  exdir <- tempfile("awb_")
  dir.create(exdir)
  utils::unzip(zip_path, exdir = exdir)

  # Files are searched recursively in case the zip contains a parent folder
  all_files <- list.files(exdir, recursive = TRUE, full.names = TRUE)
  find_file <- function(name) {
    hit <- all_files[basename(all_files) == name]
    if (length(hit) == 0) stop(sprintf("'%s' was not found in the .zip file.", name), call. = FALSE)
    hit[1]
  }

  list(
    avm = readr::read_csv(find_file("Model AVM.csv"), show_col_types = FALSE) %>%
      dplyr::filter(!is.na(Model)),
    var_stats = readr::read_csv(find_file("Variables Statistics.csv"), show_col_types = FALSE)
  )
}

# Function to read all sheets from an Excel file into a list
# Arguments:
#   filename: Path to the Excel file.
#   tibble: Logical. If FALSE, converts results to standard data.frames.
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # Get sheet names
  sheets <- readxl::excel_sheets(filename)

  # Iterate and read each sheet
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))

  # Convert to data.frame if tibble is not required
  if(!tibble) x <- lapply(x, as.data.frame)

  # Assign names to list elements and return
  names(x) <- sheets
  x
}

#=======================================#
#### 4. Formatting & Plot Helpers    ####
#=======================================#

# Formats a proportion-like value (0-1) as a percentage string, "N/A" if missing
fmt_pct <- function(x, digits = 2) {
  ifelse(is.na(x), "N/A", paste0(formatC(x * 100, format = "f", digits = digits), "%"))
}

# Returns n distinct colors. Uses the team palette and interpolates it when
# more colors than available are required.
get_palette <- function(n) {
  base_palette <- c("#2c3e50", "#c0392b", "#7f8c8d", "#27ae60", "#8e44ad",
                    "#2980b9", "#d35400", "#16a085", "#34495e", "#95a5a6")
  if (n <= length(base_palette)) return(base_palette[seq_len(n)])
  grDevices::colorRampPalette(base_palette)(n)
}
