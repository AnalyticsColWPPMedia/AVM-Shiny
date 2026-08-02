#=================================================#
#### 1. Statistics (Model Performance Metrics) ####
#=================================================#

# Function to calculate R-Squared and Adjusted R-Squared
# Arguments:
#   y_obs: Vector with observed values (Actual).
#   y_pred: Vector with predicted values (Model/Fitted).
#   full_avm: Object used to determine sample size 'n'.
#   vstatsfile: Dataframe of statistics used to determine degrees of freedom 'k'.
r2 <- function(y_obs, y_pred, full_avm, vstatsfile) {
  # Cálculo del R2 estándar
  metric_r2 <- cor(y_obs, y_pred)^2
  
  k <- length(unique(vstatsfile$VariableName)) - 1
  n <- length(full_avm)
  
  # Validación estricta:
  # 1. Si los grados de libertad son insuficientes (n <= k+1) -> NA
  # 2. Si el R2 Ajustado resulta negativo -> NA (para evitar confusión visual)
  
  if (n <= (k + 1)) {
    metric_r2_adj <- NA
  } else {
    calc <- 1 - (((1 - metric_r2) * (n - 1)) / (n - k - 1))
    # Si el resultado es negativo, lo forzamos a NA
    if (!is.na(calc) && calc < 0) {
      metric_r2_adj <- NA
      # metric_r2_adj <- 'Very few degrees of freedom'
    } else {
      metric_r2_adj <- calc
    }
  }
  
  return(list(R2 = metric_r2, R2_Adj = metric_r2_adj))
}


# Function to calculate MAPE (Mean Absolute Percentage Error)
calcMAPE <- function(y_obs, y_pred) {
  # Average of the absolute percentage difference between actual and predicted
  result <- mean(abs((y_obs - y_pred) / y_obs)) * 100
  return(result)
}

# Helper function to count valid observations
# Counts the number of rows where the 'Fitted' column is not null
calcnobs <- function(data) {
  result <- sum(!is.na(data$Fitted))
  return(result)
}

#=======================================#
#### 2. File Reading (I/O Utilities) ####
#=======================================#

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