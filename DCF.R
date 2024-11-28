# Clear the workspace
rm(list = ls())

# Set working directory
path <- ""
setwd(path)

# Set Finnhub API key
finhub_api_key <- ""

# Load required libraries
library(httr)
library(jsonlite)

# Function to fetch financials
fetch_financials <- function(ticker) {
  url <- paste0("https://finnhub.io/api/v1/stock/financials-reported?symbol=", ticker, "&freq=annual&token=", finhub_api_key)
  response <- GET(url)
  if (response$status_code == 200) {
    data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
    return(data)
  } else {
    stop("Failed to fetch data. HTTP Status Code: ", response$status_code)
  }
}

perform_dcf <- function(ticker, target_year, growth_rate, discount_rate, terminal_growth_rate, forecast_years = 5) {
  # Fetch data
  data <- fetch_financials(ticker)
  
  # Extract data
  balance_sheets <- data[["data"]][["report"]][["bs"]]
  income_statements <- data[["data"]][["report"]][["ic"]]
  cashflow_statements <- data[["data"]][["report"]][["cf"]]
  
  # Target and previous year indices
  year_index <- which(data[["data"]][["year"]] == target_year)
  prev_year_index <- which(data[["data"]][["year"]] == target_year - 1)
  
  # Balance Sheet, Income Statement, and Cash Flow Statement
  target_balance_sheet <- balance_sheets[[year_index]]
  prev_balance_sheet <- balance_sheets[[prev_year_index]]
  target_income_statement <- income_statements[[year_index]]
  target_cashflow_statement <- cashflow_statements[[year_index]]
  print(target_balance_sheet)
  print(prev_balance_sheet)
  print(target_income_statement)
  print(target_cashflow_statement)
  
  # Define Keywords for Matching
  keywords <- list(
    ebit = c("operating income","Operations"),
    taxes = c("provision for income taxes"),
    depreciation = c("depreciation and amortization"),
    current_assets = c("total current assets"),
    current_liabilities = c("total current liabilities"),
    shares_oustanding = c("common Stock, Shares Authorized"),
    ppe_net = c("Property, software, and equipment, net")
  )
  
  # Helper function to extract values using keywords
  extract_value <- function(data, label_keywords) {
    index <- grep(paste(label_keywords, collapse = "|"), data$label, ignore.case = TRUE)
    if (length(index) > 0) {
      paste(label_keywords)
      paste(data$value[index][1])
      return(data$value[index][1])  # Take the first match
    } else {
      stop(paste("Label not found for keywords:", paste(label_keywords, collapse = ", ")))
    }
  }
  
  # Extract Required Data
  ebit <- extract_value(target_income_statement, keywords$ebit)
  taxes <- extract_value(target_income_statement, keywords$taxes)
  depreciation <- extract_value(target_cashflow_statement, keywords$depreciation)
  target_current_assets <- extract_value(target_balance_sheet, keywords$current_assets)
  target_current_liabilities <- extract_value(target_balance_sheet, keywords$current_liabilities)
  prev_current_assets <- extract_value(prev_balance_sheet, keywords$current_assets)
  prev_current_liabilities <- extract_value(prev_balance_sheet, keywords$current_liabilities)
  target_ppe <- extract_value(target_balance_sheet, keywords$ppe_net)
  prev_ppe <- extract_value(prev_balance_sheet, keywords$ppe_net)
  shares_outstanding <- extract_value(prev_balance_sheet, keywords$shares_oustanding)
  
  # Calculate Operating Cash Flow (OCF)
  ocf <- ebit + depreciation - taxes
  
  # Calculate Net Capital Spending (NCS)
  ncs <- target_ppe - prev_ppe + depreciation
  
  # Calculate Changes in NWC
  target_nwc <- target_current_assets - target_current_liabilities
  prev_nwc <- prev_current_assets - prev_current_liabilities
  changes_in_nwc <- target_nwc - prev_nwc
  
  # Calculate Cash Flow from Assets (CFFA)
  cffa <- ocf - ncs - changes_in_nwc
  
  # Forecast Free Cash Flow (FCF)
  fcf_forecast <- numeric(forecast_years)
  fcf_forecast[1] <- cffa * (1 + growth_rate)
  for (i in 2:forecast_years) {
    fcf_forecast[i] <- fcf_forecast[i - 1] * (1 + growth_rate)
  }
  
  # Calculate Terminal Value
  terminal_value <- fcf_forecast[forecast_years] * (1 + terminal_growth_rate) / (discount_rate - terminal_growth_rate)
  
  # Discount FCF and Terminal Value
  discount_factors <- (1 + discount_rate)^(1:forecast_years)
  discounted_fcf <- fcf_forecast / discount_factors
  discounted_terminal_value <- terminal_value / (1 + discount_rate)^forecast_years
  
  # Calculate DCF Value
  dcf_value <- sum(discounted_fcf) + discounted_terminal_value
  
  # Calculate Intrinsic Value Per Share
  intrinsic_value <- dcf_value / shares_outstanding
  
  terminal_value_per_share <- terminal_value/shares_outstanding
  
  discounted_terminal_value_per_share <- discounted_terminal_value/shares_outstanding
  
  # Return Results
  return(list(
    cffa = cffa,
    dcf_value = dcf_value,
    intrinsic_value = intrinsic_value,
    fcf_forecast = fcf_forecast,
    terminal_value = terminal_value,
    discounted_fcf = discounted_fcf,
    discounted_terminal_value = discounted_terminal_value,
    shares_outstanding = shares_outstanding,
    terminal_value_per_share = terminal_value_per_share,
    discounted_terminal_value_per_share = discounted_terminal_value_per_share
  ))
}

# Example Execution
ticker <- "HOOD"
target_year <- 2023
growth_rate <- 0.10            
discount_rate <- 0.06           
terminal_growth_rate <- 0.05 
forecast_years <- 5

results <- perform_dcf(
  ticker = ticker,
  target_year = target_year,
  growth_rate = growth_rate,
  discount_rate = discount_rate,
  terminal_growth_rate = terminal_growth_rate,
  forecast_years = forecast_years
)

# Print Results
cat("--- ",ticker,"DCF",target_year,"Results ---\n")
cat("Cash Flow From Assets:",results$cffa, "\n")
cat("Shares Oustanding:", results$shares_outstanding, "\n")
cat("Projected Free Cash Flows:", results$fcf_forecast, "\n")
cat("Terminal Value:", results$terminal_value, "\n")
cat("Terminal Value Per Share:", results$terminal_value_per_share, "\n")
cat("Discounted Free Cash Flows:", results$discounted_fcf, "\n")
cat("Discounted Terminal Value:", results$discounted_terminal_value, "\n")
cat("Discounted Terminal Value Per Share:", results$discounted_terminal_value_per_share, "\n")
cat("DCF Value:", results$dcf_value, "\n")
cat("Intrinsic Value per Share:", results$intrinsic_value, "\n")
