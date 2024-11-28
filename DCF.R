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

perform_dcf <- function(ticker, target_year, discount_rate, terminal_growth_rate, forecast_years = 5, dividend_payout_ratio = 0.0) {
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
  print(target_balance_sheet)
  prev_balance_sheet <- balance_sheets[[prev_year_index]]
  print(prev_balance_sheet)
  target_income_statement <- income_statements[[year_index]]
  print(target_income_statement)
  target_cashflow_statement <- cashflow_statements[[year_index]]
  print(target_cashflow_statement)
  
  # Define Keywords for Matching
  keywords <- list(
    ebit = c("operations"),
    taxes = c("provision for income taxes"),
    depreciation = c("depreciation and amortization"),
    current_assets = c("total current assets"),
    current_liabilities = c("total current liabilities"),
    equity = c("total stockholders’ equity"),
    net_income = c("comprehensive loss"),
    total_assets = c("total assets"),
    shares_outstanding = c("(in shares)"),
    ppe= c("property", "equipment"),
    cash = c("cash and cash equivalents")
  )
  
  # Helper function to extract values using keywords
  extract_value <- function(data, label_keywords) {
    index <- grep(paste(label_keywords, collapse = "|"), data$label, ignore.case = TRUE)
    if (length(index) > 0) {
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
  total_equity <- extract_value(target_balance_sheet, keywords$equity)
  total_assets <- extract_value(target_balance_sheet, keywords$total_assets)
  net_income <- extract_value(target_income_statement, keywords$net_income)
  shares_outstanding <- extract_value(target_income_statement, keywords$shares_outstanding)
  target_ppe <- extract_value(target_balance_sheet, keywords$ppe)
  prev_ppe <- extract_value(prev_balance_sheet, keywords$ppe)
  traget_cash <- extract_value(target_balance_sheet, keywords$cash)
  prev_cash <- extract_value(target_balance_sheet, keywords$cash)
  
  # Calculate Operating Cash Flow (OCF)
  ocf <- ebit + depreciation - taxes
  
  # Calculate Net Capital Spending (NCS)
  ncs <- target_ppe - prev_ppe + depreciation
  
  # Calculate Changes in NWC
  target_nwc <- target_current_assets - target_current_liabilities - traget_cash
  prev_nwc <- prev_current_assets - prev_current_liabilities - prev_cash
  changes_in_nwc <- target_nwc - prev_nwc
  
  # Calculate Cash Flow from Assets (CFFA)
  cffa <- ocf - ncs - changes_in_nwc
  
  # Calculate Sustainable Growth Rate (SGR)
  roe <- net_income / total_equity
  sgr <- roe * (1 - dividend_payout_ratio)
  
  # Forecast Free Cash Flow (FCF) using SGR
  fcf_forecast <- numeric(forecast_years)
  
  for (i in 1:forecast_years) {
    fcf_forecast[i] <- cffa * (1 + sgr)^i
  }
  
  # Calculate Terminal Value
  terminal_value <- fcf_forecast[forecast_years] * (1 + terminal_growth_rate) / (discount_rate - terminal_growth_rate)
  
  # Discount FCF and Terminal Value
  discount_factors <- (1 + discount_rate)^(1:forecast_years)
  discounted_fcf <- fcf_forecast / discount_factors
  discounted_terminal_value <- terminal_value / (1 + discount_rate)^forecast_years

  # Calculate DCF Value
  dcf_value <- sum(discounted_fcf) + discounted_terminal_value
  
  # Calculate Equity Value
  equity_ratio <- total_equity / total_assets
  equity_value <- dcf_value * equity_ratio
  
  # Calculate Intrinsic Value Per Share
  intrinsic_value <- equity_value / shares_outstanding
  
  # Return Results
  return(list(
    ocf=ocf,
    ncs=ncs,
    changes_in_nwc=changes_in_nwc,
    cffa = cffa,
    sgr = sgr,
    dcf_value = dcf_value,
    equity_value = equity_value,
    intrinsic_value = intrinsic_value,
    fcf_forecast = fcf_forecast,
    terminal_value = terminal_value,
    discounted_fcf = discounted_fcf,
    discounted_terminal_value = discounted_terminal_value,
    shares_outstanding = shares_outstanding
  ))
}

# Example Execution
ticker <- "HOOD"
target_year <- 2023
discount_rate <- 0.06            # Assumed discount rate us WACC
terminal_growth_rate <- 0.05     # Assumed terminal growth rate (e.g., 10-year bond rate)
forecast_years <- 2
dividend_payout_ratio <- 0.0

results <- perform_dcf(
  ticker = ticker,
  target_year = target_year,
  discount_rate = discount_rate,
  terminal_growth_rate = terminal_growth_rate,
  forecast_years = forecast_years,
  dividend_payout_ratio = dividend_payout_ratio
)

# Print Results
cat("--- ", ticker, "DCF", target_year, "Results ---\n")
cat("Operating Cash Flow:",results$ocf, "\n")
cat("Capex:",results$ncs, "\n")
cat("Changes in NWC:",results$changes_in_nwc, "\n")
cat("Cash Flow From Assets (CFFA):", results$cffa, "\n")
cat("Sustainable Growth Rate (SGR):", results$sgr, "\n")
cat("Shares Outstanding:", results$shares_outstanding, "\n")
cat("Projected Free Cash Flows:\n", paste(results$fcf_forecast, collapse = ", "), "\n")
cat("Terminal Value:", results$terminal_value, "\n")
cat("Discounted Free Cash Flows:\n", paste(results$discounted_fcf, collapse = ", "), "\n")
cat("Discounted Terminal Value:", results$discounted_terminal_value, "\n")
cat("Total DCF Value:", results$dcf_value, "\n")
cat("Equity Value:", results$equity_value, "\n")
cat("Intrinsic Value per Share:", results$intrinsic_value, "\n")
