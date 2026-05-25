# =============================================================================
# Imputation of Missing Country Data — 53-Country Approach
# AgIncentives Database 2005–2024
# =============================================================================
#
# PURPOSE
# -------
# The AgIncentives database coverage declines in recent years due to data
# submission lags:
#   2020: 53 countries (baseline — all present)
#   2021: 50 countries (3 missing: BEN, ZMB, ZWE)
#   2022: 46 countries (7 missing)
#   2023: 35 countries (18 missing)
#   2024: 29 countries (24 missing)
#
# This script imputes missing country-years for 2021–2024 to maintain the
# full 53-country set present in 2020 throughout the analysis period.
#
# COUNTRY SELECTION
# -----------------
# The 53-country set is defined as all countries present in 2020 in the
# CountryXSector (crop/livestock) or CountryXAllAg (total agriculture) rows.
# Countries with a full 2005–2024 time series (29 countries) are never imputed.
# Only country-years missing from the reported data receive imputed values.
#
# IMPUTATION METHOD
# -----------------
# Components are imputed separately, reflecting the different policy drivers:
#
#   NRP (price support):
#     Carry-forward the last known year value.
#     Rationale: Price support is determined by trade policy (tariffs, import
#     quotas, export taxes) which changes infrequently and through formal
#     legislative processes. Extrapolating NRP using a trend is unjustified
#     without evidence of policy change.
#
#   Subsidies (Inputs, Outputs, Others):
#     OLS linear trend fitted on FIT_START to last known year.
#     Fallback: 3-year average if coefficient of variation > CV_THRESHOLD (1.0),
#     indicating a volatile series where trend extrapolation is unreliable.
#     Fallback to carry-forward if fewer than MIN_OBS (5) observations.
#     Rationale: Subsidies are budget-driven and vary with fiscal cycles and
#     policy decisions; a trend-based approach is more appropriate than
#     assuming no change.
#
# OUTPUTS
# -------
# For each sector (crop, livestock, total agriculture):
#   - Full 53-country dataset (reported + imputed), 2005–2024
#   - Imputation log: country, year, component, method, imputed value
#   - Summary statistics: coverage by year before and after imputation
# =============================================================================

library(tidyverse)

# ── PARAMETERS ────────────────────────────────────────────────────────────────
FIT_START    <- 2015   # start year for trend fitting window
CV_THRESHOLD <- 1.0    # CV above which 3-year average replaces trend
MIN_OBS      <- 5      # minimum observations required for trend fitting
IMP_YEARS    <- 2021:2024   # years to impute (2020 is fully reported)

# ── LOAD DATA ─────────────────────────────────────────────────────────────────
crop <- read_csv("Support_Crop_2026.csv") %>%
  mutate(Support_USD = as.numeric(Support_USD),
         RVP         = as.numeric(RelevantValueProduction))

lvst <- read_csv("Support_livestock_2026.csv") %>%
  mutate(Support_USD = as.numeric(Support_USD),
         RVP         = as.numeric(RelevantValueProduction))

totl <- read_csv("Support_total_2026.csv") %>%
  mutate(Support_USD = as.numeric(Support_USD),
         RVP         = as.numeric(RelevantValueProduction))

# ── IDENTIFY 53-COUNTRY SETS ──────────────────────────────────────────────────
# One set per sector (crop/livestock may differ slightly from total agriculture)

c53_crop <- crop %>%
  filter(AGGREGATION == "CountryXSector", Year == 2020) %>%
  pull(Country_Code) %>% unique() %>% sort()

c53_lvst <- lvst %>%
  filter(AGGREGATION == "CountryXSector", Year == 2020) %>%
  pull(Country_Code) %>% unique() %>% sort()

c53_totl <- totl %>%
  filter(AGGREGATION == "CountryXAllAg", Year == 2020) %>%
  pull(Country_Code) %>% unique() %>% sort()

message("53-country sets: crop=", length(c53_crop),
        " | lvst=", length(c53_lvst),
        " | totl=", length(c53_totl))

# ── COVERAGE DIAGNOSTICS ──────────────────────────────────────────────────────
coverage_summary <- function(df, c53, agg_val) {
  map_dfr(2020:2024, function(yr) {
    reported <- df %>%
      filter(AGGREGATION == agg_val, Year == yr,
             Country_Code %in% c53) %>%
      pull(Country_Code) %>% unique()
    missing <- setdiff(c53, reported)
    tibble(Year         = yr,
           N_reported   = length(reported),
           N_missing    = length(missing),
           Missing      = paste(sort(missing), collapse = ", "))
  })
}

message("\nCrop sector coverage (2020–2024):")
print(coverage_summary(crop, c53_crop, "CountryXSector"))

message("\nTotal agriculture coverage (2020–2024):")
print(coverage_summary(totl, c53_totl, "CountryXAllAg"))

# ── IMPUTATION FUNCTION ───────────────────────────────────────────────────────

#' Impute Support_USD for one NRA_Cat component, one country, one year.
#'
#' @param series_df  Data frame: historical rows for this country x NRA_Cat.
#'                   Must contain columns Year, Support_USD, RVP.
#' @param cat        NRA_Cat value ("NRP", "Inputs", "Outputs", "Others")
#' @param target_yr  Year to impute
#' @param fit_start  Start of trend-fitting window (default FIT_START)
#' @param cv_thr     CV threshold for 3-year average fallback (default CV_THRESHOLD)
#' @param min_obs    Minimum observations for trend fitting (default MIN_OBS)
#'
#' @return tibble with columns: Support_USD (imputed), RVP, Method
impute_component <- function(series_df, cat, target_yr,
                             fit_start = FIT_START,
                             cv_thr    = CV_THRESHOLD,
                             min_obs   = MIN_OBS) {

  s        <- series_df %>% arrange(Year)
  last_usd <- if (nrow(s) > 0) tail(s$Support_USD, 1) else 0
  last_rvp <- if (nrow(s) > 0) tail(s$RVP, 1)         else NA_real_

  # ── NRP: carry-forward last known (trade policy sticky) ──
  if (cat == "NRP") {
    return(tibble(Support_USD = last_usd,
                  RVP         = last_rvp,
                  Method      = "carry_forward"))
  }

  # ── Subsidies: trend or fallback ──
  hist <- s %>% filter(Year >= fit_start)
  n    <- nrow(hist)

  if (n >= min_obs) {
    y  <- hist$Support_USD
    cv <- if (abs(mean(y)) > 0) sd(y) / abs(mean(y)) else Inf

    if (cv > cv_thr) {
      # Volatile series: 3-year average
      imp_usd <- mean(tail(y, 3))
      method  <- paste0("3yr_avg (CV=", round(cv, 2), ")")
    } else {
      # OLS linear trend
      fit     <- lm(Support_USD ~ Year, data = hist)
      imp_usd <- predict(fit, newdata = tibble(Year = target_yr))
      method  <- paste0("linear_trend (slope=",
                        round(coef(fit)["Year"] / 1e9, 4), "B/yr)")
    }
  } else {
    # Insufficient history: carry-forward
    imp_usd <- last_usd
    method  <- paste0("carry_forward (n=", n, ")")
  }

  tibble(Support_USD = imp_usd, RVP = last_rvp, Method = method)
}

# ── BUILD 53-COUNTRY DATASET ──────────────────────────────────────────────────

#' Build full 53-country dataset for one sector with imputations.
#'
#' @param df       Source data frame (crop, lvst, or totl)
#' @param c53      Character vector: 53-country set
#' @param agg_val  AGGREGATION value ("CountryXSector" or "CountryXAllAg")
#'
#' @return list with:
#'   $data    Full dataset (reported + imputed rows)
#'   $log     Imputation log (one row per imputed country-year-component)
build_53c <- function(df, c53, agg_val) {

  cats <- c("NRP", "Inputs", "Outputs", "Others")

  # Reported data (all years, 53-country set only)
  rep <- df %>%
    filter(AGGREGATION == agg_val, Country_Code %in% c53) %>%
    select(Country_Code, Year, NRA_Cat, Support_USD, RVP,
           any_of(c("LISTNAME_EN","REGIONNAME","WBNAME2015",
                    "CATPROD","Country_ISO3")))

  # Identify and impute missing country-years
  imp_data <- map_dfr(c53, function(code) {
    sub      <- rep %>% filter(Country_Code == code)
    rep_yrs  <- unique(sub$Year)
    miss_yrs <- setdiff(IMP_YEARS, rep_yrs)

    if (length(miss_yrs) == 0) return(NULL)

    map_dfr(miss_yrs, function(yr) {
      map_dfr(cats, function(cat) {
        series <- sub %>% filter(NRA_Cat == cat)
        result <- impute_component(series, cat, yr)
        tibble(Country_Code = code,
               Year         = yr,
               NRA_Cat      = cat,
               Support_USD  = result$Support_USD,
               RVP          = result$RVP,
               Method       = result$Method,
               Imputed      = TRUE)
      })
    })
  })

  # Imputation log (keep method column)
  imp_log <- imp_data %>%
    select(Country_Code, Year, NRA_Cat, Support_USD, RVP, Method) %>%
    mutate(Support_USD_bn = round(Support_USD / 1e9, 3)) %>%
    select(Country_Code, Year, NRA_Cat, Support_USD_bn, Method)

  # Drop Method/Imputed from data before binding
  imp_rows <- imp_data %>% select(-Method, -Imputed)

  # Combine and tag
  combined <- bind_rows(
    rep     %>% mutate(Imputed = FALSE),
    imp_rows %>% mutate(Imputed = TRUE)
  ) %>%
    arrange(Country_Code, Year, NRA_Cat)

  list(data = combined, log = imp_log)
}

# ── RUN IMPUTATION ────────────────────────────────────────────────────────────
message("\nRunning imputation for crop sector...")
crop_result <- build_53c(crop, c53_crop, "CountryXSector")
crop53      <- crop_result$data
crop_log    <- crop_result$log

message("Running imputation for livestock sector...")
lvst_result <- build_53c(lvst, c53_lvst, "CountryXSector")
lvst53      <- lvst_result$data
lvst_log    <- lvst_result$log

message("Running imputation for total agriculture...")
totl_result <- build_53c(totl, c53_totl, "CountryXAllAg")
totl53      <- totl_result$data
totl_log    <- totl_result$log

# ── IMPUTATION SUMMARY ────────────────────────────────────────────────────────
message("\n=== CROP IMPUTATION LOG ===")
print(
  crop_log %>%
    filter(NRA_Cat == "NRP") %>%  # one row per country-year for readability
    select(Country_Code, Year, Support_USD_bn, Method) %>%
    rename(Total_NRP_bn = Support_USD_bn) %>%
    arrange(Year, Country_Code),
  n = 50
)

message("\n=== TOTAL AGRI IMPUTATION LOG ===")
print(
  totl_log %>%
    group_by(Country_Code, Year) %>%
    summarise(Total_USD_bn = sum(Support_USD_bn), .groups = "drop") %>%
    arrange(Year, Country_Code),
  n = 50
)

# ── COVERAGE AFTER IMPUTATION ─────────────────────────────────────────────────
message("\n=== COVERAGE AFTER IMPUTATION ===")
coverage_after <- function(dat53, c53) {
  map_dfr(2020:2024, function(yr) {
    n_rep <- dat53 %>%
      filter(Year == yr, !Imputed, Country_Code %in% c53) %>%
      pull(Country_Code) %>% unique() %>% length()
    n_imp <- dat53 %>%
      filter(Year == yr,  Imputed, Country_Code %in% c53) %>%
      pull(Country_Code) %>% unique() %>% length()
    tibble(Year = yr, Reported = n_rep, Imputed = n_imp, Total = n_rep + n_imp)
  })
}

print(coverage_after(crop53, c53_crop))

# ── GLOBAL AGGREGATES CHECK ───────────────────────────────────────────────────
message("\n=== GLOBAL TOTALS CHECK (total agri, 53-country) ===")
global_check <- totl53 %>%
  filter(Year >= 2020, NRA_Cat != "NRA_Total") %>%
  group_by(Year) %>%
  summarise(
    Total_USD_bn = sum(Support_USD, na.rm = TRUE) / 1e9,
    .groups = "drop"
  ) %>%
  left_join(
    totl53 %>%
      filter(Year >= 2020, NRA_Cat == "NRP") %>%
      group_by(Year) %>%
      summarise(Total_RVP = sum(RVP, na.rm = TRUE), .groups = "drop"),
    by = "Year"
  ) %>%
  mutate(NRA_pct = round(Total_USD_bn * 1e9 / Total_RVP * 100, 2),
         Total_USD_bn = round(Total_USD_bn, 1))

print(global_check)

# ── SAVE OUTPUTS ──────────────────────────────────────────────────────────────
dir.create("imputation_output", showWarnings = FALSE)

write_csv(crop53,    "imputation_output/crop53_imputed.csv")
write_csv(lvst53,    "imputation_output/lvst53_imputed.csv")
write_csv(totl53,    "imputation_output/totl53_imputed.csv")
write_csv(crop_log,  "imputation_output/crop_imputation_log.csv")
write_csv(lvst_log,  "imputation_output/lvst_imputation_log.csv")
write_csv(totl_log,  "imputation_output/totl_imputation_log.csv")

message("\nOutputs saved to imputation_output/:")
message("  crop53_imputed.csv       — Full 53-country crop dataset (reported + imputed)")
message("  lvst53_imputed.csv       — Full 53-country livestock dataset")
message("  totl53_imputed.csv       — Full 53-country total agriculture dataset")
message("  crop_imputation_log.csv  — Imputed values and methods, crop sector")
message("  lvst_imputation_log.csv  — Imputed values and methods, livestock sector")
message("  totl_imputation_log.csv  — Imputed values and methods, total agriculture")
