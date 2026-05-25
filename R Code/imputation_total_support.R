# =============================================================================
# Imputation of Missing Country Data — AgIncentives Support_total_2026
# =============================================================================
#
# PURPOSE
# -------
# The AgIncentives database coverage declines in recent years due to data
# submission lags: 53 countries in 2020, 46 in 2022, 35 in 2023, 29 in 2024.
# This script imputes missing values for 2023 and 2024 to restore the
# 46-country baseline from 2022.
#
# APPROACH
# --------
# Current implementation (proportional share):
#   - Imputes NRA_Total (sum of all components) using country-specific linear
#     trend or 3-year average fallback
#   - Distributes imputed total across NRA_Cat components (NRP, Inputs,
#     Outputs, Others) using each country's last known year proportional share
#
# NOTE ON METHODOLOGY
# -------------------
# A more defensible approach would treat NRP (price support) and subsidies
# separately, since:
#   - NRP is determined by trade policy (tariffs, quotas, export taxes) which
#     tends to be sticky and changes infrequently
#   - Subsidies (Inputs, Outputs, Others) are budget-driven and vary with
#     fiscal cycles and policy decisions
# A refined approach (hold NRP at last known year; trend subsidies separately)
# is outlined at the end of this script as an extension.
#
# INPUTS
#   Support_total_2026.csv   — AgIncentives total agriculture database
#   VoP_FAOSTAT.csv          — FAOSTAT global value of production (for coverage)
#
# OUTPUT
#   imputed_support_46.csv   — Full 46-country dataset (reported + imputed)
#   imputation_summary.csv   — Summary of imputed values by country and year
#   coverage_comparison.csv  — RVP coverage vs FAOSTAT by approach and year
# =============================================================================

library(tidyverse)

# -----------------------------------------------------------------------------
# 0. PARAMETERS
# -----------------------------------------------------------------------------

FIT_START    <- 2015    # Start year for trend fitting
BASELINE_YR  <- 2022    # Year defining the 46-country baseline set
TARGET_YRS   <- c(2023, 2024)   # Years to impute
CV_THRESHOLD <- 1.0     # CV above which 3-year average is used instead of trend

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

totl <- read_csv("Support_total_2026.csv") %>%
  mutate(
    Support_USD = as.numeric(Support_USD),
    RVP         = as.numeric(RelevantValueProduction)
  )

fao <- read_csv("VoP_FAOSTAT.csv") %>%
  mutate(VoP = as.numeric(gsub(",", "", ValueOfProduction))) %>%
  select(Year, VoP)

# Country-level rows only (NRA_Total for RVP; all NRA_Cat for support)
cty_all <- totl %>%
  filter(AGGREGATION == "CountryXAllAg", NRA_Cat != "NRA_Total")

cty_rvp <- totl %>%
  filter(AGGREGATION == "CountryXAllAg", NRA_Cat == "NRA_Total")

# -----------------------------------------------------------------------------
# 2. IDENTIFY 46-COUNTRY BASELINE AND MISSING COUNTRIES
# -----------------------------------------------------------------------------

# Countries present in baseline year
countries_46 <- cty_rvp %>%
  filter(Year == BASELINE_YR) %>%
  pull(Country_Code) %>%
  unique()

cat(sprintf("46-country baseline set: %d countries from %d\n",
            length(countries_46), BASELINE_YR))

# Countries reported in each target year
reported_by_yr <- map(TARGET_YRS, ~{
  cty_rvp %>% filter(Year == .x) %>% pull(Country_Code) %>% unique()
}) %>% set_names(TARGET_YRS)

# Missing countries per target year
missing_by_yr <- map(TARGET_YRS, ~{
  setdiff(countries_46, reported_by_yr[[as.character(.x)]])
}) %>% set_names(TARGET_YRS)

cat("Missing countries:\n")
walk(TARGET_YRS, ~cat(sprintf("  %d: %d missing — %s\n",
  .x, length(missing_by_yr[[as.character(.x)]]),
  paste(sort(missing_by_yr[[as.character(.x)]]), collapse = ", "))))

# -----------------------------------------------------------------------------
# 3. IMPUTATION FUNCTION — TOTAL SUPPORT
# -----------------------------------------------------------------------------
# For each missing country:
#   Step 1: Compute annual total support from FIT_START to last known year
#   Step 2: If CV <= threshold, fit OLS linear trend and predict
#           If CV > threshold (volatile), use 3-year average of last known values
#   Step 3: Store imputed total and method used

impute_total <- function(code, target_year, fit_start = FIT_START,
                         cv_threshold = CV_THRESHOLD) {

  # Annual total support (sum across all NRA_Cat) from fit_start onward
  annual <- cty_all %>%
    filter(Country_Code == code, Year >= fit_start) %>%
    group_by(Year) %>%
    summarise(Total_USD = sum(Support_USD, na.rm = TRUE), .groups = "drop") %>%
    arrange(Year)

  last_yr  <- max(annual$Year)
  last_val <- annual %>% filter(Year == last_yr) %>% pull(Total_USD)

  # Need at least 3 observations to attempt trend
  if (nrow(annual) < 3) {
    return(tibble(
      Country_Code = code,
      Year         = target_year,
      Imputed_USD  = last_val,
      Method       = "carry_forward",
      Last_known_yr = last_yr,
      Last_known_USD = last_val,
      N_obs        = nrow(annual),
      CV           = NA_real_,
      Slope        = NA_real_
    ))
  }

  # Coefficient of variation (absolute mean to handle negative series)
  cv <- sd(annual$Total_USD) / abs(mean(annual$Total_USD))

  if (cv > cv_threshold || nrow(annual) < 5) {
    # Volatile or insufficient data: use 3-year average
    avg3      <- mean(tail(annual$Total_USD, 3))
    method    <- if (cv > cv_threshold) "3yr_avg (volatile)" else "3yr_avg (few obs)"
    imputed   <- avg3
    slope_val <- NA_real_
  } else {
    # OLS linear trend: Total_USD ~ Year
    fit       <- lm(Total_USD ~ Year, data = annual)
    imputed   <- predict(fit, newdata = tibble(Year = target_year))
    slope_val <- coef(fit)[["Year"]]
    method    <- "linear_trend"
  }

  tibble(
    Country_Code   = code,
    Year           = target_year,
    Imputed_USD    = imputed,
    Method         = method,
    Last_known_yr  = last_yr,
    Last_known_USD = last_val,
    N_obs          = nrow(annual),
    CV             = round(cv, 3),
    Slope          = round(slope_val / 1e9, 4)   # in USD billions per year
  )
}

# Run imputation for all missing country-years
imputation_summary <- map_dfr(TARGET_YRS, function(yr) {
  map_dfr(missing_by_yr[[as.character(yr)]], ~impute_total(.x, yr))
})

cat("\nImputation summary:\n")
print(imputation_summary %>%
  select(Country_Code, Year, Last_known_yr, Last_known_USD, Imputed_USD, Method, CV) %>%
  mutate(across(c(Last_known_USD, Imputed_USD), ~round(./1e9, 2)))
)

# -----------------------------------------------------------------------------
# 4. DISTRIBUTE IMPUTED TOTAL ACROSS NRA_CAT COMPONENTS
# -----------------------------------------------------------------------------
# Uses each country's last known year proportional share of NRA_Cat in total
# NOTE: This holds composition fixed. A refined approach would hold NRP at
# last known level and trend subsidies separately — see Section 7 below.

get_nra_cat_shares <- function(code) {
  sub <- cty_all %>%
    filter(Country_Code == code) %>%
    arrange(Year)
  last_yr <- max(sub$Year)
  sub_last <- sub %>% filter(Year == last_yr)
  total    <- sum(sub_last$Support_USD, na.rm = TRUE)
  if (total == 0 || is.na(total)) {
    # Equal split if total is zero
    return(tibble(NRA_Cat = c("NRP","Inputs","Outputs","Others"),
                  Share   = rep(0.25, 4)))
  }
  sub_last %>%
    group_by(NRA_Cat) %>%
    summarise(Share = sum(Support_USD, na.rm = TRUE) / total, .groups = "drop")
}

# Build full imputed rows with NRA_Cat breakdown
imputed_rows <- map_dfr(seq_len(nrow(imputation_summary)), function(i) {
  row    <- imputation_summary[i, ]
  code   <- row$Country_Code
  yr     <- row$Year
  total  <- row$Imputed_USD

  # NRA_Cat shares from last known year
  shares <- get_nra_cat_shares(code)

  # RVP: use last known year (NRA_Total row)
  rvp_val <- cty_rvp %>%
    filter(Country_Code == code) %>%
    slice_max(Year, n = 1) %>%
    pull(RVP)
  rvp_val <- if (length(rvp_val) == 0) NA_real_ else rvp_val[1]

  # Country metadata from last known year
  meta <- cty_all %>%
    filter(Country_Code == code) %>%
    slice_max(Year, n = 1) %>%
    slice(1) %>%
    select(Country_Code, LISTNAME_EN, REGIONNAME, WBNAME2015)

  shares %>%
    mutate(
      Year        = yr,
      Support_USD = total * Share,
      RVP         = rvp_val / 4,   # RVP divided equally across 4 NRA_Cat rows
      AGGREGATION = "CountryXAllAg",
      CATPROD     = "TOTAL",
      imputed     = TRUE
    ) %>%
    bind_cols(meta %>% select(-Country_Code)) %>%
    mutate(Country_Code = code) %>%
    select(Year, Country_Code, LISTNAME_EN, REGIONNAME, WBNAME2015,
           NRA_Cat, CATPROD, Support_USD, RVP, AGGREGATION, imputed)
})

# -----------------------------------------------------------------------------
# 5. COMBINE REPORTED AND IMPUTED DATA
# -----------------------------------------------------------------------------

reported_rows <- cty_all %>%
  filter(Country_Code %in% countries_46) %>%
  mutate(imputed = FALSE) %>%
  select(Year, Country_Code, LISTNAME_EN, REGIONNAME, WBNAME2015,
         NRA_Cat, CATPROD, Support_USD, RVP, AGGREGATION, imputed)

combined_46 <- bind_rows(reported_rows, imputed_rows) %>%
  arrange(Country_Code, Year, NRA_Cat)

cat(sprintf("\nCombined dataset: %d rows (%d reported + %d imputed)\n",
            nrow(combined_46),
            nrow(reported_rows),
            nrow(imputed_rows)))

# -----------------------------------------------------------------------------
# 6. COVERAGE ANALYSIS AGAINST FAOSTAT
# -----------------------------------------------------------------------------

# RVP for each approach (using NRA_Total rows, which carry the correct RVP)
compute_rvp_coverage <- function(codes, yr, label) {

  # Reported RVP
  rep_rvp <- cty_rvp %>%
    filter(Country_Code %in% codes, Year == yr) %>%
    pull(RVP) %>% sum(na.rm = TRUE)

  # Imputed RVP: last known year for missing countries
  missing <- setdiff(codes, cty_rvp %>% filter(Year == yr) %>% pull(Country_Code))
  imp_rvp <- map_dbl(missing, ~{
    cty_rvp %>%
      filter(Country_Code == .x) %>%
      slice_max(Year, n = 1) %>%
      pull(RVP) %>%
      { if (length(.) == 0) 0 else .[1] }
  }) %>% sum()

  tibble(Approach = label, Year = yr,
         RVP_T = (rep_rvp + imp_rvp) / 1e12)
}

# Full DB RVP
full_rvp <- totl %>%
  filter(AGGREGATION == "WorldxAllAg", NRA_Cat == "NRA_Total") %>%
  group_by(Year) %>%
  summarise(RVP_T = sum(RVP, na.rm = TRUE) / 1e12, .groups = "drop") %>%
  mutate(Approach = "Full database (WorldxAllAg)")

# 29 consistent countries
CORE29 <- c("ARG","AUS","BRA","CAN","CHE","CHL","CHN","COL","CRI","ETH","EUR",
            "GBR","IDN","IND","ISL","ISR","JPN","KAZ","KOR","MEX","NOR","NZL",
            "PHL","RUS","TUR","UKR","USA","VNM","ZAF")

coverage_df <- bind_rows(
  full_rvp,
  map_dfr(2005:2024, ~compute_rvp_coverage(countries_46, .x, "Imputed 46 (main)")),
  map_dfr(2005:2024, ~{
    rvp29 <- cty_rvp %>%
      filter(Country_Code %in% CORE29, Year == .x) %>%
      pull(RVP) %>% sum(na.rm = TRUE) / 1e12
    tibble(Approach = "29 countries (balanced panel)", Year = .x, RVP_T = rvp29)
  })
) %>%
  left_join(fao %>% mutate(FAO_VoP_T = VoP / 1e12) %>% select(Year, FAO_VoP_T),
            by = "Year") %>%
  mutate(Share_of_FAO = round(RVP_T / FAO_VoP_T * 100, 1))

cat("\nCoverage vs FAOSTAT (2020-2024):\n")
print(coverage_df %>%
  filter(Year >= 2020) %>%
  select(Year, Approach, RVP_T, FAO_VoP_T, Share_of_FAO) %>%
  arrange(Year, Approach))

# -----------------------------------------------------------------------------
# 7. COMPUTE GLOBAL AND INCOME GROUP AGGREGATES
# -----------------------------------------------------------------------------

# Production-weighted NRA = sum(Support_USD) / sum(RVP)
# RVP here uses the NRA_Total row (not component rows) to avoid quadruple counting
# For reported countries: use NRA_Total RVP from cty_rvp
# For imputed countries: use last known year RVP from cty_rvp

rvp_for_nra <- bind_rows(
  cty_rvp %>%
    filter(Country_Code %in% countries_46) %>%
    select(Country_Code, Year, RVP, WBNAME2015, REGIONNAME),
  # Imputed RVP for missing country-years
  map_dfr(TARGET_YRS, function(yr) {
    map_dfr(missing_by_yr[[as.character(yr)]], function(code) {
      last_rvp <- cty_rvp %>%
        filter(Country_Code == code) %>%
        slice_max(Year, n = 1)
      if (nrow(last_rvp) == 0) return(NULL)
      last_rvp %>%
        mutate(Year = yr) %>%
        select(Country_Code, Year, RVP, WBNAME2015, REGIONNAME)
    })
  })
)

# Global NRA by year
global_nra <- combined_46 %>%
  group_by(Year) %>%
  summarise(Total_USD_bn = sum(Support_USD, na.rm = TRUE) / 1e9,
            .groups = "drop") %>%
  left_join(
    rvp_for_nra %>%
      group_by(Year) %>%
      summarise(Total_RVP = sum(RVP, na.rm = TRUE), .groups = "drop"),
    by = "Year"
  ) %>%
  mutate(
    NRA_pct  = round(Total_USD_bn * 1e9 / Total_RVP * 100, 2),
    Imputed  = Year %in% TARGET_YRS
  )

cat("\nGlobal support and NRA (46-country, 2020-2024):\n")
print(global_nra %>% filter(Year >= 2020))

# Income group NRA (production-weighted)
income_nra <- combined_46 %>%
  left_join(rvp_for_nra %>% select(Country_Code, Year, RVP) %>%
              rename(RVP_country = RVP),
            by = c("Country_Code","Year")) %>%
  group_by(WBNAME2015, Year) %>%
  summarise(
    Total_USD_bn = sum(Support_USD, na.rm = TRUE) / 1e9,
    Total_RVP    = sum(RVP_country, na.rm = TRUE) / n_distinct(NRA_Cat),
    .groups = "drop"
  ) %>%
  mutate(NRA_pct = round(Total_USD_bn * 1e9 / Total_RVP * 100, 2))

cat("\nIncome group NRA — avg 2020-2024 (imputed 46):\n")
print(income_nra %>%
  filter(Year >= 2020, !is.na(WBNAME2015)) %>%
  group_by(WBNAME2015) %>%
  summarise(Avg_USD_bn = round(mean(Total_USD_bn), 1),
            Avg_NRA    = round(mean(NRA_pct), 2),
            .groups = "drop"))

# -----------------------------------------------------------------------------
# 8. SAVE OUTPUTS
# -----------------------------------------------------------------------------

write_csv(combined_46,        "imputed_support_46.csv")
write_csv(imputation_summary, "imputation_summary.csv")
write_csv(coverage_df,        "coverage_comparison.csv")
write_csv(global_nra,         "global_nra_imputed.csv")
write_csv(income_nra,         "income_nra_imputed.csv")

cat("\nOutputs saved:\n")
cat("  imputed_support_46.csv    — full 46-country dataset (reported + imputed)\n")
cat("  imputation_summary.csv    — country-year imputed values and methods\n")
cat("  coverage_comparison.csv   — RVP coverage vs FAOSTAT by approach\n")
cat("  global_nra_imputed.csv    — global annual support and NRA\n")
cat("  income_nra_imputed.csv    — income group support and NRA by year\n")

# =============================================================================
# EXTENSION: REFINED APPROACH — HOLD NRP, TREND SUBSIDIES SEPARATELY
# =============================================================================
# Uncomment and run this section to implement the refined decomposition.
#
# Rationale: NRP (price support) is determined by trade policy (tariffs,
# import quotas, export taxes) which changes infrequently. Subsidies
# (Inputs, Outputs, Others) are budget-driven and vary annually.
# Holding NRP at last known and trending subsidies separately is more
# theoretically defensible.
#
# impute_component <- function(code, target_year, component,
#                               fit_start = FIT_START,
#                               cv_threshold = CV_THRESHOLD) {
#
#   annual <- cty_all %>%
#     filter(Country_Code == code, NRA_Cat == component, Year >= fit_start) %>%
#     group_by(Year) %>%
#     summarise(USD = sum(Support_USD, na.rm = TRUE), .groups = "drop") %>%
#     arrange(Year)
#
#   last_yr  <- max(annual$Year)
#   last_val <- annual %>% filter(Year == last_yr) %>% pull(USD)
#
#   if (component == "NRP") {
#     # Hold NRP at last known year — trade policy is sticky
#     return(tibble(NRA_Cat = "NRP", Imputed_USD = last_val,
#                   Method = "carry_forward (trade policy sticky)"))
#   }
#
#   # Subsidies: linear trend or 3-year average
#   if (nrow(annual) < 3) {
#     return(tibble(NRA_Cat = component, Imputed_USD = last_val,
#                   Method = "carry_forward (few obs)"))
#   }
#   cv <- sd(annual$USD) / abs(mean(annual$USD))
#   if (cv > cv_threshold || nrow(annual) < 5) {
#     return(tibble(NRA_Cat = component,
#                   Imputed_USD = mean(tail(annual$USD, 3)),
#                   Method = "3yr_avg"))
#   }
#   fit <- lm(USD ~ Year, data = annual)
#   tibble(NRA_Cat  = component,
#          Imputed_USD = predict(fit, newdata = tibble(Year = target_year)),
#          Method   = "linear_trend")
# }
#
# # Run component-level imputation
# components <- c("NRP", "Inputs", "Outputs", "Others")
#
# imputed_refined <- map_dfr(TARGET_YRS, function(yr) {
#   map_dfr(missing_by_yr[[as.character(yr)]], function(code) {
#     map_dfr(components, ~impute_component(code, yr, .x)) %>%
#       mutate(Country_Code = code, Year = yr)
#   })
# })
#
# write_csv(imputed_refined, "imputed_refined_components.csv")
