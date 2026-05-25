# =============================================================================
# Agricultural Support Charts — R Script v3
# Reproduces Figures 1–6 for Part 1 blog using the 53-country imputed approach
#
# Data inputs:
#   Support_Crop_2026.csv
#   Support_livestock_2026.csv
#   Support_total_2026.csv
#   VoP_FAOSTAT.csv
#
# Key methodological decisions
# ----------------------------------------------------------------------------
# 1. COUNTRY SET
#    All figures use the 53-country set — countries present in 2020.
#    Missing years (2021-2024) are imputed per country:
#      - NRP (price support): carry-forward last known value (trade policy sticky)
#      - Subsidies (Inputs, Outputs, Others): OLS linear trend on 2015-last known;
#        3-year average fallback where coefficient of variation > 1.0
#
# 2. NRA AGGREGATION
#    All NRA figures use production-weighted averages:
#      NRA = sum(Support_USD) / sum(RVP) * 100
#    where RVP is taken from the NRP row (sector total; same for all components).
#    Simple means of annual NRA rates are NOT used.
#
# 3. USD AVERAGES
#    Average annual USD = sum(Support_USD 2020-2024) / 5 / 1e9
#
# 4. RVP denominator
#    Always use the NRP row RVP — it equals the total sector value of production
#    and is identical across all NRA_Cat rows for the same country-year.
#    Summing RVP across NRA_Cat rows would overcount by a factor of 4.
# =============================================================================

library(tidyverse)
library(scales)
library(patchwork)

# ── COLOUR PALETTE ────────────────────────────────────────────────────────────
COL_PS  <- "#1A6FBF"   # Price support
COL_SUB <- "#E07B00"   # Subsidy
COL_OTH <- "#2AAD6F"   # Others
COL_OUT <- "#C0392B"   # Outputs
COL_INP <- "#E07B00"   # Inputs

# ── SHARED THEME ──────────────────────────────────────────────────────────────
theme_agri <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(colour = "grey20", face = "bold", size = 10),
      axis.title.y       = element_text(size = 10, colour = "grey30", face = "plain"),
      axis.title.x       = element_blank(),
      axis.line          = element_line(colour = "grey20", linewidth = 0.5),
      axis.ticks         = element_line(colour = "grey20"),
      plot.title         = element_blank(),   # titles handled by figure captions
      legend.position    = "bottom",
      legend.title       = element_blank(),
      legend.text        = element_text(size = 10),
      plot.margin        = margin(8, 12, 8, 8)
    )
}

theme_agri_area <- function(base_size = 11) {
  theme_agri(base_size) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.4))
}

# ── LOAD DATA ─────────────────────────────────────────────────────────────────
crop <- read_csv("NRA/Support_Database/Support_Crop_2026.csv") %>%
  mutate(Support_USD = as.numeric(Support_USD),
         RVP         = as.numeric(RelevantValueProduction))

lvst <- read_csv("NRA/Support_Database/Support_livestock_2026.csv") %>%
  mutate(Support_USD = as.numeric(Support_USD),
         RVP         = as.numeric(RelevantValueProduction))

totl <- read_csv("NRA/Support_Database/Support_total_2026.csv") %>%
  mutate(Support_USD = as.numeric(Support_USD),
         RVP         = as.numeric(RelevantValueProduction))

fao  <- read_csv("NRA/test/VoP_FAOSTAT.csv") %>%
  mutate(VoP = as.numeric(gsub(",", "", ValueOfProduction))) %>%
  select(Year, VoP)

# ── MAPPINGS ──────────────────────────────────────────────────────────────────
income_map <- c(
  ARG="Middle Income", AUS="High Income",  BRA="Middle Income", BFA="Low Income",
  BGD="Low Income",    BLZ="Middle Income",BOL="Middle Income", CAN="High Income",
  CHE="High Income",   CHL="High Income",  CHN="Middle Income", COL="Middle Income",
  CRI="Middle Income", DOM="Middle Income",ECU="Middle Income", ETH="Low Income",
  EUR="High Income",   GBR="High Income",  GHA="Middle Income", IDN="Middle Income",
  IND="Middle Income", ISL="High Income",  ISR="High Income",   JPN="High Income",
  KAZ="Middle Income", KEN="Middle Income",KOR="High Income",   MEX="Middle Income",
  MLI="Low Income",    MOZ="Low Income",   MWI="Low Income",    NGA="Middle Income",
  NOR="High Income",   NZL="High Income",  PER="Middle Income", PHL="Middle Income",
  PRY="Middle Income", RUS="Middle Income",RWA="Low Income",    SEN="Low Income",
  SLV="Middle Income", TUR="Middle Income",TZA="Low Income",    UGA="Low Income",
  UKR="Middle Income", URY="High Income",  USA="High Income",   VNM="Middle Income",
  ZAF="Middle Income", ZMB="Low Income",   ZWE="Low Income",    BDI="Low Income",
  BEN="Low Income"
)

region_map <- c(
  CHN="Asia",   IDN="Asia",       IND="Asia",       JPN="Asia",
  KAZ="Asia",   KOR="Asia",       PHL="Asia",       VNM="Asia",
  BGD="Asia",   ISR="Asia",
  AUS="Oceania",NZL="Oceania",
  EUR="Europe", CHE="Europe",     GBR="Europe",     ISL="Europe",
  NOR="Europe", RUS="Europe",     TUR="Europe",     UKR="Europe",
  CAN="N. America", USA="N. America",
  ARG="Lat. America", BRA="Lat. America", CHL="Lat. America",
  COL="Lat. America", CRI="Lat. America", MEX="Lat. America",
  DOM="Lat. America", PER="Lat. America", PRY="Lat. America",
  BOL="Lat. America", URY="Lat. America", ECU="Lat. America",
  SLV="Lat. America", BLZ="Lat. America",
  ETH="Africa", ZAF="Africa", NGA="Africa", UGA="Africa",
  SEN="Africa", MLI="Africa", MOZ="Africa", MWI="Africa",
  KEN="Africa", RWA="Africa", BFA="Africa", BEN="Africa",
  TZA="Africa", ZMB="Africa", ZWE="Africa", BDI="Africa",
  GHA="Africa"
)

# ── 53-COUNTRY SET ────────────────────────────────────────────────────────────
c53_crop <- crop %>%
  filter(AGGREGATION == "CountryXSector", Year == 2020) %>%
  pull(Country_Code) %>% unique()

c53_lvst <- lvst %>%
  filter(AGGREGATION == "CountryXSector", Year == 2020) %>%
  pull(Country_Code) %>% unique()

c53_totl <- totl %>%
  filter(AGGREGATION == "CountryXAllAg", Year == 2020) %>%
  pull(Country_Code) %>% unique()

# ── IMPUTATION ────────────────────────────────────────────────────────────────
FIT_START    <- 2015
CV_THRESHOLD <- 1.0
IMP_YEARS    <- 2021:2024

#' Impute one NRA_Cat component for one country for one year.
#' NRP: carry-forward last known (trade policy sticky).
#' Subsidies: OLS linear trend; 3-year average if CV > threshold.
impute_component <- function(series_df, cat, target_yr,
                             fit_start = FIT_START, cv_thr = CV_THRESHOLD) {
  s <- series_df %>% arrange(Year)
  last_usd <- if (nrow(s) > 0) tail(s$Support_USD, 1) else 0
  last_rvp <- if (nrow(s) > 0) tail(s$RVP, 1)         else NA_real_

  if (cat == "NRP") return(tibble(Support_USD = last_usd, RVP = last_rvp))

  hist <- s %>% filter(Year >= fit_start)
  if (nrow(hist) >= 5) {
    y  <- hist$Support_USD
    cv <- if (mean(y) != 0) sd(y) / abs(mean(y)) else Inf
    if (cv > cv_thr) {
      imp_usd <- mean(tail(y, 3))
    } else {
      fit     <- lm(Support_USD ~ Year, data = hist)
      imp_usd <- predict(fit, newdata = tibble(Year = target_yr))
    }
  } else {
    imp_usd <- last_usd
  }
  tibble(Support_USD = imp_usd, RVP = last_rvp)
}

#' Build full 53-country dataset for one sector.
#' Reported values used as-is; missing country-years imputed.
build_53c <- function(df, c53, agg_val) {
  cats <- c("NRP", "Inputs", "Outputs", "Others")
  rep  <- df %>%
    filter(AGGREGATION == agg_val, Country_Code %in% c53) %>%
    select(Country_Code, Year, NRA_Cat, Support_USD, RVP)

  imp_rows <- map_dfr(c53, function(code) {
    sub      <- rep %>% filter(Country_Code == code)
    rep_yrs  <- unique(sub$Year)
    miss_yrs <- setdiff(IMP_YEARS, rep_yrs)
    if (length(miss_yrs) == 0) return(NULL)

    map_dfr(miss_yrs, function(yr) {
      map_dfr(cats, function(cat) {
        series <- sub %>% filter(NRA_Cat == cat)
        vals   <- impute_component(series, cat, yr)
        tibble(Country_Code = code, Year = yr, NRA_Cat = cat,
               Support_USD  = vals$Support_USD,
               RVP          = vals$RVP)
      })
    })
  })

  bind_rows(rep, imp_rows) %>%
    mutate(
      INCOME = income_map[Country_Code],
      REGION = region_map[Country_Code]
    )
}

message("Building 53-country datasets...")
crop53 <- build_53c(crop, c53_crop, "CountryXSector")
lvst53 <- build_53c(lvst, c53_lvst, "CountryXSector")
totl53 <- build_53c(totl, c53_totl, "CountryXAllAg")
message("  Done. crop53: ", nrow(crop53), " rows | lvst53: ",
        nrow(lvst53), " | totl53: ", nrow(totl53))

# ── AGGREGATION HELPERS ───────────────────────────────────────────────────────

#' Production-weighted NRA and avg annual USD by group (2020-2024).
#' NRA  = sum(Support_USD) / sum(RVP from NRP rows) * 100
#' USD  = sum(Support_USD) / 5 years / 1e9
agg_group <- function(df, group_col, group_order) {
  sub <- df %>%
    filter(Year >= 2020, !NRA_Cat %in% c("NRA_Total")) %>%
    drop_na(all_of(group_col))

  rvp_base <- sub %>%
    filter(NRA_Cat == "NRP") %>%
    group_by(across(all_of(group_col))) %>%
    summarise(Total_RVP = sum(RVP, na.rm = TRUE), .groups = "drop")

  sub %>%
    group_by(across(all_of(c(group_col, "NRA_Cat")))) %>%
    summarise(Total_USD = sum(Support_USD, na.rm = TRUE), .groups = "drop") %>%
    left_join(rvp_base, by = group_col) %>%
    mutate(
      Avg_USD_bn = Total_USD / 5 / 1e9,
      NRA_pct    = Total_USD / Total_RVP * 100,
      Component  = if_else(NRA_Cat == "NRP", "Price support", "Subsidy"),
      !!group_col := factor(.data[[group_col]], levels = group_order)
    ) %>%
    group_by(across(all_of(c(group_col, "Component")))) %>%
    summarise(Avg_USD_bn = sum(Avg_USD_bn),
              NRA_pct    = sum(NRA_pct),
              .groups    = "drop")
}

#' Global annual USD and production-weighted NRA for stacked area charts.
agg_global_annual <- function(df) {
  sub <- df %>% filter(!NRA_Cat %in% c("NRA_Total"))

  rvp_annual <- sub %>%
    filter(NRA_Cat == "NRP") %>%
    group_by(Year) %>%
    summarise(Total_RVP = sum(RVP, na.rm = TRUE), .groups = "drop")

  sub %>%
    group_by(Year, NRA_Cat) %>%
    summarise(USD_bn = sum(Support_USD, na.rm = TRUE) / 1e9, .groups = "drop") %>%
    left_join(rvp_annual, by = "Year") %>%
    mutate(
      NRA_pct   = USD_bn * 1e9 / Total_RVP * 100,
      NRA_Cat   = recode(NRA_Cat, NRP = "Price support"),
      NRA_Cat   = factor(NRA_Cat,
                         levels = c("Others", "Inputs", "Outputs", "Price support"))
    )
}

# ── PLOT HELPERS ──────────────────────────────────────────────────────────────
plot_stacked_bar <- function(dat, x_col, metric, y_lab) {
  fill_vals <- c("Price support" = COL_PS, "Subsidy" = COL_SUB)
  y_col     <- if (metric == "usd") "Avg_USD_bn" else "NRA_pct"
  fmt       <- if (metric == "usd") dollar_format(prefix = "$", suffix = "B", accuracy = 1) 
               else function(x) paste0(x, "%")
  ggplot(dat, aes(.data[[x_col]], .data[[y_col]], fill = Component)) +
    geom_col(width = 0.6) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    scale_fill_manual(values = fill_vals) +
    scale_y_continuous(labels = fmt) +
    labs(y = y_lab) +
    theme_agri()
}

# ── ORDER DEFINITIONS ─────────────────────────────────────────────────────────
INC_ORDER   <- c("High Income", "Middle Income", "Low Income")
REG_C_ORDER <- c("Asia", "N. America", "Europe", "Lat. America", "Oceania", "Africa")
REG_L_ORDER <- c("Asia", "Europe",    "N. America", "Lat. America", "Oceania", "Africa")

# ── FIGURE 1 — Global agricultural support (USD) stacked area ─────────────────
totl_ann <- agg_global_annual(totl53)

area_cols <- c("Others"        = paste0(COL_OTH, "BB"),
               "Inputs"        = paste0(COL_INP, "BB"),
               "Outputs"       = paste0(COL_OUT, "BB"),
               "Price support" = paste0(COL_PS,  "BB"))

figure1 <- ggplot(totl_ann, aes(Year, USD_bn, fill = NRA_Cat)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.25) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  annotate("rect", xmin = 2020.5, xmax = 2024.5, ymin = -Inf, ymax = Inf,
           fill = "grey50", alpha = 0.08) +
  annotate("segment", x = 2020.5, xend = 2020.5, y = -Inf, yend = Inf,
           colour = "grey40", linewidth = 0.8, linetype = "dashed") +
  scale_fill_manual(values = area_cols,
                    breaks = c("Price support", "Outputs", "Inputs", "Others")) +
  scale_x_continuous(breaks = seq(2005, 2024, 3)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "B", accuracy = 1)) +
  labs(y = "USD billions") +
  theme_agri_area() +
  guides(fill = guide_legend(nrow = 1))

# ── FIGURE 2 — Global NRA stacked area ────────────────────────────────────────
figure2 <- ggplot(totl_ann, aes(Year, NRA_pct, fill = NRA_Cat)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.25) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  annotate("rect", xmin = 2020.5, xmax = 2024.5, ymin = -Inf, ymax = Inf,
           fill = "grey50", alpha = 0.08) +
  annotate("segment", x = 2020.5, xend = 2020.5, y = -Inf, yend = Inf,
           colour = "grey40", linewidth = 0.8, linetype = "dashed") +
  scale_fill_manual(values = area_cols,
                    breaks = c("Price support", "Outputs", "Inputs", "Others")) +
  scale_x_continuous(breaks = seq(2005, 2024, 3)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(y = "NRA rate (%)") +
  theme_agri_area() +
  guides(fill = guide_legend(nrow = 1))

# ── FIGURES 3 & 4 — Regional panels ──────────────────────────────────────────
crop_reg <- agg_group(crop53, "REGION", REG_C_ORDER)
lvst_reg <- agg_group(lvst53, "REGION", REG_L_ORDER)

figure3 <- (plot_stacked_bar(crop_reg, "REGION", "usd", "USD bn/yr") +
              ggtitle("Crop sector")) +
           (plot_stacked_bar(lvst_reg, "REGION", "usd", "USD bn/yr") +
              ggtitle("Livestock sector")) +
           plot_layout(guides = "collect") &
           theme(legend.position = "bottom",
                 plot.title = element_text(face = "bold", size = 11))

figure4 <- (plot_stacked_bar(crop_reg, "REGION", "nra", "NRA rate (%)") +
              ggtitle("Crop sector")) +
           (plot_stacked_bar(lvst_reg, "REGION", "nra", "NRA rate (%)") +
              ggtitle("Livestock sector")) +
           plot_layout(guides = "collect") &
           theme(legend.position = "bottom",
                 plot.title = element_text(face = "bold", size = 11))

# ── FIGURES 5 & 6 — Income group panels ──────────────────────────────────────
crop_inc <- agg_group(crop53, "INCOME", INC_ORDER)
lvst_inc <- agg_group(lvst53, "INCOME", INC_ORDER)

figure5 <- (plot_stacked_bar(crop_inc, "INCOME", "usd", "USD bn/yr") +
              ggtitle("Crop sector")) +
           (plot_stacked_bar(lvst_inc, "INCOME", "usd", "USD bn/yr") +
              ggtitle("Livestock sector")) +
           plot_layout(guides = "collect") &
           theme(legend.position = "bottom",
                 plot.title = element_text(face = "bold", size = 11))

figure6 <- (plot_stacked_bar(crop_inc, "INCOME", "nra", "NRA rate (%)") +
              ggtitle("Crop sector")) +
           (plot_stacked_bar(lvst_inc, "INCOME", "nra", "NRA rate (%)") +
              ggtitle("Livestock sector")) +
           plot_layout(guides = "collect") &
           theme(legend.position = "bottom",
                 plot.title = element_text(face = "bold", size = 11))

# ── SAVE ALL FIGURES ──────────────────────────────────────────────────────────
dir.create("output_figures", showWarnings = FALSE)

ggsave("output_figures/figure1_global_usd.png",  figure1, width=10, height=5, dpi=300, bg="white")
ggsave("output_figures/figure2_global_nra.png",  figure2, width=10, height=5, dpi=300, bg="white")
ggsave("output_figures/figure3_region_usd.png",  figure3, width=12, height=5, dpi=300, bg="white")
ggsave("output_figures/figure4_region_nra.png",  figure4, width=12, height=5, dpi=300, bg="white")
ggsave("output_figures/figure5_income_usd.png",  figure5, width=10, height=5, dpi=300, bg="white")
ggsave("output_figures/figure6_income_nra.png",  figure6, width=10, height=5, dpi=300, bg="white")

message("All 6 figures saved to output_figures/")

# ── EXPORT DATA TABLES ────────────────────────────────────────────────────────
# Global annual
fig1_data <- totl_ann %>%
  select(Year, NRA_Cat, USD_bn, NRA_pct) %>%
  mutate(Imputed = if_else(Year >= 2021, "Yes", "No")) %>%
  pivot_wider(names_from = NRA_Cat,
              values_from = c(USD_bn, NRA_pct))

# Regional
fig3_data <- bind_rows(
  crop_reg %>% mutate(Sector = "Crop"),
  lvst_reg %>% mutate(Sector = "Livestock")
)

# Income
fig5_data <- bind_rows(
  crop_inc %>% mutate(Sector = "Crop"),
  lvst_inc %>% mutate(Sector = "Livestock")
)

write_csv(fig1_data, "output_figures/data_fig1_fig2_global.csv")
write_csv(fig3_data, "output_figures/data_fig3_fig4_region.csv")
write_csv(fig5_data, "output_figures/data_fig5_fig6_income.csv")

message("Data tables saved to output_figures/")
