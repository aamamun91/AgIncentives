# =============================================================================
# Agricultural Support Charts — R Script
# Reproduces all 21 charts using ggplot2
# Data: Support_Crop_2026.csv, Support_livestock_2026.csv,
#       Support_total_2026.csv, Support_detailed_2026.csv
#
# NOTE ON NRA AGGREGATION (2020-2024 averages)
# ----------------------------------------------------------------------------
# NRA at regional and income group level is computed as a production-weighted
# average: sum(Support_USD) / sum(RelevantValueProduction) across all years
# and entities in each group. This is consistent with the AgIncentives
# database methodology which defines NRA = Support_USD / RVP at each level.
# Simple (unweighted) means of NRA are NOT used for cross-sectional charts.
#
# For the global annual trend (charts 1, 6, 10, 11), the pre-aggregated
# WorldxSector / WorldxAllAg rows from the database are used directly, as
# these already embed production-weighted aggregation across countries.
# =============================================================================

library(tidyverse)
library(scales)
library(patchwork)

# -----------------------------------------------------------------------------
# COLOUR PALETTE
# -----------------------------------------------------------------------------
COL_PS  <- "#1A6FBF"   # Price support (blue)
COL_SUB <- "#E07B00"   # Subsidy (orange)
COL_NEG <- "#C0392B"   # Negative / taxed (red)
COL_INP <- "#E07B00"   # Inputs
COL_OTH <- "#2AAD6F"   # Others (green)
COL_OUT <- "#C0392B"   # Outputs (red)
COL_CRP <- "#1A6FBF"   # Crop
COL_LVS <- "#E07B00"   # Livestock

# -----------------------------------------------------------------------------
# SHARED THEMES
# -----------------------------------------------------------------------------
theme_agri <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.4),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(colour = "grey20", face = "bold", size = 10),
      axis.title         = element_blank(),
      axis.line          = element_line(colour = "grey20", linewidth = 0.6),
      axis.ticks         = element_line(colour = "grey20"),
      plot.title         = element_text(face = "bold", size = 12, colour = "grey10"),
      plot.subtitle      = element_text(size = 9, colour = "grey40"),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      legend.text        = element_text(size = 10, face = "bold"),
      plot.margin        = margin(10, 15, 10, 10)
    )
}

theme_agri_flip <- function(base_size = 11) {
  theme_agri(base_size) +
    theme(
      panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.4),
      panel.grid.major.y = element_blank()
    )
}

theme_agri_line <- function(base_size = 11) {
  theme_agri(base_size) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.4)
    )
}

# -----------------------------------------------------------------------------
# LOAD DATA
# -----------------------------------------------------------------------------
crop <- read_csv("Support_Crop_2026.csv")
lvst <- read_csv("Support_livestock_2026.csv")
totl <- read_csv("Support_total_2026.csv")
detl <- read_csv("Support_detailed_2026.csv")

# Income group display order: High | Middle | Low
INC_ORDER <- c("High Income", "Middle Income", "Low Income")

INC_LBL <- c("High Income Countries"   = "High Income",
             "Middle Income Countries" = "Middle Income",
             "Low Income Countries"    = "Low Income")

REG_LBL <- c("Northern America"                = "N. America",
             "Latin America and the Caribbean" = "Lat. America")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Production-weighted NRA for grouped data (region / income), 2020-2024
# Formula: sum(Support_USD) / sum(RVP) per group x NRA_Cat
prep_group_nra_wtd <- function(df, agg_val, group_col, label_map = NULL) {
  dat <- df %>%
    filter(AGGREGATION == agg_val, Year >= 2020) %>%
    mutate(Support_USD             = as.numeric(Support_USD),
           RelevantValueProduction = as.numeric(RelevantValueProduction)) %>%
    filter(!is.na(RelevantValueProduction), RelevantValueProduction > 0) %>%
    group_by(across(all_of(c(group_col, "NRA_Cat")))) %>%
    summarise(
      WtdNRA = sum(Support_USD, na.rm = TRUE) /
               sum(RelevantValueProduction, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    mutate(Component = if_else(NRA_Cat == "NRP", "Price support", "Subsidy")) %>%
    group_by(across(all_of(c(group_col, "Component")))) %>%
    summarise(NRA = sum(WtdNRA), .groups = "drop")

  if (!is.null(label_map)) dat[[group_col]] <- recode(dat[[group_col]], !!!label_map)

  tot <- dat %>%
    group_by(across(all_of(group_col))) %>%
    summarise(Total = sum(NRA), .groups = "drop")

  left_join(dat, tot, by = group_col) %>%
    mutate(!!group_col := fct_reorder(.data[[group_col]], Total))
}

# Production-weighted NRA for country level, 2020-2024
prep_country_nra_wtd <- function(df, agg_val, n = 10) {
  base <- df %>%
    filter(AGGREGATION == agg_val, Year >= 2020) %>%
    mutate(Support_USD             = as.numeric(Support_USD),
           RelevantValueProduction = as.numeric(RelevantValueProduction)) %>%
    filter(!is.na(RelevantValueProduction), RelevantValueProduction > 0) %>%
    group_by(LISTNAME_EN, NRA_Cat) %>%
    summarise(
      WtdNRA = sum(Support_USD, na.rm = TRUE) /
               sum(RelevantValueProduction, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    mutate(Component = if_else(NRA_Cat == "NRP", "Price support", "Subsidy")) %>%
    group_by(LISTNAME_EN, Component) %>%
    summarise(NRA = sum(WtdNRA), .groups = "drop") %>%
    group_by(LISTNAME_EN) %>%
    mutate(Total = sum(NRA)) %>%
    ungroup()

  top <- base %>% slice_max(Total, n = n * 2, with_ties = FALSE) %>%
    mutate(group = "top")
  bot <- base %>% slice_min(Total, n = n * 2, with_ties = FALSE) %>%
    mutate(group = "bottom")

  bind_rows(bot, top) %>%
    mutate(
      LISTNAME_EN = fct_reorder(LISTNAME_EN, Total),
      fill_col = case_when(
        Component == "Subsidy"                    ~ "Subsidy",
        Component == "Price support" & NRA >= 0   ~ "Price support (+)",
        TRUE                                       ~ "Price support (-)"
      )
    )
}

# Average annual USD for grouped data (simple mean is correct for $/yr)
prep_group_usd <- function(df, agg_val, group_col, label_map = NULL) {
  dat <- df %>%
    filter(AGGREGATION == agg_val, Year >= 2020) %>%
    mutate(Support_USD = as.numeric(Support_USD)) %>%
    group_by(across(all_of(c(group_col, "NRA_Cat")))) %>%
    summarise(Support_USD = mean(Support_USD, na.rm = TRUE) / 1e9, .groups = "drop") %>%
    mutate(Component = if_else(NRA_Cat == "NRP", "Price support", "Subsidy")) %>%
    group_by(across(all_of(c(group_col, "Component")))) %>%
    summarise(Support_USD = sum(Support_USD), .groups = "drop")

  if (!is.null(label_map)) dat[[group_col]] <- recode(dat[[group_col]], !!!label_map)

  tot <- dat %>%
    group_by(across(all_of(group_col))) %>%
    summarise(Total = sum(Support_USD), .groups = "drop")

  left_join(dat, tot, by = group_col) %>%
    mutate(!!group_col := fct_reorder(.data[[group_col]], Total))
}

# Fix income group x-axis to High | Middle | Low order
fix_income_order <- function(dat, group_col) {
  dat %>%
    mutate(!!group_col := factor(.data[[group_col]], levels = rev(INC_ORDER)))
}

# Production-weighted global NRA for crop vs livestock comparison
calc_global_nra_wtd <- function(df, sector_label) {
  df %>%
    filter(AGGREGATION == "WorldxSector", Year >= 2020) %>%
    mutate(Support_USD             = as.numeric(Support_USD),
           RelevantValueProduction = as.numeric(RelevantValueProduction)) %>%
    filter(!is.na(RelevantValueProduction), RelevantValueProduction > 0) %>%
    group_by(NRA_Cat) %>%
    summarise(
      WtdNRA = sum(Support_USD, na.rm = TRUE) /
               sum(RelevantValueProduction, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    mutate(Component = if_else(NRA_Cat == "NRP", "Price support", "Subsidy"),
           Sector    = sector_label) %>%
    group_by(Sector, Component) %>%
    summarise(NRA = sum(WtdNRA), .groups = "drop")
}

# =============================================================================
# PLOT HELPERS
# =============================================================================
plot_stacked_bar <- function(dat, group_col, title, subtitle = "") {
  ggplot(dat, aes(.data[[group_col]], Support_USD, fill = Component)) +
    geom_col(width = 0.6) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    scale_fill_manual(values = c("Price support" = COL_PS, "Subsidy" = COL_SUB)) +
    scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "B",
                                              scale = 1, accuracy = 1)) +
    labs(title = title, subtitle = subtitle) +
    theme_agri()
}

plot_nra_bar <- function(dat, group_col, title, subtitle = "") {
  ggplot(dat, aes(.data[[group_col]], NRA, fill = Component)) +
    geom_col(width = 0.6) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    scale_fill_manual(values = c("Price support" = COL_PS, "Subsidy" = COL_SUB)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = title, subtitle = subtitle) +
    theme_agri()
}

plot_country_horiz <- function(dat, title) {
  ggplot(dat, aes(Support_USD, LISTNAME_EN, fill = Component)) +
    geom_col(width = 0.6) +
    geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
    scale_fill_manual(values = c("Price support" = COL_PS, "Subsidy" = COL_SUB)) +
    scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "B",
                                              scale = 1, accuracy = 1)) +
    labs(title = title, subtitle = "USD billions/yr (5-yr average, 2020-2024)") +
    theme_agri_flip()
}

plot_country_nra <- function(dat, title) {
  ggplot(dat, aes(NRA, LISTNAME_EN, fill = fill_col)) +
    geom_col(width = 0.65) +
    geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.5) +
    scale_fill_manual(values = c("Price support (+)" = COL_PS,
                                 "Price support (-)" = COL_NEG,
                                 "Subsidy"           = COL_SUB)) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    labs(title    = title,
         subtitle = "Production-weighted NRA rate (%), avg 2020-2024. Top 10 & bottom 10.") +
    theme_agri_flip()
}

plot_area <- function(dat, y_label, title, subtitle) {
  cols <- c("Others" = COL_OTH, "Inputs" = COL_INP,
            "Outputs" = COL_OUT, "Price support" = COL_PS)
  ggplot(dat, aes(Year, val, fill = NRA_Cat)) +
    geom_area(alpha = 0.75, colour = "white", linewidth = 0.3) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    scale_fill_manual(values = cols,
                      breaks = c("Price support", "Outputs", "Inputs", "Others")) +
    scale_x_continuous(breaks = seq(2005, 2024, 3)) +
    scale_y_continuous(labels = function(x) paste0(x, y_label)) +
    labs(title = title, subtitle = subtitle) +
    theme_agri_line() +
    guides(fill = guide_legend(nrow = 1))
}

# =============================================================================
# CHARTS 1 & 6 — Crop / Livestock global NRA line (2005-2024)
# Pre-aggregated WorldxSector rows used — already production-weighted by year
# =============================================================================
world_nra_line <- function(df, agg_val) {
  df %>%
    filter(AGGREGATION == agg_val) %>%
    group_by(Year, NRA_Cat) %>%
    summarise(NRA = mean(NRA, na.rm = TRUE) * 100, .groups = "drop") %>%
    mutate(NRA_Cat = recode(NRA_Cat, NRP = "Price support"))
}

plot_nra_line <- function(df, agg_val, sector_label) {
  dat    <- world_nra_line(df, agg_val)
  cols   <- c("Price support" = COL_PS, "Inputs" = COL_INP,
              "Others" = COL_OTH, "Outputs" = COL_OUT)
  dashes <- c("Price support" = "solid",   "Inputs"  = "longdash",
              "Others"        = "dotdash", "Outputs" = "dotted")
  ggplot(dat, aes(Year, NRA, colour = NRA_Cat, linetype = NRA_Cat)) +
    geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.4) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8) +
    scale_colour_manual(values = cols) +
    scale_linetype_manual(values = dashes) +
    scale_x_continuous(breaks = seq(2005, 2024, 3)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title    = paste(sector_label,
                          "- global NRA by support type, annual trend, 2005-2024"),
         subtitle = "NRA rate (%).") +
    theme_agri_line() +
    guides(colour = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))
}

chart01 <- plot_nra_line(crop, "WorldxSector", "Crop sector")
chart06 <- plot_nra_line(lvst, "WorldxSector", "Livestock sector")

# =============================================================================
# CHARTS 2, 3, 7, 8 — Income / Region USD
# =============================================================================
chart02 <- prep_group_usd(crop, "IncomeLevelxSector", "WBNAME2015", INC_LBL) %>%
  fix_income_order("WBNAME2015") %>%
  plot_stacked_bar("WBNAME2015",
    "Crop sector - avg annual support by type, income groups, 2020-2024",
    "USD billions/yr (5-yr average)")

chart03 <- prep_group_usd(crop, "RegionxSector", "REGIONNAME", REG_LBL) %>%
  plot_stacked_bar("REGIONNAME",
    "Crop sector - avg annual support by type, regions, 2020-2024",
    "USD billions/yr (5-yr average)")

chart07 <- prep_group_usd(lvst, "IncomeLevelxSector", "WBNAME2015", INC_LBL) %>%
  fix_income_order("WBNAME2015") %>%
  plot_stacked_bar("WBNAME2015",
    "Livestock sector - avg annual support by type, income groups, 2020-2024",
    "USD billions/yr (5-yr average)")

chart08 <- prep_group_usd(lvst, "RegionxSector", "REGIONNAME", REG_LBL) %>%
  plot_stacked_bar("REGIONNAME",
    "Livestock sector - avg annual support by type, regions, 2020-2024",
    "USD billions/yr (5-yr average)")

# =============================================================================
# CHARTS 4, 9 — Top 10 countries USD
# =============================================================================
prep_country_usd <- function(df, agg_val, n = 10) {
  df %>%
    filter(AGGREGATION == agg_val, Year >= 2020) %>%
    mutate(Support_USD = as.numeric(Support_USD)) %>%
    group_by(LISTNAME_EN, NRA_Cat) %>%
    summarise(Support_USD = mean(Support_USD, na.rm = TRUE) / 1e9, .groups = "drop") %>%
    mutate(Component = if_else(NRA_Cat == "NRP", "Price support", "Subsidy")) %>%
    group_by(LISTNAME_EN, Component) %>%
    summarise(Support_USD = sum(Support_USD), .groups = "drop") %>%
    group_by(LISTNAME_EN) %>%
    mutate(Total = sum(Support_USD)) %>%
    ungroup() %>%
    slice_max(Total, n = n * 2, with_ties = FALSE) %>%
    mutate(LISTNAME_EN = fct_reorder(LISTNAME_EN, Total))
}

chart04 <- prep_country_usd(crop, "CountryXSector") %>%
  plot_country_horiz(
    "Crop sector - avg annual support by type, top 10 countries, 2020-2024")

chart09 <- prep_country_usd(lvst, "CountryXSector") %>%
  plot_country_horiz(
    "Livestock sector - avg annual support by type, top 10 countries, 2020-2024")

# =============================================================================
# CHARTS 10, 11 — Total agriculture stacked area
# Pre-aggregated WorldxAllAg rows — already production-weighted per year
# =============================================================================
prep_total_area <- function(metric = "usd") {
  w <- totl %>%
    filter(AGGREGATION == "WorldxAllAg", NRA_Cat != "NRA_Total") %>%
    mutate(NRA_Cat = recode(NRA_Cat, NRP = "Price support")) %>%
    mutate(NRA_Cat = factor(NRA_Cat,
             levels = c("Others", "Inputs", "Outputs", "Price support")))
  if (metric == "usd") {
    w %>% group_by(Year, NRA_Cat) %>%
      summarise(val = sum(as.numeric(Support_USD), na.rm = TRUE) / 1e9,
                .groups = "drop")
  } else {
    w %>% group_by(Year, NRA_Cat) %>%
      summarise(val = mean(as.numeric(NRA), na.rm = TRUE) * 100, .groups = "drop")
  }
}

chart10 <- plot_area(prep_total_area("usd"), "B",
  "Agriculture - global support by type, annual trend, 2005-2024",
  "USD billions. Stack: Others (bottom) -> Inputs -> Outputs -> Price support (top).")

chart11 <- plot_area(prep_total_area("nra"), "%",
  "Agriculture - global NRA by type, annual trend, 2005-2024",
  "NRA rate (%). Stack: Others (bottom) -> Inputs -> Outputs -> Price support (top).")

# =============================================================================
# CHART 12 — Crop vs livestock global USD composition
# =============================================================================
cv_global_usd <- bind_rows(
  crop %>% filter(AGGREGATION == "WorldxSector", Year >= 2020) %>%
    mutate(Support_USD = as.numeric(Support_USD)) %>%
    group_by(NRA_Cat) %>%
    summarise(val = mean(Support_USD, na.rm = TRUE) / 1e9, .groups = "drop") %>%
    mutate(Component = if_else(NRA_Cat == "NRP", "Price support", "Subsidy"),
           Sector = "Crop"),
  lvst %>% filter(AGGREGATION == "WorldxSector", Year >= 2020) %>%
    mutate(Support_USD = as.numeric(Support_USD)) %>%
    group_by(NRA_Cat) %>%
    summarise(val = mean(Support_USD, na.rm = TRUE) / 1e9, .groups = "drop") %>%
    mutate(Component = if_else(NRA_Cat == "NRP", "Price support", "Subsidy"),
           Sector = "Livestock")
) %>%
  group_by(Sector, Component) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(Sector = factor(Sector, levels = c("Crop", "Livestock")))

chart12 <- ggplot(cv_global_usd, aes(Sector, val, fill = Component)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = c("Price support" = COL_PS, "Subsidy" = COL_SUB)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "B",
                                            scale = 1, accuracy = 1)) +
  labs(title    = "Crop vs livestock - global support composition, average 2020-2024",
       subtitle = "USD billions/yr. Subsidy = Inputs + Outputs + Others.") +
  theme_agri()

# =============================================================================
# CHARTS 13, 14 — Crop vs livestock income / region USD panels
# =============================================================================
c_inc <- prep_group_usd(crop, "IncomeLevelxSector", "WBNAME2015", INC_LBL) %>%
  fix_income_order("WBNAME2015")
l_inc <- prep_group_usd(lvst, "IncomeLevelxSector", "WBNAME2015", INC_LBL) %>%
  fix_income_order("WBNAME2015")

chart13 <- (plot_stacked_bar(c_inc, "WBNAME2015", "Crop sector",     "USD bn/yr") +
            plot_stacked_bar(l_inc, "WBNAME2015", "Livestock sector", "USD bn/yr")) +
  plot_annotation(
    title    = "Crop vs livestock - avg annual support by income group, 2020-2024",
    subtitle = "USD billions/yr. Subsidy = Inputs + Outputs + Others."
  ) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

c_reg <- prep_group_usd(crop, "RegionxSector", "REGIONNAME", REG_LBL)
l_reg <- prep_group_usd(lvst, "RegionxSector", "REGIONNAME", REG_LBL)

chart14 <- (plot_stacked_bar(c_reg, "REGIONNAME", "Crop sector",     "USD bn/yr") +
            plot_stacked_bar(l_reg, "REGIONNAME", "Livestock sector", "USD bn/yr")) +
  plot_annotation(
    title    = "Crop vs livestock - avg annual support by region, 2020-2024",
    subtitle = "USD billions/yr."
  ) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# =============================================================================
# CHART 15 — Crop vs livestock global NRA composition (production-weighted)
# =============================================================================
cv_global_nra <- bind_rows(
  calc_global_nra_wtd(crop, "Crop"),
  calc_global_nra_wtd(lvst, "Livestock")
) %>% mutate(Sector = factor(Sector, levels = c("Crop", "Livestock")))

chart15 <- ggplot(cv_global_nra, aes(Sector, NRA, fill = Component)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = c("Price support" = COL_PS, "Subsidy" = COL_SUB)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title    = "Crop vs livestock - global NRA composition, average 2020-2024",
       subtitle = "Production-weighted NRA rate (%). Subsidy = Inputs + Outputs + Others.") +
  theme_agri()

# =============================================================================
# CHARTS 16, 17 — Crop vs livestock income / region NRA panels (prod-weighted)
# =============================================================================
c_inc_nra <- prep_group_nra_wtd(crop, "IncomeLevelxSector", "WBNAME2015", INC_LBL) %>%
  fix_income_order("WBNAME2015")
l_inc_nra <- prep_group_nra_wtd(lvst, "IncomeLevelxSector", "WBNAME2015", INC_LBL) %>%
  fix_income_order("WBNAME2015")

chart16 <- (plot_nra_bar(c_inc_nra, "WBNAME2015", "Crop sector",     "NRA rate (%)") +
            plot_nra_bar(l_inc_nra, "WBNAME2015", "Livestock sector", "NRA rate (%)")) +
  plot_annotation(
    title    = "Crop vs livestock - NRA by income group, 2020-2024",
    subtitle = "Production-weighted NRA rate (%). Subsidy = Inputs + Outputs + Others."
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top", legend.justification = "right")

c_reg_nra <- prep_group_nra_wtd(crop, "RegionxSector", "REGIONNAME", REG_LBL)
l_reg_nra <- prep_group_nra_wtd(lvst, "RegionxSector", "REGIONNAME", REG_LBL)

chart17 <- (plot_nra_bar(c_reg_nra, "REGIONNAME", "Crop sector",     "NRA rate (%)") +
            plot_nra_bar(l_reg_nra, "REGIONNAME", "Livestock sector", "NRA rate (%)")) +
  plot_annotation(
    title    = "Crop vs livestock - NRA by region, 2020-2024",
    subtitle = "Production-weighted NRA rate (%)."
  ) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# =============================================================================
# CHARTS 18, 19 — Country NRA top 10 + bottom 10 (production-weighted)
# =============================================================================
chart18 <- prep_country_nra_wtd(crop, "CountryXSector") %>%
  plot_country_nra("Crop sector - country NRA, top 10 & bottom 10, 2020-2024")

chart19 <- prep_country_nra_wtd(lvst, "CountryXSector") %>%
  plot_country_nra("Livestock sector - country NRA, top 10 & bottom 10, 2020-2024")

# =============================================================================
# CHARTS 20, 21 — Commodity USD and NRA
# USD: mean(Support_USD)/yr — simple mean correct for $/yr
# NRA: production-weighted sum(Rate*RVP)/sum(RVP)
# =============================================================================
det_clean <- detl %>%
  mutate(Support_USD = as.numeric(Support_USD),
         Rate        = as.numeric(Rate),
         RVP         = as.numeric(RelevantValueProduction)) %>%
  filter(Year >= 2020) %>%
  filter(!Commodity_Label %in% c("All crops", "All livestock", "Crops, Non-MPS",
                                  "Livestock, Non-MPS", "Unallocated",
                                  "Non-allocated crops", "Beef and milk")) %>%
  filter(!str_detect(Commodity_Label, "NonMPS|Non-MPS|Non-allocated|Workbook"))

grp_excl <- c("Grains", "Maize, beans and rice", "Grains and oilseeds",
              "Pulses nes", "Leguminous crops", "Protein crops",
              "Horticulture", "Fruits and vegetables")

# Chart 20 — USD top 15
usd_top15 <- det_clean %>%
  filter(!Commodity_Label %in% grp_excl) %>%
  group_by(Commodity_Label, CATPROD) %>%
  summarise(Support_USD = mean(Support_USD, na.rm = TRUE) / 1e9, .groups = "drop") %>%
  slice_max(Support_USD, n = 15) %>%
  mutate(Commodity_Label = fct_reorder(Commodity_Label, Support_USD),
         Sector = if_else(CATPROD == "LVS", "Livestock", "Crop"))

chart20 <- ggplot(usd_top15, aes(Support_USD, Commodity_Label, fill = Sector)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = c("Crop" = COL_CRP, "Livestock" = COL_LVS)) +
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "B",
                                            scale = 1, accuracy = 0.1)) +
  labs(title    = "Top 15 commodities by average annual support, 2020-2024",
       subtitle = "USD billions/yr (5-yr average). Crop = blue, Livestock = orange.") +
  theme_agri_flip()

# Chart 21 — NRA top 15 + bottom 10 (production-weighted)
nra_ranked <- det_clean %>%
  filter(!Commodity_Label %in% grp_excl) %>%
  filter(!is.na(Rate), !is.na(RVP), RVP > 0) %>%
  group_by(Commodity_Label, CATPROD) %>%
  summarise(WtRate = sum(Rate * RVP, na.rm = TRUE) /
                     sum(RVP, na.rm = TRUE) * 100,
            .groups = "drop")

nra_plot_dat <- bind_rows(
  nra_ranked %>% slice_max(WtRate, n = 15),
  nra_ranked %>% slice_min(WtRate, n = 10)
) %>%
  mutate(
    Commodity_Label = fct_reorder(Commodity_Label, WtRate),
    fill_col = case_when(
      WtRate < 0       ~ "Taxed (-)",
      CATPROD == "LVS" ~ "Livestock",
      TRUE             ~ "Crop"
    )
  )

chart21 <- ggplot(nra_plot_dat, aes(WtRate, Commodity_Label, fill = fill_col)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.5) +
  scale_fill_manual(values = c("Crop" = COL_CRP, "Livestock" = COL_LVS,
                                "Taxed (-)" = COL_NEG)) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(title    = "Top 15 & bottom 10 commodities by NRA rate, 2020-2024",
       subtitle = "Production-weighted NRA rate (%). Negative = net taxation of producers.") +
  theme_agri_flip()

# =============================================================================
# SAVE ALL CHARTS
# =============================================================================
charts <- list(
  "01_crop_global_nra"        = chart01,
  "02_crop_income_usd"        = chart02,
  "03_crop_region_usd"        = chart03,
  "04_crop_country_usd"       = chart04,
  "05_livestock_global_nra"   = chart06,
  "06_livestock_income_usd"   = chart07,
  "07_livestock_region_usd"   = chart08,
  "08_livestock_country_usd"  = chart09,
  "09_total_agri_usd_area"    = chart10,
  "10_total_agri_nra_area"    = chart11,
  "11_cv_global_usd"          = chart12,
  "12_cv_income_usd_panel"    = chart13,
  "13_cv_region_usd_panel"    = chart14,
  "14_cv_global_nra"          = chart15,
  "15_cv_income_nra_panel"    = chart16,
  "16_cv_region_nra_panel"    = chart17,
  "17_crop_country_nra"       = chart18,
  "18_livestock_country_nra"  = chart19,
  "19_commodity_usd"          = chart20,
  "20_commodity_nra"          = chart21
)

# Save individual PNGs (300 dpi, 10x6 inches — PowerPoint-ready)
dir.create("output_charts", showWarnings = FALSE)

for (nm in names(charts)) {
  ggsave(
    filename = file.path("output_charts", paste0(nm, ".png")),
    plot     = charts[[nm]],
    width    = 10,
    height   = 6,
    dpi      = 300,
    bg       = "white"
  )
  message("Saved: ", nm)
}

# Save all charts to a single PDF
ggsave(
  filename = "output_charts/all_charts.pdf",
  plot     = gridExtra::marrangeGrob(grobs = lapply(charts, ggplotGrob),
                                      nrow = 1, ncol = 1),
  width    = 10, height = 6
)

message("All 21 charts saved to output_charts/")
