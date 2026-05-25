import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import statsmodels.formula.api as smf
import matplotlib.ticker as ticker

# Define your hex color palette (6-character hex codes so they can be modified cleanly)
PALETTE_COLORS = {
    'Price support': '#1A6FBF',
    'Subsidy': '#E07B00',
    'Others': '#2AAD6F',
    'Outputs': '#C0392B',
    'Inputs': '#E07B00'
}

def apply_theme_agri(ax=None):
    """
    Applies the custom theme_agri aesthetic to a matplotlib axes object.
    Replicates the minimalist, professional look of the ggplot2 version.
    """
    if ax is None:
        ax = plt.gca()
        
    # Start with a clean white/minimal base template
    sns.set_style("white", rc={"axes.facecolor": (0, 0, 0, 0), "figure.facecolor": "white"})
    
    # 1. Panel Grids: Vertical lines blank, horizontal lines light gray
    ax.grid(True, axis='y', color='0.88', linestyle='-', linewidth=0.4, zorder=0)
    ax.grid(False, axis='x')
    
    # Remove top and right spines
    sns.despine(ax=ax, top=True, right=True, left=False, bottom=False)
    
    # 2. Axis lines, ticks, and labels
    ax.spines['left'].set_color('grey')
    ax.spines['left'].set_linewidth(0.5)
    ax.spines['bottom'].set_color('grey')
    ax.spines['bottom'].set_linewidth(0.5)
    
    ax.tick_params(axis='both', colors='grey', direction='out')
    
    # Font properties for axis labels
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_color('#333333')
        label.set_weight('bold')
        label.set_size(10)
        
    # Y-Axis Title
    ax.yaxis.label.set_size(10)
    ax.yaxis.label.set_color('#4D4D4D')
    ax.yaxis.label.set_weight('normal')
    
    # X-Axis Title Blank
    ax.set_xlabel('')
    
    # Clean up an existing legend if present (e.g. from Pandas bar plots)
    legend = ax.get_legend()
    if legend is not None:
        legend.set_title(None)
        ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.15), 
                  ncol=4, frameon=False, fontsize=10)
        
    # Adjust subplot margins tightly
    plt.subplots_adjust(top=0.95, bottom=0.2, left=0.12, right=0.95)
    
    return ax

def plot_stacked_bar(dat, x_col, metric, y_lab):
    """
    Creates a true cumulative stacked bar chart by wide-pivoting 
    the data to mirror ggplot2's geom_col() layout.
    """
    fill_vals = {
        "Price support": "#1A6FBF",
        "Subsidy": "#E07B00"
    }
    
    # Determine y-column dynamically
    y_col = "Avg_USD_bn" if metric == "usd" else "NRA_pct"
    
    # Pivot long data to wide data for proper cumulative stacking
    pivot_df = dat.pivot(index=x_col, columns="Component", values=y_col).fillna(0)
    
    # Align active columns and match color ordering
    display_cols = [c for c in ["Price support", "Subsidy"] if c in pivot_df.columns]
    pivot_df = pivot_df[display_cols]
    colors = [fill_vals[c] for c in display_cols]
    
    fig, ax = plt.subplots(figsize=(8, 5.5))
    
    # Draw native stacked bar chart
    pivot_df.plot(kind="bar", stacked=True, ax=ax, color=colors, width=0.6, zorder=2, edgecolor='none')
    
    # Add solid gray zero baseline
    ax.axhline(0, color="grey", linewidth=0.3, zorder=3)
    
    # Apply dynamic Y-axis formatting
    if metric == "usd":
        ax.yaxis.set_major_formatter(ticker.StrMethodFormatter("${x:,.0f}B"))
    else:
        ax.yaxis.set_major_formatter(ticker.PercentFormatter(xmax=100, decimals=0))
        
    ax.set_ylabel(y_lab)
    ax.set_xlabel('')
    plt.xticks(rotation=0)  # Keeps text labels horizontal like R
    
    apply_theme_agri(ax)
    
    return fig, ax

FIT_START = 2015
CV_THRESHOLD = 1.0
IMP_YEARS = [2021, 2022, 2023, 2024]

income_map = {
    "ARG": "Middle Income", "AUS": "High Income",   "BRA": "Middle Income", "BFA": "Low Income",
    "BGD": "Low Income",    "BLZ": "Middle Income", "BOL": "Middle Income", "CAN": "High Income",
    "CHE": "High Income",   "CHL": "High Income",   "CHN": "Middle Income", "COL": "Middle Income",
    "CRI": "Middle Income", "DOM": "Middle Income", "ECU": "Middle Income", "ETH": "Low Income",
    "EUR": "High Income",   "GBR": "High Income",   "GHA": "Middle Income", "IDN": "Middle Income",
    "IND": "Middle Income", "ISL": "High Income",   "ISR": "High Income",   "JPN": "High Income",
    "KAZ": "Middle Income", "KEN": "Middle Income", "KOR": "High Income",   "MEX": "Middle Income",
    "MLI": "Low Income",    "MOZ": "Low Income",    "MWI": "Low Income",    "NGA": "Middle Income",
    "NOR": "High Income",   "NZL": "High Income",   "PER": "Middle Income", "PHL": "Middle Income",
    "PRY": "Middle Income", "RUS": "Middle Income", "RWA": "Low Income",    "SEN": "Low Income",
    "SLV": "Middle Income", "TUR": "Middle Income", "TZA": "Low Income",    "UGA": "Low Income",
    "UKR": "Middle Income", "URY": "High Income",   "USA": "High Income",   "VNM": "Middle Income",
    "ZAF": "Middle Income", "ZMB": "Low Income",    "ZWE": "Low Income",    "BDI": "Low Income",
    "BEN": "Low Income"
}

region_map = { 
    "CHN": "Asia",         "IDN": "Asia",         "IND": "Asia",         "JPN": "Asia",
    "KAZ": "Asia",         "KOR": "Asia",         "PHL": "Asia",         "VNM": "Asia",
    "BGD": "Asia",         "ISR": "Asia",
    "AUS": "Oceania",      "NZL": "Oceania",
    "EUR": "Europe",       "CHE": "Europe",       "GBR": "Europe",       "ISL": "Europe",
    "NOR": "Europe",       "RUS": "Europe",       "TUR": "Europe",       "UKR": "Europe",
    "CAN": "N. America",   "USA": "N. America",
    "ARG": "Lat. America", "BRA": "Lat. America", "CHL": "Lat. America",
    "COL": "Lat. America", "CRI": "Lat. America", "MEX": "Lat. America",
    "DOM": "Lat. America", "PER": "Lat. America", "PRY": "Lat. America",
    "BOL": "Lat. America", "URY": "Lat. America", "ECU": "Lat. America",
    "SLV": "Lat. America", "BLZ": "Lat. America",
    "ETH": "Africa",       "ZAF": "Africa",       "NGA": "Africa",       "UGA": "Africa",
    "SEN": "Africa",       "MLI": "Africa",       "MOZ": "Africa",       "MWI": "Africa",
    "KEN": "Africa",       "RWA": "Africa",       "BFA": "Africa",       "BEN": "Africa",
    "TZA": "Africa",       "ZMB": "Africa",       "ZWE": "Africa",       "BDI": "Africa",
    "GHA": "Africa"
}

def impute_component(series_df, cat, target_yr, fit_start=FIT_START, cv_thr=CV_THRESHOLD):
    s = series_df.sort_values(by="Year")
    last_usd = s["Support_USD"].iloc[-1] if len(s) > 0 else 0
    last_rvp = s["RVP"].iloc[-1] if len(s) > 0 else np.nan

    if cat == "NRP":
        return pd.DataFrame({"Support_USD": [last_usd], "RVP": [last_rvp]})
        
    hist = s[s["Year"] >= fit_start]

    if len(hist) >= 5:
        y = hist["Support_USD"]
        cv = (y.std(ddof=1) / abs(y.mean())) if y.mean() != 0 else float("inf")
        if (cv > cv_thr):
            imp_usd = y.tail(3).mean()
        else:
            fit = smf.ols("Support_USD ~ Year", data=hist).fit()
            newdata = pd.DataFrame({"Year": [target_yr]})
            imp_usd = fit.predict(newdata).iloc[0]
    else:
        imp_usd = last_usd

    return pd.DataFrame({"Support_USD": [imp_usd], "RVP": [last_rvp]})

def build_53c(df, c53, agg_val):
    cats = ["NRP", "Inputs", "Outputs", "Others"]
    rep = df.query("AGGREGATION == @agg_val and Country_Code in @c53")[["Country_Code", "Year", "NRA_Cat", "Support_USD", "RVP"]]
    all_imp_dfs = []

    for code in c53:
        sub = rep[rep["Country_Code"] == code]
        rep_yrs = sub["Year"].unique()
        miss_yrs = list(set(IMP_YEARS) - set(rep_yrs))

        if len(miss_yrs) == 0:
            continue

        for yr in miss_yrs:
            for cat in cats:
                series = sub[sub["NRA_Cat"] == cat]
                vals = impute_component(series, cat, yr) 
                vals["Country_Code"] = code
                vals["Year"] = yr
                vals["NRA_Cat"] = cat
                vals = vals[["Country_Code", "Year", "NRA_Cat", "Support_USD", "RVP"]]
                all_imp_dfs.append(vals)
    
    imp_rows = pd.concat(all_imp_dfs, ignore_index=True) if all_imp_dfs else pd.DataFrame()
    final_df = pd.concat([rep, imp_rows], ignore_index=True)
    final_df["INCOME"] = final_df["Country_Code"].map(income_map)
    final_df["REGION"] = final_df["Country_Code"].map(region_map)
    return final_df

def agg_group(df, group_col, group_order):
    sub = df[(df["Year"] >= 2020) & (df["NRA_Cat"] != "NRA_Total")].dropna(subset=[group_col])
    rvp_base = (sub[sub["NRA_Cat"] == "NRP"].groupby(group_col, as_index=False)["RVP"].sum().rename(columns={"RVP": "Total_RVP"}))

    sub = (
        sub.groupby([group_col, "NRA_Cat"], as_index=False)["Support_USD"]
        .sum()
        .rename(columns={"Support_USD": "Total_USD"})
        .merge(rvp_base, on=group_col, how="left")
        .assign(Avg_USD_bn=lambda x: x["Total_USD"] / 5 / 1e9, 
                NRA_pct=lambda x: x["Total_USD"] / x["Total_RVP"] * 100,
                Component=lambda x: np.where(x["NRA_Cat"] == "NRP", "Price support", "Subsidy")
        )
    )
    sub[group_col] = pd.Categorical(sub[group_col], categories=group_order, ordered=True)
    sub = sub.groupby([group_col, "Component"], as_index=False)[["Avg_USD_bn", "NRA_pct"]].sum()
    return sub

def agg_global_annual(df):
    # Retain full historical records back to 2005 for long-term tracking
    sub = df[df["NRA_Cat"] != "NRA_Total"]
    rvp_annual = (sub[sub["NRA_Cat"] == "NRP"].groupby("Year", as_index=False)["RVP"].sum().rename(columns={"RVP": "Total_RVP"}))

    result = (
        sub.groupby(["Year", "NRA_Cat"], as_index=False)["Support_USD"]
        .sum()
        .assign(USD_bn=lambda x: x["Support_USD"] / 1e9)
        .drop(columns=["Support_USD"])
        .merge(rvp_annual, on="Year", how="left")
        .assign(
            NRA_pct=lambda x: (x["USD_bn"] * 1e9 / x["Total_RVP"]) * 100,
            NRA_Cat=lambda x: x["NRA_Cat"].replace({"NRP": "Price support"})
        )
    )
    cat_levels = ["Others", "Inputs", "Outputs", "Price support"]
    result["NRA_Cat"] = pd.Categorical(result["NRA_Cat"], categories=cat_levels, ordered=True)
    return result

def main():
    # Load data files
    crop = pd.read_csv("data/Support_crop_2026.csv")
    lvst = pd.read_csv("data/Support_livestock_2026.csv")
    totl = pd.read_csv("data/Support_total_2026.csv")
    fao = pd.read_csv("data/VoP_FAOSTAT.csv")

    def clean_ag_df(df):
        df["Support_USD"] = pd.to_numeric(df["Support_USD"], errors="coerce")
        df["RVP"] = pd.to_numeric(df["RelevantValueProduction"], errors="coerce")
        return df
    
    crop, lvst, totl = [clean_ag_df(df) for df in (crop, lvst, totl)]
    fao["VoP"] = pd.to_numeric(fao["ValueOfProduction"].str.replace(",", "", regex=False), errors="coerce")
    fao = fao[["Year", "VoP"]]    

    c53_crop = list(crop[(crop["AGGREGATION"] == "CountryXSector") & (crop["Year"]==2020)]["Country_Code"].unique())
    c53_lvst = list(lvst[(lvst["AGGREGATION"] == "CountryXSector") & (lvst["Year"]==2020)]["Country_Code"].unique())
    c53_totl = list(totl[(totl["AGGREGATION"] == "CountryXAllAg") & (totl["Year"]==2020)]["Country_Code"].unique())

    crop53 = build_53c(crop, c53_crop, "CountryXSector")
    lvst53 = build_53c(lvst, c53_lvst, "CountryXSector")
    totl53 = build_53c(totl, c53_totl, "CountryXAllAg")

    totl_ann = agg_global_annual(totl53)

    # Correctly appends transparent 'BB' channel onto clean base hex codes
    area_cols = {category: f"{hex_code}BB" for category, hex_code in PALETTE_COLORS.items()}

    # Enforce order: Bottom -> Top ("Others" on floor, "Price support" on top layer)
    breaks_order = ["Others", "Inputs", "Outputs", "Price support"]
    pivot_df = totl_ann.pivot(index="Year", columns="NRA_Cat", values="USD_bn").fillna(0)
    pivot_df = pivot_df[breaks_order].astype(float)

    x = pivot_df.index
    y_layers = [pivot_df[col] for col in breaks_order]
    colors = [area_cols[col] for col in breaks_order]

    fig, ax = plt.subplots(figsize=(9, 5.5))

    # Draw Stacked Area Chart
    ax.stackplot(x, y_layers, labels=breaks_order, colors=colors, 
                alpha=0.85, linewidth=0.25, edgecolor='white', zorder=2)

    # Baselines, shading zones, and separators
    ax.axhline(0, color="grey", linewidth=0.3, zorder=3)
    ax.axvspan(2020.5, 2024.5, color="grey", alpha=0.08, zorder=1)
    ax.axvline(2020.5, color="#666666", linewidth=0.8, linestyle="--", zorder=4)

    # Axis scales
    ax.set_xticks(np.arange(2005, 2025, 3))
    ax.set_xlim(x.min(), x.max())
    ax.yaxis.set_major_formatter(ticker.StrMethodFormatter("${x:,.0f}B"))
    ax.set_ylabel("USD billions")

    apply_theme_agri(ax)

    # Initialize layout-adjusted legend directly
    ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.15), 
              ncol=4, frameon=False, fontsize=10)

    # 🚀 REPLACED plt.show() WITH AUTOMATIC HIGH-QUALITY FILE EXPORT
    # output_filename = "total_support_area_chart.png"
    plt.savefig("output/total_support_area_chart.png", dpi=300, bbox_inches='tight')
    
    print(f"\n✨ Success! The chart has been saved directly to your folder as 'output/total_support_area_chart.png'")   

    # ── FIGURE 2: NRA RATE AREA CHART ─────────────────────────────────────────
    
    # 1. Prepare data (Pivot long to wide format for stackplot using NRA_pct)
    # This specific order ensures 'Others' sits on the bottom floor and 'Price support' stacks on top
    pivot_df_rate = totl_ann.pivot(index="Year", columns="NRA_Cat", values="NRA_pct").fillna(0)
    pivot_df_rate = pivot_df_rate[breaks_order].astype(float)

    x_rate = pivot_df_rate.index
    y_layers_rate = [pivot_df_rate[col] for col in breaks_order]
    colors_rate = [area_cols[col] for col in breaks_order]

    # 2. Initialize figure container
    fig2, ax2 = plt.subplots(figsize=(9, 5.5))

    # 3. Draw the Stacked Area Chart
    ax2.stackplot(x_rate, y_layers_rate, labels=breaks_order, colors=colors_rate, 
                 alpha=0.85, linewidth=0.25, edgecolors='white', zorder=2)

    # 4. Add visual indicators (Baselines, Shaded forecast zone, Separator line)
    ax2.axhline(0, color="grey", linewidth=0.3, zorder=3)
    ax2.axvspan(2020.5, 2024.5, color="grey", alpha=0.08, zorder=1)
    ax2.axvline(2020.5, color="#666666", linewidth=0.8, linestyle="--", zorder=4)

    # 5. Format X-axis timeline constraints
    ax2.set_xticks(np.arange(2005, 2025, 3))
    ax2.set_xlim(x_rate.min(), x_rate.max())

    # 6. Format Y-axis text as standard percentages (R: function(x) paste0(x, "%"))
    # Since NRA_pct is already scaled to 0-100 in your aggregation logic, xmax=100 fits perfectly
    ax2.yaxis.set_major_formatter(ticker.PercentFormatter(xmax=100, decimals=0))
    ax2.set_ylabel("NRA rate (%)")

    # 7. Inject custom minimalist layout rules
    apply_theme_agri(ax2)

    # 8. Align the shared single-row legend along the bottom floor
    ax2.legend(loc='upper center', bbox_to_anchor=(0.5, -0.15), 
              ncol=4, frameon=False, fontsize=10)

    # 9. Save high-resolution publication asset directly to folder
    output_filename_rate = "nra_rate_area_chart.png"
    plt.savefig("output/total_support_rate_chart.png", dpi=300, bbox_inches='tight')
    
    print(f"✨ Success! The second chart has been saved directly as 'output/total_support_rate_chart.png'")
    

if __name__ == "__main__":
    main()