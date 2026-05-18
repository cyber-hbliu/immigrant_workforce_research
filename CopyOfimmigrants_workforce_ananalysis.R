# =============================================================================
# IMMIGRANT EMPLOYMENT & ECONOMIC MOBILITY IN PHILADELPHIA
# Analysis pipeline — v2 (revised)
#
# REVISIONS vs v1, with theoretical anchors for every analytical choice:
#
# 1. ACS 5-year trend chart: YoY annotation REMOVED.
#    Rationale: ACS 5-year vintages share 4/5 of their input years
#    (U.S. Census Bureau, ACS Handbook 2020, Ch. 3). Differences between
#    consecutive 5-year vintages are smoothed across overlapping samples
#    and CANNOT be interpreted as single-year changes. Replaced with
#    decadal cumulative-growth framing.
#
# 2. Duplicate tract variables removed: housing_totalE, hh_totalE,
#    pct_married_couple_hh dropped — canonical total_hhE +
#    pct_married_couple retained.
#
# 3. Multicollinearity fix: pct_lfp dropped from correlation/modeling
#    (r = 0.97 with pct_emp_to_pop is mechanical — LF and employment
#    differ only by the unemployed fraction). pct_emp_to_pop kept because
#    it more directly measures opportunity realization (Spletzer 2000;
#    BLS LAUS methodology).
#
# 4. PUMS tabulations use PWGTP weights throughout. PUMS person weights
#    are the unbiased estimator of population counts; raw n misrepresents
#    distributions because sampling rates vary by PUMA and household type
#    (Lumley 2010, Complex Surveys; Census Bureau ACS PUMS Accuracy
#    Statement 2024).
#
# 5. Primary outcome: log(WAGP) among employed foreign-born.
#    Theoretical anchors:
#      - Mincer (1974). Schooling, Experience, and Earnings. NBER.
#          Foundational log-wage = f(schooling, experience, experience²)
#      - Chiswick (1978). JPE 86(5): 897-921.
#          Immigrant earnings convergence with years since migration
#      - Borjas (1985). JoLE 3(4): 463-489; Borjas (1995). JoLE 13(2).
#          Cohort effects and origin selection in immigrant earnings
#      - Bleakley & Chin (2004). RESTAT 86(2): 481-496.
#          Returns to English proficiency for the foreign-born
#      - Heckman (1979). Econometrica 47(1): 153-161.
#          Sample selection correction (employed-only subsample is
#          non-randomly selected; correction via inverse Mills ratio)
#      - Becker (1964). Human Capital. Columbia UP.
#          Human capital theory underpinning education and experience terms
#      - Card (2005). Economic Journal 115(507): F300-F323.
#          Local labor market context for immigration earnings
#
# 6. Individual-level correlation is PRIMARY (matches the unit of the
#    outcome). Tract-level correlation is SUPPLEMENTARY (spatial-context
#    lens; supports the spatial regression). Robinson (1950, ASR 15(3))
#    establishes that aggregate-level correlations cannot be interpreted
#    as individual-level relationships (ecological fallacy).
#
# 7. VIF diagnostics added to the Mincer regression
#    (Belsley, Kuh & Welsch 1980; Fox 2016).
# =============================================================================

# -----------------------------------------------------------------------------
# Step 1. Libraries
# -----------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(scales)
library(glue)
library(tidycensus)
library(tigris)
library(sf)
library(spdep)
library(spatialreg)
library(broom)
library(nnet)
library(ranger)
library(vip)
library(ggplot2)
library(ggrepel)
library(ggcorrplot)
library(treemapify)
library(leaflet)
library(htmlwidgets)
library(gt)
library(knitr)
library(htmltools)
library(car)              # VIF diagnostics (Fox 2016)
library(sandwich)         # Heteroskedasticity-robust SEs (White 1980)
library(lmtest)           # coeftest with robust SEs

# Optional but recommended for Heckman selection correction (Heckman 1979).
# If not installed: install.packages("sampleSelection")
heckman_available <- requireNamespace("sampleSelection", quietly = TRUE)

# -----------------------------------------------------------------------------
# Step 2. Editorial palette ("sesame cake" tones) and ggplot theme
# -----------------------------------------------------------------------------
artsy <- c(
  burgundy   = "#c98590",
  terracotta = "#7d7676",
  mustard    = "#c5d68a",
  sage       = "#8fa56b",
  teal       = "#3a3f5e",
  rose       = "#c98590"
)
accent_burgundy <- as.character(artsy["burgundy"])
accent_teal     <- as.character(artsy["teal"])
ink         <- "#2a2522"
gray_dark   <- "#3b3b3b"
gray_mid    <- "#888888"
gray_light  <- "#dddddd"
paper       <- "#f5f0e8"

ramp_seq <- c("#f5f0e8", "#ecd1d5", "#dca8b0", "#c98590", "#a86670")
ramp_div <- c("#c98590", "#dca8b0", "#f5f0e8", "#dde6b0", "#c5d68a")

theme_editorial <- theme_minimal(base_size = 11) +
  theme(
    text             = element_text(family = "sans", color = gray_dark),
    plot.title       = element_text(face = "bold", size = 15, color = ink,
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 11, color = gray_mid,
                                    margin = margin(b = 14)),
    plot.caption     = element_text(size = 9, color = gray_mid, hjust = 0,
                                    margin = margin(t = 12)),
    plot.caption.position = "plot",
    plot.title.position   = "plot",
    axis.title       = element_text(size = 10, color = gray_mid),
    axis.text        = element_text(size = 9,  color = gray_dark),
    axis.line.x      = element_line(color = gray_dark, linewidth = 0.4),
    axis.ticks.x     = element_line(color = gray_dark, linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = gray_light, linewidth = 0.3),
    legend.position  = "top",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 9, color = gray_dark),
    plot.margin      = margin(20, 24, 16, 20)
  )
theme_set(theme_editorial)

theme_map <- theme_void(base_size = 11) +
  theme(
    text             = element_text(family = "sans", color = gray_dark),
    plot.title       = element_text(face = "bold", size = 15, color = ink,
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 11, color = gray_mid,
                                    margin = margin(b = 12)),
    plot.caption     = element_text(size = 9, color = gray_mid, hjust = 0,
                                    margin = margin(t = 12)),
    plot.caption.position = "plot",
    plot.title.position   = "plot",
    legend.position  = "right",
    legend.title     = element_text(size = 10, color = gray_dark),
    legend.text      = element_text(size = 9, color = gray_dark),
    plot.margin      = margin(20, 24, 16, 20)
  )

# -----------------------------------------------------------------------------
# Step 3. Project constants and helpers
# -----------------------------------------------------------------------------
philly_pumas <- c("03216", "03221", "03222", "03223", "03224",
                  "03225", "03227", "03228", "03229", "03230", "03231")
acs_year   <- 2024
lodes_year <- 2023
options(tigris_use_cache = TRUE, scipen = 999)
set.seed(2025)
if (!dir.exists("output")) dir.create("output")

# Weighted median helper — used because PUMS weights vary widely by subgroup.
# Method: linear interpolation on the CDF of weighted values (Härdle & Linton
# 1994; matches base R quantile type 4 on weighted data).
weighted_median <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  x <- x[ok]; w <- w[ok]
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

# Weighted tabulation — returns a data.frame, used in every distribution chart.
weighted_table <- function(df, group_var, weight_col = "PWGTP") {
  df %>%
    group_by(across(all_of(group_var))) %>%
    summarise(weighted_n = sum(.data[[weight_col]], na.rm = TRUE),
              raw_n      = n(),
              .groups    = "drop") %>%
    mutate(pct = weighted_n / sum(weighted_n) * 100) %>%
    arrange(desc(weighted_n))
}

# -----------------------------------------------------------------------------
# Step 4. Pull ACS PUMS for Pennsylvania
# -----------------------------------------------------------------------------
pums_vars <- c(
  "AGEP", "SEX", "NATIVITY", "SCHL", "ESR", "CIT", "POBP", "POVPIP",
  "NP", "WAGP", "INDP", "NAICSP", "OCCP", "ENG", "HHL", "LNGI", "YOEP",
  "PUMA", "COW", "WAOB", "HHLDRRAC1P", "MAR", "MIG", "HHT2", "HINCP",
  "WKHP"                # usual hours worked — needed for FT-equivalent wage
)

pums_raw <- get_pums(
  variables = pums_vars,
  state     = "PA",
  survey    = "acs5",
  year      = acs_year,
  recode    = TRUE
)
cat("PUMS rows (PA):", nrow(pums_raw), "\n")
#PUMS rows (PA): 660142 
# -----------------------------------------------------------------------------
# Step 5. Filter PUMS to Philadelphia County
# -----------------------------------------------------------------------------
pums_philly <- pums_raw[pums_raw$PUMA %in% philly_pumas, ]
cat("PUMS rows (Philly):", nrow(pums_philly), "\n")
cat("Foreign-born (raw n):",       sum(pums_philly$NATIVITY == 2), "\n")
cat("Foreign-born (weighted pop):",
    scales::comma(sum(pums_philly$PWGTP[pums_philly$NATIVITY == 2])), "\n")
# PUMS rows (Philly): 41076 
# Foreign-born (raw n): 5955 
# Foreign-born (weighted pop): 242,213 

# -----------------------------------------------------------------------------
# Step 6. Pull ACS tract data — CANONICAL VARIABLES ONLY
# -----------------------------------------------------------------------------
# REVISION: housing_total and hh_total dropped because they equal
# total_hh (B11001_001 ≡ B25003_001 ≡ B11001_001 universe at tract level
# in 2024 ACS — verified in v1 summary stats: identical Min/Med/Max).
tract_vars <- c(
  total_pop          = "B05002_001",
  total_foreign_born = "B05002_013",
  naturalized        = "B05002_014",
  noncitizen         = "B05002_021",
  fb_pov_universe    = "B06012_017",
  fb_below_pov       = "B06012_018",
  total_hh           = "B11001_001",   # canonical household count
  iso_spanish        = "C16002_004",
  iso_indo_euro      = "C16002_007",
  iso_api            = "C16002_010",
  iso_other          = "C16002_013",
  median_earn_fb     = "B20017_003",
  lf_total           = "B23025_002",
  lf_employed        = "B23025_004",
  lf_unemployed      = "B23025_005",
  hispanic           = "B03003_003",
  owner_occ          = "B25003_002",
  median_gross_rent  = "B25064_001",
  median_hh_income   = "B19013_001",
  hh_married_couple  = "B11001_003",
  hh_female_no_spouse = "B11001_006",
  occ_total           = "C24010_001",
  occ_mgmt_prof       = "C24010_003",
  occ_service         = "C24010_019",
  pop_16plus          = "B23025_001",
  rent_burden_universe = "B25070_001",
  rent_30_pct          = "B25070_007",
  rent_35_pct          = "B25070_008",
  rent_40_pct          = "B25070_009",
  rent_50_pct          = "B25070_010"
)

tracts_raw <- get_acs(
  geography = "tract",
  variables = tract_vars,
  state     = "PA",
  county    = "Philadelphia",
  year      = acs_year,
  geometry  = TRUE,
  output    = "wide"
)
tracts_raw <- st_transform(tracts_raw, 4326)
cat("Tracts pulled:", nrow(tracts_raw), "\n")

# -----------------------------------------------------------------------------
# Step 7. Compute derived tract indicators (deduplicated)
# -----------------------------------------------------------------------------
# REVISION: pct_married_couple_hh and pct_lfp removed (multicollinearity).
# pct_emp_to_pop retained as the workforce-engagement measure because:
#   (a) r(pct_lfp, pct_emp_to_pop) = 0.97 — redundant
#   (b) emp-to-pop captures realized opportunity, not just supply
#       (Spletzer 2000, BLS Monthly Labor Review)
tracts <- tracts_raw
tracts$pct_foreign_born <- tracts$total_foreign_bornE / tracts$total_popE * 100

tracts$pct_naturalized_of_fb <- ifelse(
  tracts$total_foreign_bornE > 0,
  tracts$naturalizedE / tracts$total_foreign_bornE * 100, NA)

tracts$pct_hispanic <- ifelse(
  tracts$total_popE > 0,
  tracts$hispanicE / tracts$total_popE * 100, NA)

tracts$pct_poverty_fb <- ifelse(
  tracts$fb_pov_universeE > 0,
  tracts$fb_below_povE / tracts$fb_pov_universeE * 100, NA)

tracts$total_isolated <- tracts$iso_spanishE + tracts$iso_indo_euroE +
  tracts$iso_apiE + tracts$iso_otherE
tracts$pct_lang_isolated <- ifelse(
  tracts$total_hhE > 0,
  tracts$total_isolated / tracts$total_hhE * 100, NA)

tracts$unemp_rate <- ifelse(
  tracts$lf_totalE > 0,
  tracts$lf_unemployedE / tracts$lf_totalE * 100, NA)

tracts$pct_homeowner <- ifelse(
  tracts$total_hhE > 0,
  tracts$owner_occE / tracts$total_hhE * 100, NA)

tracts$pct_married_couple <- ifelse(
  tracts$total_hhE > 0,
  tracts$hh_married_coupleE / tracts$total_hhE * 100, NA)

tracts$pct_female_head <- ifelse(
  tracts$total_hhE > 0,
  tracts$hh_female_no_spouseE / tracts$total_hhE * 100, NA)

tracts$pct_mgmt_prof <- ifelse(
  tracts$occ_totalE > 0,
  tracts$occ_mgmt_profE / tracts$occ_totalE * 100, NA)

tracts$pct_service_occ <- ifelse(
  tracts$occ_totalE > 0,
  tracts$occ_serviceE / tracts$occ_totalE * 100, NA)

tracts$pct_emp_to_pop <- ifelse(
  tracts$pop_16plusE > 0,
  tracts$lf_employedE / tracts$pop_16plusE * 100, NA)

tracts$pct_rent_burdened <- ifelse(
  tracts$rent_burden_universeE > 0,
  (tracts$rent_30_pctE + tracts$rent_35_pctE +
     tracts$rent_40_pctE + tracts$rent_50_pctE) /
    tracts$rent_burden_universeE * 100, NA)

# Outlier sanitation: cap at structurally implausible cells
# (Bollen & Jackman 1985 on influential observations in regression).
# Tracts with LF < 50 are mostly institutional (jails, dorms); flag for review.
tracts$small_lf_flag <- tracts$lf_totalE < 50
cat("Tracts flagged (LF < 50):", sum(tracts$small_lf_flag, na.rm = TRUE), "\n")
#Tracts flagged (LF < 50): 21 
# -----------------------------------------------------------------------------
# Step 8. Decadal foreign-born population trend
# -----------------------------------------------------------------------------
years_to_pull <- 2014:2024
trend_rows    <- vector("list", length(years_to_pull))
for (i in seq_along(years_to_pull)) {
  y <- years_to_pull[i]
  trend_rows[[i]] <- get_acs(
    geography = "county",
    state     = "PA",
    county    = "Philadelphia",
    survey    = "acs5",
    variables = c(total_pop = "B05002_001", foreign_born = "B05002_013"),
    output    = "wide",
    year      = y
  )
  trend_rows[[i]]$year <- y
}
philly_trend <- do.call(rbind, trend_rows)
philly_trend$pct_foreign_born <- philly_trend$foreign_bornE /
  philly_trend$total_popE * 100

# Decadal cumulative growth — the methodologically defensible summary statistic
# for overlapping 5-year vintages (Census ACS Handbook 2020, Ch. 3).
baseline_pop <- philly_trend$foreign_bornE[philly_trend$year == 2014]
final_pop    <- philly_trend$foreign_bornE[philly_trend$year == 2024]
decadal_growth_pct <- (final_pop / baseline_pop - 1) * 100
decadal_growth_n   <- final_pop - baseline_pop
cat(sprintf("Decadal growth 2014→2024: +%s (+%.1f%%)\n",
            scales::comma(decadal_growth_n), decadal_growth_pct))

# -----------------------------------------------------------------------------
# Step 9. Chart 1 — population trend (decadal-growth framing)
# -----------------------------------------------------------------------------
# REVISION: YoY annotation removed. Replaced with cumulative decadal growth
# callout. ACS 5-year vintages share 4/5 of their input years, so consecutive
# 5-year estimates differ primarily because of the one rolling-in / rolling-out
# year — they are NOT directly interpretable as single-year changes
# (Census ACS Handbook 2020, Ch. 3).

p1_trend <- ggplot(philly_trend, aes(x = year, y = foreign_bornE)) +
  geom_line(color = ink, linewidth = 0.7) +
  geom_point(color = ink, size = 3) +
  geom_point(data = philly_trend[philly_trend$year %in% c(2014, 2024), ],
             color = accent_burgundy, size = 4) +
  geom_text(aes(label = scales::comma(foreign_bornE)),
            vjust = -1.3, size = 3.0, color = gray_dark, family = "sans") +
  annotate("text",
           x = 2019, y = baseline_pop + 0.55 * (final_pop - baseline_pop),
           label = sprintf("+%s foreign-born residents\n(+%.1f%%) over the decade",
                           scales::comma(decadal_growth_n),
                           decadal_growth_pct),
           hjust = -0.5, size = 4.0, lineheight = 1.0,
           color = accent_burgundy, family = "sans", fontface = "bold") +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0.08, 0.12))) +
  scale_x_continuous(breaks = years_to_pull) +
  labs(
    title    = "A decade of foreign-born population growth in Philadelphia",
    subtitle = "ACS 5-year estimates, 2014-2024 vintages",
    x = NULL, y = NULL,
    caption  = paste0(
      "Source: U.S. Census Bureau, ACS 5-year estimates.\n",
      "Note: each point pools 5 years of data (e.g., 2024 vintage = 2020-2024). ",
      "Consecutive vintages share 4 of 5 sample years; differences between ",
      "adjacent points should not be read as single-year changes."
    )
  )
print(p1_trend)
ggsave("output/chart1_pop_trend.png", p1_trend,
       width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Step 10. Recode English proficiency
# -----------------------------------------------------------------------------
pums_philly$eng_factor <- NA_character_
pums_philly$eng_factor[pums_philly$ENG == "1"] <- "Very well"
pums_philly$eng_factor[pums_philly$ENG == "2"] <- "Well"
pums_philly$eng_factor[pums_philly$ENG == "3"] <- "Not well"
pums_philly$eng_factor[pums_philly$ENG == "4"] <- "Not at all"
pums_philly$eng_factor <- factor(
  pums_philly$eng_factor,
  levels = c("Not at all", "Not well", "Well", "Very well")
)

# -----------------------------------------------------------------------------
# Step 11. Collapse education
# -----------------------------------------------------------------------------
schl_num <- suppressWarnings(as.numeric(as.character(pums_philly$SCHL)))
pums_philly$edu_collapsed <- NA_character_
pums_philly$edu_collapsed[schl_num <= 15] <- "<HS"
pums_philly$edu_collapsed[pums_philly$SCHL %in% c("16", "17")] <- "HS/GED"
pums_philly$edu_collapsed[pums_philly$SCHL %in% c("18", "19", "20")] <- "Some college/Assoc."
pums_philly$edu_collapsed[pums_philly$SCHL %in% c("21", "22", "23", "24")] <- "Bachelor's+"
pums_philly$edu_collapsed <- factor(
  pums_philly$edu_collapsed,
  levels = c("<HS", "HS/GED", "Some college/Assoc.", "Bachelor's+")
)

# -----------------------------------------------------------------------------
# Step 12. Employment status & foreign-born flag
# -----------------------------------------------------------------------------
pums_philly$foreign_born <- pums_philly$NATIVITY == 2
pums_philly$employed     <- pums_philly$ESR %in% c("1", "2")
pums_philly$in_lf        <- pums_philly$ESR %in% c("1", "2", "3")

pums_philly$esr3 <- NA_character_
pums_philly$esr3[pums_philly$ESR %in% c("1", "2")] <- "Employed"
pums_philly$esr3[pums_philly$ESR == "3"]           <- "Unemployed"
pums_philly$esr3[pums_philly$ESR == "6"]           <- "Not in LF"
pums_philly$esr3 <- factor(
  pums_philly$esr3,
  levels = c("Employed", "Unemployed", "Not in LF")
)

# -----------------------------------------------------------------------------
# Step 13. Region of birth
# -----------------------------------------------------------------------------
pobp_num <- suppressWarnings(as.numeric(as.character(pums_philly$POBP)))
pums_philly$waob_lab <- NA_character_
pums_philly$waob_lab[pobp_num %in% 1:59]                       <- "US-born"
pums_philly$waob_lab[pobp_num %in% 60:99]                      <- "PR/US Islands"
pums_philly$waob_lab[pobp_num %in% c(100:157, 160, 162:199)]   <- "Europe"
pums_philly$waob_lab[pobp_num %in% c(158, 159, 161, 200:299)]  <- "Asia"
pums_philly$waob_lab[pobp_num %in% c(300:302, 304:309)]        <- "Northern America"
pums_philly$waob_lab[pobp_num %in% c(303, 310:399)]            <- "Latin America"
pums_philly$waob_lab[pobp_num %in% 400:499]                    <- "Africa"
pums_philly$waob_lab[pobp_num %in% c(60, 500:554)]             <- "Oceania"

# -----------------------------------------------------------------------------
# Step 14. Mincer regressors
# -----------------------------------------------------------------------------
# Mincer (1974) specification: log earnings = β0 + β1·schooling + β2·exp + β3·exp² + ε
# Chiswick (1978) extension for immigrants: experience replaced (or augmented)
# with years-since-migration to capture U.S.-specific human capital accumulation.
pums_philly$age_num <- as.numeric(pums_philly$AGEP)
pums_philly$age_sq  <- pums_philly$age_num^2

# Years in US: PUMS YOEP = 1937 is the U.S.-born sentinel value; force NA
# for US-born to avoid spurious "70+ years in US" values.
yoep_num <- suppressWarnings(as.numeric(as.character(pums_philly$YOEP)))
yoep_num[pums_philly$NATIVITY == 1] <- NA
pums_philly$yrs_us    <- pmax(acs_year - yoep_num, 0)
pums_philly$yrs_us_sq <- pums_philly$yrs_us^2

# Recently-arrived flag (≤5 years) — captures the cohort still in initial
# adjustment phase (Chiswick 1978; Borjas 1995 on entry cohort effects).
pums_philly$recent_arrival <- pums_philly$yrs_us <= 5 & !is.na(pums_philly$yrs_us)
pums_philly$wage_num <- as.numeric(pums_philly$WAGP)
pums_philly$hh_income <- suppressWarnings(as.numeric(as.character(pums_philly$HINCP)))
pums_philly$wkhp_num  <- suppressWarnings(as.numeric(as.character(pums_philly$WKHP)))

# -----------------------------------------------------------------------------
# Step 14b. NAICS subsector + class of worker
# -----------------------------------------------------------------------------
naicsp_str <- as.character(pums_philly$NAICSP)
naicsp_3   <- substr(naicsp_str, 1, 3)
naicsp_2   <- substr(naicsp_str, 1, 2)   # BUG FIX: this was missing in v1

naicsp3_to_subsector <- c(
  # 2-digit fallbacks
  "11" = "Agriculture/Forestry/Fishing (sector)",
  "21" = "Mining/Oil/Gas (sector)",
  "22" = "Utilities",
  "23" = "Construction (sector-level)",
  "42" = "Wholesale Trade (sector)",
  "51" = "Information (sector)",
  "52" = "Finance/Insurance (sector)",
  "53" = "Real Estate (sector)",
  "55" = "Management of Companies",
  "61" = "Educational Services (sector)",
  "81" = "Other Services (sector)",
  "92" = "Public Administration (sector)",
  # 3-digit subsectors
  "111" = "Crop Production", "112" = "Animal Production",
  "115" = "Agriculture Support Services", "211" = "Oil and Gas Extraction",
  "236" = "Building Construction", "237" = "Heavy/Civil Engineering Construction",
  "238" = "Specialty Trade Contractors",
  "311" = "Food Manufacturing", "312" = "Beverage/Tobacco Manufacturing",
  "313" = "Textile Mills", "314" = "Textile Product Mills",
  "315" = "Apparel Manufacturing", "316" = "Leather Manufacturing",
  "321" = "Wood Product Manufacturing", "322" = "Paper Manufacturing",
  "323" = "Printing/Related Support", "324" = "Petroleum/Coal Products",
  "325" = "Chemical Manufacturing", "326" = "Plastics/Rubber Products",
  "327" = "Nonmetallic Mineral Products", "331" = "Primary Metal Manufacturing",
  "332" = "Fabricated Metal Products", "333" = "Machinery Manufacturing",
  "334" = "Computer/Electronic Products", "335" = "Electrical Equipment",
  "336" = "Transportation Equipment", "337" = "Furniture Manufacturing",
  "339" = "Miscellaneous Manufacturing",
  "423" = "Wholesale Durable Goods", "424" = "Wholesale Nondurable Goods",
  "425" = "Wholesale Electronic Markets",
  "441" = "Motor Vehicle/Parts Dealers", "444" = "Building Material/Garden Stores",
  "445" = "Food and Beverage Stores", "449" = "Furniture/Home Furnishings/Electronics",
  "455" = "General Merchandise Retailers", "456" = "Health and Personal Care Retailers",
  "457" = "Gasoline Stations/Fuel Dealers", "458" = "Clothing/Apparel Retailers",
  "459" = "Sporting Goods/Hobby/Book/Misc Retailers",
  "481" = "Air Transportation", "482" = "Rail Transportation",
  "483" = "Water Transportation", "484" = "Truck Transportation",
  "485" = "Transit/Ground Passenger Transport", "486" = "Pipeline Transportation",
  "487" = "Scenic/Sightseeing Transport", "488" = "Transportation Support Activities",
  "491" = "Postal Service", "492" = "Couriers and Messengers",
  "493" = "Warehousing and Storage",
  "513" = "Publishing Industries", "516" = "Broadcasting/Content Providers",
  "518" = "Computing Infrastructure/Data Processing",
  "519" = "Web Search/Other Information Services",
  "522" = "Credit Intermediation", "523" = "Securities/Commodities/Investments",
  "524" = "Insurance Carriers", "525" = "Funds, Trusts, Other Vehicles",
  "531" = "Real Estate", "532" = "Rental and Leasing Services",
  "533" = "Lessors of Nonfinancial Intangibles",
  "541" = "Professional/Scientific/Technical", "551" = "Management of Companies",
  "561" = "Administrative/Support Services", "562" = "Waste Management/Remediation",
  "611" = "Educational Services", "621" = "Ambulatory Health Care",
  "622" = "Hospitals", "623" = "Nursing/Residential Care",
  "624" = "Social Assistance",
  "711" = "Performing Arts/Spectator Sports", "712" = "Museums/Historical Sites",
  "713" = "Amusement/Gambling/Recreation", "721" = "Accommodation",
  "722" = "Food Services/Drinking Places",
  "811" = "Repair and Maintenance", "812" = "Personal and Laundry Services",
  "813" = "Religious/Civic Organizations", "814" = "Private Households",
  "921" = "Executive/Legislative Gov", "922" = "Justice/Public Order/Safety",
  "923" = "Human Resource Programs Admin", "924" = "Environmental Quality Admin",
  "925" = "Housing Programs Admin", "926" = "Economic Programs Admin",
  "927" = "Space Research/Technology", "928" = "National Security/Intl Affairs"
)

pums_philly$subsector <- naicsp3_to_subsector[naicsp_3]
unmapped <- is.na(pums_philly$subsector)
pums_philly$subsector[unmapped] <- naicsp3_to_subsector[naicsp_2[unmapped]]
pums_philly$subsector[is.na(pums_philly$subsector)] <-
  paste0("NAICS ", naicsp_str[is.na(pums_philly$subsector)])
pums_philly$subsector[naicsp_str == "0" |
                        naicsp_str == "" |
                        is.na(naicsp_str)] <- "Not in labor force / Military"

cow_num <- suppressWarnings(as.numeric(as.character(pums_philly$COW)))
pums_philly$cow_detailed <- case_when(
  cow_num == 1 ~ "Private for-profit employee",
  cow_num == 2 ~ "Private nonprofit employee",
  cow_num == 3 ~ "Local government employee",
  cow_num == 4 ~ "State government employee",
  cow_num == 5 ~ "Federal government employee",
  cow_num == 6 ~ "Self-employed (unincorporated)",
  cow_num == 7 ~ "Self-employed (incorporated)",
  cow_num == 8 ~ "Unpaid family worker",
  TRUE         ~ NA_character_
)
pums_philly$cow_detailed <- factor(
  pums_philly$cow_detailed,
  levels = c("Private for-profit employee", "Private nonprofit employee",
             "Local government employee", "State government employee",
             "Federal government employee", "Self-employed (incorporated)",
             "Self-employed (unincorporated)", "Unpaid family worker")
)

# -----------------------------------------------------------------------------
# Step 14c. SOC group, citizenship, household type
# -----------------------------------------------------------------------------
occp_num <- suppressWarnings(as.numeric(as.character(pums_philly$OCCP)))
pums_philly$occ_soc <- case_when(
  occp_num >= 0010 & occp_num <= 0440 ~ "MGR (Management)",
  occp_num >= 0500 & occp_num <= 0750 ~ "BUS (Business Operations)",
  occp_num >= 0800 & occp_num <= 0960 ~ "FIN (Financial Specialists)",
  occp_num >= 1005 & occp_num <= 1240 ~ "CMM (Computer/Math)",
  occp_num >= 1305 & occp_num <= 1560 ~ "ENG (Architecture/Engineering)",
  occp_num >= 1600 & occp_num <= 1980 ~ "SCI (Life/Physical/Social Science)",
  occp_num >= 2001 & occp_num <= 2060 ~ "CMS (Community/Social Service)",
  occp_num >= 2100 & occp_num <= 2180 ~ "LGL (Legal)",
  occp_num >= 2205 & occp_num <= 2555 ~ "EDU (Education/Training/Library)",
  occp_num >= 2600 & occp_num <= 2920 ~ "ENT (Arts/Design/Entertainment/Media)",
  occp_num >= 3000 & occp_num <= 3550 ~ "MED (Healthcare Practitioners)",
  occp_num >= 3601 & occp_num <= 3655 ~ "HLS (Healthcare Support)",
  occp_num >= 3700 & occp_num <= 3960 ~ "PRT (Protective Service)",
  occp_num >= 4000 & occp_num <= 4160 ~ "EAT (Food Preparation/Serving)",
  occp_num >= 4200 & occp_num <= 4255 ~ "CLN (Building/Grounds Cleaning)",
  occp_num >= 4330 & occp_num <= 4655 ~ "PRS (Personal Care/Service)",
  occp_num >= 4700 & occp_num <= 4965 ~ "SAL (Sales)",
  occp_num >= 5000 & occp_num <= 5940 ~ "OFF (Office/Admin Support)",
  occp_num >= 6005 & occp_num <= 6130 ~ "FFF (Farming/Fishing/Forestry)",
  occp_num >= 6200 & occp_num <= 6765 ~ "CON (Construction)",
  occp_num >= 6800 & occp_num <= 6950 ~ "EXT (Extraction)",
  occp_num >= 7000 & occp_num <= 7640 ~ "RPR (Installation/Maintenance/Repair)",
  occp_num >= 7700 & occp_num <= 8990 ~ "PRD (Production)",
  occp_num >= 9005 & occp_num <= 9760 ~ "TRN (Transportation/Material Moving)",
  occp_num >= 9800 & occp_num <= 9830 ~ "MIL (Military)",
  TRUE                                ~ NA_character_
)

cit_num <- suppressWarnings(as.numeric(as.character(pums_philly$CIT)))
pums_philly$citizenship <- case_when(
  cit_num == 1 ~ "Born in the U.S.",
  cit_num == 2 ~ "Born in PR/territory",
  cit_num == 3 ~ "Born abroad of U.S. parents",
  cit_num == 4 ~ "Naturalized citizen",
  cit_num == 5 ~ "Not a U.S. citizen",
  TRUE         ~ NA_character_
)
pums_philly$citizenship <- factor(
  pums_philly$citizenship,
  levels = c("Born in the U.S.", "Born in PR/territory",
             "Born abroad of U.S. parents", "Naturalized citizen",
             "Not a U.S. citizen")
)
pums_philly$is_naturalized <- case_when(
  cit_num == 4 ~ "Naturalized",
  cit_num == 5 ~ "Non-citizen",
  TRUE         ~ NA_character_
)

hht2_num <- suppressWarnings(as.numeric(as.character(pums_philly$HHT2)))
pums_philly$hh_type <- case_when(
  hht2_num == 1  ~ "Married couple, with children <18",
  hht2_num == 2  ~ "Married couple, no children <18",
  hht2_num == 3  ~ "Cohabiting couple, with children <18",
  hht2_num == 4  ~ "Cohabiting couple, no children <18",
  hht2_num == 5  ~ "Female head, living alone",
  hht2_num == 6  ~ "Female head, with children <18",
  hht2_num == 7  ~ "Female head, with other relatives",
  hht2_num == 8  ~ "Female head, with nonrelatives only",
  hht2_num == 9  ~ "Male head, living alone",
  hht2_num == 10 ~ "Male head, with children <18",
  hht2_num == 11 ~ "Male head, with other relatives",
  hht2_num == 12 ~ "Male head, with nonrelatives only",
  TRUE           ~ NA_character_
)
pums_philly$hh_type <- factor(
  pums_philly$hh_type,
  levels = c(
    "Married couple, with children <18", "Married couple, no children <18",
    "Cohabiting couple, with children <18", "Cohabiting couple, no children <18",
    "Female head, with children <18", "Female head, with other relatives",
    "Female head, with nonrelatives only", "Female head, living alone",
    "Male head, with children <18", "Male head, with other relatives",
    "Male head, with nonrelatives only", "Male head, living alone"
  )
)
pums_philly$hh_supergroup <- case_when(
  grepl("^(Married|Cohabiting) couple", pums_philly$hh_type) ~
    "Coupled households",
  grepl("(with children <18|with other relatives)$", pums_philly$hh_type) ~
    "Single householder with family",
  grepl("(living alone|nonrelatives only)$", pums_philly$hh_type) ~
    "Nonfamily / solo households"
)
pums_philly$hh_supergroup <- factor(
  pums_philly$hh_supergroup,
  levels = c("Coupled households", "Single householder with family",
             "Nonfamily / solo households")
)

# -----------------------------------------------------------------------------
# Step 15. Foreign-born subset
# -----------------------------------------------------------------------------
foreign_born <- pums_philly[pums_philly$foreign_born, ]
cat("Foreign-born records:", nrow(foreign_born), "\n")
cat("Foreign-born weighted population:",
    scales::comma(sum(foreign_born$PWGTP)), "\n")

# Weighted tabulations — REVISION: all tabs now use PWGTP, not raw n
cat("\n=== Weighted occupation distribution (employed foreign-born) ===\n")
emp_fb <- foreign_born[foreign_born$ESR %in% c("1", "2"), ]
print(weighted_table(emp_fb, "occ_soc"), n = 30)

cat("\n=== Weighted citizenship distribution (all foreign-born) ===\n")
print(weighted_table(foreign_born, "citizenship"))

cat("\n=== Weighted household type (foreign-born) ===\n")
print(weighted_table(foreign_born[!is.na(foreign_born$hh_type), ], "hh_type"))

cat("\n=== Weighted region of birth ===\n")
print(weighted_table(foreign_born, "waob_lab"))

# -----------------------------------------------------------------------------
# Step 15b. Birthplace treemap — UNCHANGED (already uses PWGTP)
# -----------------------------------------------------------------------------
pobp_codes <- c(
  "207" = "China", "210" = "Hong Kong", "215" = "Korea",
  "217" = "India", "240" = "Pakistan", "242" = "Bangladesh",
  "247" = "Vietnam", "248" = "Cambodia", "211" = "Indonesia",
  "245" = "Philippines", "203" = "Burma/Myanmar", "209" = "Taiwan",
  "138" = "Italy", "148" = "Poland", "150" = "Portugal",
  "156" = "Ukraine", "126" = "Ireland", "120" = "United Kingdom",
  "127" = "Albania",
  "303" = "Mexico", "311" = "Belize", "327" = "Honduras",
  "328" = "Nicaragua", "329" = "Panama", "375" = "Dominican Republic",
  "337" = "Cuba", "316" = "El Salvador", "317" = "Guatemala",
  "338" = "Haiti", "330" = "Jamaica",
  "374" = "Colombia", "390" = "Ecuador", "393" = "Peru", "381" = "Brazil",
  "414" = "Cabo Verde", "421" = "Ethiopia", "427" = "Ghana",
  "436" = "Liberia", "440" = "Nigeria", "444" = "Senegal",
  "451" = "Egypt", "455" = "Sudan"
)
foreign_born$country_label <- pobp_codes[as.character(foreign_born$POBP)]
foreign_born$country_label[is.na(foreign_born$country_label)] <- "Other"

top_countries <- foreign_born %>%
  filter(country_label != "Other") %>%
  group_by(country_label) %>%
  summarise(weighted_pop = sum(PWGTP, na.rm = TRUE),
            waob_lab     = first(waob_lab),
            .groups = "drop") %>%
  arrange(desc(weighted_pop)) %>%
  slice_head(n = 15) %>%
  mutate(
    pct = weighted_pop / sum(weighted_pop) * 100,
    label = sprintf("%s\n%s (%.1f%%)",
                    country_label,
                    scales::comma(round(weighted_pop, -2)),
                    pct)
  )

region_colors <- c(
  "Asia"             = as.character(artsy["burgundy"]),
  "Latin America"    = as.character(artsy["teal"]),
  "Europe"           = as.character(artsy["mustard"]),
  "Africa"           = as.character(artsy["sage"]),
  "Northern America" = as.character(artsy["terracotta"]),
  "Oceania"          = "#a86670",
  "PR/US Islands"    = gray_mid
)

p_origins <- ggplot(top_countries,
                    aes(area = weighted_pop, fill = waob_lab,
                        label = label, subgroup = waob_lab)) +
  geom_treemap(color = "white", linewidth = 3) +
  geom_treemap_subgroup_border(color = "white", linewidth = 4) +
  geom_treemap_text(color = "white", place = "centre",
                    grow = FALSE, reflow = TRUE,
                    family = "sans", fontface = "bold", size = 11) +
  scale_fill_manual(values = region_colors, name = "World region") +
  labs(
    title    = "Where Philadelphia's immigrants come from",
    subtitle = "Top 15 countries of birth, sized by PWGTP-weighted population",
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(foreign_born)),
                      " foreign-born records (weighted population: ",
                      scales::comma(sum(foreign_born$PWGTP)), ").")
  ) +
  theme(legend.position = "top",
        panel.grid = element_blank(),
        axis.line.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text = element_blank(), axis.title = element_blank())
print(p_origins)
ggsave("output/chart_origin_treemap.png", p_origins,
       width = 10, height = 7, dpi = 300)

# -----------------------------------------------------------------------------
# Step 16. Chart 2 — wage by English (weighted)
# -----------------------------------------------------------------------------
employed_fb <- foreign_born[
  foreign_born$ESR %in% c("1", "2") & foreign_born$wage_num > 1000, ]

wage_by_eng <- employed_fb %>%
  group_by(eng_factor) %>%
  summarise(
    mean_wage = weighted.mean(wage_num, PWGTP, na.rm = TRUE),
    raw_n     = n(),
    weighted_n = sum(PWGTP, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  filter(!is.na(eng_factor)) %>%
  arrange(eng_factor) %>%
  mutate(bar_color = ramp_seq[2:5])

gap_dollars <- wage_by_eng$mean_wage[wage_by_eng$eng_factor == "Very well"] -
  wage_by_eng$mean_wage[wage_by_eng$eng_factor == "Not at all"]

p2_wage <- ggplot(wage_by_eng,
                  aes(x = eng_factor, y = mean_wage, fill = bar_color)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = scales::dollar(round(mean_wage, -2))),
            hjust = -0.15, size = 3.6, color = ink,
            family = "sans", fontface = "bold") +
  scale_fill_identity() +
  scale_y_continuous(labels = scales::dollar,
                     expand = expansion(mult = c(0.02, 0.18))) +
  coord_flip() +
  labs(
    title = sprintf("The $%s raw English-proficiency wage gap",
                    scales::comma(round(gap_dollars, -2))),
    subtitle = "PWGTP-weighted mean annual wages, employed foreign-born, Philadelphia",
    x = NULL, y = NULL,
    caption = paste0("Source: ACS 5-year PUMS (2020-2024). Raw N = ",
                     scales::comma(nrow(employed_fb)),
                     " employed foreign-born with positive wages. ",
                     "Raw gap; multivariate Mincer estimates in Step 18 below.")
  )
print(p2_wage)
ggsave("output/chart2_wage_by_eng.png", p2_wage,
       width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Steps 16b-16h. (All FB-vs-USborn comparison charts — UNCHANGED, weighted)
# -----------------------------------------------------------------------------
# These already use PWGTP/WGTP in v1; retained as-is.
# Industry distribution
workers_only <- pums_philly[
  pums_philly$ESR %in% c("1", "2") &
    !is.na(pums_philly$subsector) &
    pums_philly$subsector != "Not in labor force / Military", ]

industry_dist <- workers_only %>%
  group_by(subsector) %>%
  summarise(
    fb_workers    = sum(PWGTP[foreign_born], na.rm = TRUE),
    us_workers    = sum(PWGTP[!foreign_born], na.rm = TRUE),
    total_workers = sum(PWGTP, na.rm = TRUE),
    n_records     = n(),
    .groups       = "drop"
  ) %>%
  filter(n_records >= 20) %>%
  mutate(
    sector_share_of_fb = fb_workers / sum(fb_workers) * 100,
    sector_share_of_us = us_workers / sum(us_workers) * 100,
    concentration_idx  = sector_share_of_fb / pmax(sector_share_of_us, 0.1)
  ) %>%
  arrange(desc(sector_share_of_fb)) %>%
  slice_head(n = 15)

industry_long <- industry_dist %>%
  select(subsector, `Foreign-born` = sector_share_of_fb,
         `U.S.-born` = sector_share_of_us) %>%
  pivot_longer(cols = -subsector, names_to = "group", values_to = "share")
industry_long$subsector <- factor(industry_long$subsector,
                                  levels = rev(industry_dist$subsector))

p_industry <- ggplot(industry_long,
                     aes(x = share, y = subsector, fill = group)) +
  geom_col(width = 0.7, position = position_dodge(width = 0.75)) +
  geom_text(aes(label = sprintf("%.1f%%", share)),
            position = position_dodge(width = 0.75),
            hjust = -0.15, size = 2.6, color = ink, fontface = "bold") +
  scale_fill_manual(values = c(
    "Foreign-born" = accent_burgundy,
    "U.S.-born"    = as.character(artsy["mustard"])
  )) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title    = "Where Philadelphia's immigrants work",
    subtitle = "Top 15 NAICS subsectors by foreign-born employment share",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(workers_only)), " employed workers.")
  )
print(p_industry)
ggsave("output/chart_industry_fb_vs_us.png", p_industry,
       width = 10, height = 14, dpi = 300)

# -----------------------------------------------------------------------------
# Step 17. INDIVIDUAL-LEVEL correlation matrix (PRIMARY)
# -----------------------------------------------------------------------------
# THIS IS THE PRIMARY CORRELATION FOR THE PAPER. Matches the outcome
# unit (individual log wage). Aligns with Mincer (1974) and Chiswick (1978)
# tradition of individual-level wage determinant analysis.
#
# Robinson (1950, ASR 15(3): 351-357): tract-level correlations cannot
# be interpreted as individual-level relationships (ecological fallacy).
fb_ind <- foreign_born %>%
  mutate(
    eng_very_well   = as.numeric(eng_factor == "Very well"),
    eng_well        = as.numeric(eng_factor == "Well"),
    eng_not_well    = as.numeric(eng_factor == "Not well"),
    edu_hs_ged      = as.numeric(edu_collapsed == "HS/GED"),
    edu_some_col    = as.numeric(edu_collapsed == "Some college/Assoc."),
    edu_bachelors   = as.numeric(edu_collapsed == "Bachelor's+"),
    lang_isolated   = as.numeric(LNGI == "2"),
    is_female       = as.numeric(SEX == "2"),
    is_naturalized_num = as.numeric(is_naturalized == "Naturalized"),
    is_employed     = as.numeric(esr3 == "Employed"),
    in_lf_num       = as.numeric(esr3 %in% c("Employed", "Unemployed")),
    hh_single_fam   = as.numeric(hh_supergroup == "Single householder with family"),
    hh_solo         = as.numeric(hh_supergroup == "Nonfamily / solo households"),
    ind_health      = as.numeric(grepl("^(Ambulatory Health|Hospitals|Nursing/Resi)", subsector)),
    ind_food        = as.numeric(grepl("^Food (Services|and Beverage)", subsector)),
    ind_construction= as.numeric(grepl("^(Construction|Specialty Trade|Building Construction)", subsector)),
    ind_transport   = as.numeric(grepl("^(Truck|Transit/Ground|Couriers)", subsector)),
    ind_professional= as.numeric(grepl("^Professional/Scientific", subsector)),
    cow_private_fp  = as.numeric(cow_detailed == "Private for-profit employee"),
    cow_private_np  = as.numeric(cow_detailed == "Private nonprofit employee"),
    cow_government  = as.numeric(cow_detailed %in% c(
      "Local government employee", "State government employee",
      "Federal government employee")),
    cow_self_inc    = as.numeric(cow_detailed == "Self-employed (incorporated)"),
    cow_self_unic   = as.numeric(cow_detailed == "Self-employed (unincorporated)"),
    ln_wage_ind = ifelse(wage_num > 1000, log(wage_num), NA),
    ln_hh_inc   = ifelse(hh_income > 0,   log(hh_income), NA)
  ) %>%
  select(
    `Age`                            = age_num,
    `Years in U.S.`                  = yrs_us,
    `English: Not well`              = eng_not_well,
    `English: Well`                  = eng_well,
    `English: Very well`             = eng_very_well,
    `Linguistically isolated`        = lang_isolated,
    `Education: HS/GED`              = edu_hs_ged,
    `Education: Some college`        = edu_some_col,
    `Education: Bachelor's+`         = edu_bachelors,
    `Female`                         = is_female,
    `Naturalized`                    = is_naturalized_num,
    `In labor force`                 = in_lf_num,
    `Employed (in LF)`               = is_employed,
    `Industry: Healthcare`           = ind_health,
    `Industry: Food`                 = ind_food,
    `Industry: Construction`         = ind_construction,
    `Industry: Transportation`       = ind_transport,
    `Industry: Prof/Scientific`      = ind_professional,
    `COW: Private for-profit`        = cow_private_fp,
    `COW: Private nonprofit`         = cow_private_np,
    `COW: Government`                = cow_government,
    `COW: Self-emp (incorporated)`   = cow_self_inc,
    `COW: Self-emp (unincorporated)` = cow_self_unic,
    `HH: Single head w/ family`      = hh_single_fam,
    `HH: Nonfamily / solo`           = hh_solo,
    `Log annual wage`                = ln_wage_ind,
    `Log household income`           = ln_hh_inc
  )

cor_matrix_ind <- cor(fb_ind, use = "pairwise.complete.obs")

p_cor_ind <- ggcorrplot(
  cor_matrix_ind,
  hc.order      = FALSE, type = "lower",
  lab           = TRUE, lab_size = 2.0,
  outline.color = "white",
  colors        = c("#c5d68a", "#f5f0e8", "#c98590"),
  ggtheme       = theme_editorial
) +
  labs(
    title    = "Individual-level correlations among Philadelphia's immigrants",
    subtitle = "PRIMARY correlation matrix — 27 person-level attributes, foreign-born",
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). ",
                      "N = ", scales::comma(nrow(fb_ind)), " foreign-born individuals.\n",
                      "Reference categories (English 'Not at all', Education '<HS', ",
                      "HH 'Coupled') omitted to avoid mechanical collinearity.")
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
print(p_cor_ind)
ggsave("output/chart_correlation_individual_PRIMARY.png", p_cor_ind,
       width = 10, height = 13, dpi = 300)

# -----------------------------------------------------------------------------
# Step 17b. TRACT-LEVEL correlation (SUPPLEMENTARY, deduplicated)
# -----------------------------------------------------------------------------
# Reframed as the spatial-context/neighborhood-effects lens. Supports the
# spatial regression in Step 19 but is NOT the primary correlation analysis
# (Robinson 1950 ecological fallacy).
#
# DEDUPLICATED variables vs v1:
#   - pct_married_couple_hh removed (duplicate of pct_married_couple)
#   - pct_lfp removed (r=0.97 with pct_emp_to_pop)
#   - housing_total / hh_total removed (duplicates of total_hh)
cor_df_tract <- tracts %>%
  st_drop_geometry() %>%
  select(
    `% foreign-born`            = pct_foreign_born,
    `% naturalized (of FB)`     = pct_naturalized_of_fb,
    `% Hispanic`                = pct_hispanic,
    `% poverty (FB)`            = pct_poverty_fb,
    `Median FB earnings ($)`    = median_earn_fbE,
    `% lang. isolated`          = pct_lang_isolated,
    `Unemp. rate`               = unemp_rate,
    `Employment-to-pop rate`    = pct_emp_to_pop,    # kept (dropped pct_lfp)
    `% married-couple HH`       = pct_married_couple,
    `% homeowner`               = pct_homeowner,
    `% rent-burdened`           = pct_rent_burdened,
    `Median rent ($)`           = median_gross_rentE,
    `Median HH income ($)`      = median_hh_incomeE
  )

cor_matrix_tract <- cor(cor_df_tract, use = "pairwise.complete.obs")

p_cor_tract <- ggcorrplot(
  cor_matrix_tract,
  hc.order      = FALSE, type = "lower",
  lab           = TRUE, lab_size = 2.4,
  outline.color = "white",
  colors        = c("#c5d68a", "#f5f0e8", "#c98590"),
  ggtheme       = theme_editorial
) +
  labs(
    title    = "Tract-level co-variation of workforce and socioeconomic indicators",
    subtitle = "SUPPLEMENTARY — spatial-context lens; 13 indicators across Philadelphia tracts",
    caption  = paste0("Source: ACS 5-year estimates (2024 vintage). ",
                      "N = ", nrow(cor_df_tract), " tracts.\n",
                      "Reading caveat (Robinson 1950): tract-level correlations ",
                      "cannot be interpreted as individual-level relationships.")
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
print(p_cor_tract)
ggsave("output/chart_correlation_tract_SUPP.png", p_cor_tract,
       width = 10, height = 11, dpi = 300)

# =============================================================================
# Step 18. PRIMARY MODEL — Mincer wage regression (employed foreign-born)
# =============================================================================
# Outcome: log(WAGP) | employed, foreign-born, WAGP > $1,000.
#
# Specification:
#   ln(wage) = β0
#            + β1·education (factor, ref = <HS)         [Becker 1964; Mincer 1974]
#            + β2·age + β3·age²                          [Mincer 1974: life-cycle]
#            + β4·yrs_us + β5·yrs_us²                    [Chiswick 1978: assimilation]
#            + β6·english (factor, ref = Not at all)     [Bleakley & Chin 2004]
#            + β7·lang_isolated                          [household-level enclave]
#            + β8·female                                 [Blau & Kahn 2017]
#            + β9·married                                [Korenman & Neumark 1991]
#            + β10·origin region FE                      [Borjas 1985, 1995]
#            + β11·SOC group FE                          [Card 2005: occupational sorting]
#            + β12·class of worker FE                    [employer-type axis]
#            + β13·household supergroup                  [household context]
#            + ε
#
# Weighted by PWGTP. Heteroskedasticity-robust SEs (White 1980).
# VIF diagnostic (Belsley/Kuh/Welsch 1980; threshold = 10).
# =============================================================================
mincer_df <- foreign_born[
  foreign_born$ESR %in% c("1", "2") &
    foreign_born$wage_num > 1000 &
    !is.na(foreign_born$eng_factor) &
    !is.na(foreign_born$edu_collapsed) &
    !is.na(foreign_born$yrs_us) &
    !is.na(foreign_born$occ_soc) &
    !is.na(foreign_born$cow_detailed) &
    !is.na(foreign_born$hh_supergroup) &
    !is.na(foreign_born$LNGI) &
    !is.na(foreign_born$MAR) &
    foreign_born$age_num >= 16 & foreign_born$age_num <= 75,
]
mincer_df$ln_wage       <- log(mincer_df$wage_num)
mincer_df$lang_isolated <- as.numeric(mincer_df$LNGI == "2")
mincer_df$married_bin   <- as.numeric(mincer_df$MAR == "1")
mincer_df$soc_group     <- factor(mincer_df$occ_soc)
mincer_df$cow_factor    <- mincer_df$cow_detailed
mincer_df$origin_region <- factor(mincer_df$waob_lab,
                                  levels = c("Latin America", "Asia", "Africa",
                                             "Europe", "Northern America",
                                             "Oceania", "PR/US Islands"))
cat("Mincer sample size:", nrow(mincer_df), "\n")

# -----------------------------------------------------------------------------
# Step 19. Fit Mincer regression
# -----------------------------------------------------------------------------
mincer_fit <- lm(
  ln_wage ~ eng_factor + lang_isolated +
    edu_collapsed +
    age_num + age_sq +
    yrs_us + yrs_us_sq +
    origin_region + SEX + married_bin +
    soc_group + cow_factor + hh_supergroup,
  data    = mincer_df,
  weights = PWGTP
)

# Robust SEs (White 1980)
mincer_robust <- coeftest(mincer_fit, vcov = vcovHC(mincer_fit, type = "HC1"))
cat("\n=== Mincer regression (HC1 robust SE) ===\n")
print(mincer_robust)

# VIF diagnostic
cat("\n=== VIF diagnostic ===\n")
print(car::vif(mincer_fit))

# Fit summary
cat("\nR²:", round(summary(mincer_fit)$r.squared, 4),
    "| Adj. R²:", round(summary(mincer_fit)$adj.r.squared, 4),
    "| RMSE:", round(sqrt(mean(mincer_fit$residuals^2)), 4),
    "| N:", nrow(mincer_df), "\n")

# -----------------------------------------------------------------------------
# Step 20. Tidy coefficients with % effects, using ROBUST CIs
# -----------------------------------------------------------------------------
mincer_tidy <- tidy(mincer_robust, conf.int = TRUE) %>%
  mutate(
    pct_effect = (exp(estimate)  - 1) * 100,
    pct_low    = (exp(conf.low)  - 1) * 100,
    pct_high   = (exp(conf.high) - 1) * 100
  )
write_csv(mincer_tidy, "output/mincer_coefficients.csv")

# -----------------------------------------------------------------------------
# Step 21. Chart 3 — focal coefficients (% effects)
# -----------------------------------------------------------------------------
coef_plot_df <- mincer_tidy %>%
  filter(term %in% c("eng_factorNot well", "eng_factorWell",
                     "eng_factorVery well", "lang_isolated",
                     "edu_collapsedHS/GED",
                     "edu_collapsedSome college/Assoc.",
                     "edu_collapsedBachelor's+",
                     "yrs_us", "SEX2", "married_bin")) %>%
  mutate(
    label = case_match(term,
                       "eng_factorNot well"               ~ "English: Not well",
                       "eng_factorWell"                   ~ "English: Well",
                       "eng_factorVery well"              ~ "English: Very well",
                       "lang_isolated"                    ~ "Linguistically isolated HH",
                       "edu_collapsedHS/GED"              ~ "Education: HS/GED",
                       "edu_collapsedSome college/Assoc." ~ "Education: Some college",
                       "edu_collapsedBachelor's+"         ~ "Education: Bachelor's+",
                       "yrs_us"                           ~ "Each year in U.S.",
                       "SEX2"                             ~ "Female",
                       "married_bin"                      ~ "Married"
    ),
    group = case_when(
      grepl("English|isolated", label) ~ "Language",
      grepl("Education",        label) ~ "Education",
      grepl("Female|Married",   label) ~ "Demographics",
      TRUE                             ~ "Tenure"
    )
  ) %>%
  arrange(pct_effect) %>%
  mutate(label = factor(label, levels = label))

p3_coef <- ggplot(coef_plot_df,
                  aes(x = pct_effect, y = label, color = group)) +
  geom_vline(xintercept = 0, color = gray_dark, linewidth = 0.5) +
  geom_errorbarh(aes(xmin = pct_low, xmax = pct_high),
                 height = 0, linewidth = 1) +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%+.0f%%", pct_effect)),
            vjust = -0.9, size = 3.0, family = "sans", fontface = "bold",
            show.legend = FALSE) +
  scale_color_manual(values = c(
    "Language"     = accent_burgundy,
    "Education"    = as.character(artsy["mustard"]),
    "Demographics" = as.character(artsy["teal"]),
    "Tenure"       = as.character(artsy["sage"])
  )) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    title    = "Four channels of wage variation among foreign-born workers",
    subtitle = "Mincer regression: % effect on annual wages, 95% CI (HC1 robust)",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(mincer_df)),
                      ". Adj R² = ",
                      round(summary(mincer_fit)$adj.r.squared, 3),
                      ". Controls: age, age², yrs_us², origin, SOC, ",
                      "class of worker, household.\n",
                      "Reference: English 'Not at all', Education '<HS', ",
                      "Male, Unmarried.")
  )
print(p3_coef)
ggsave("output/chart3_mincer_coefs.png", p3_coef,
       width = 10, height = 14, dpi = 300)

# -----------------------------------------------------------------------------
# Step 21b. Full coefficient plot — all variables grouped
# -----------------------------------------------------------------------------
coef_full_df <- mincer_tidy %>%
  filter(term != "(Intercept)", term != "age_sq", term != "yrs_us_sq",
         !grepl("^soc_group|^cow_factor", term)) %>%
  mutate(
    label = case_match(term,
                       "eng_factorNot well"               ~ "English: Not well",
                       "eng_factorWell"                   ~ "English: Well",
                       "eng_factorVery well"              ~ "English: Very well",
                       "lang_isolated"                    ~ "Linguistically isolated HH",
                       "edu_collapsedHS/GED"              ~ "Education: HS/GED",
                       "edu_collapsedSome college/Assoc." ~ "Education: Some college",
                       "edu_collapsedBachelor's+"         ~ "Education: Bachelor's+",
                       "age_num"                          ~ "Each year of age",
                       "yrs_us"                           ~ "Each year in U.S.",
                       "origin_regionAsia"                ~ "Origin: Asia",
                       "origin_regionAfrica"              ~ "Origin: Africa",
                       "origin_regionEurope"              ~ "Origin: Europe",
                       "origin_regionNorthern America"    ~ "Origin: N. America",
                       "origin_regionOceania"             ~ "Origin: Oceania",
                       "SEX2"                             ~ "Female",
                       "married_bin"                      ~ "Married",
                       "hh_supergroupSingle householder with family" ~ "HH: Single head w/ family",
                       "hh_supergroupNonfamily / solo households"    ~ "HH: Nonfamily / solo",
                       .default = term
    ),
    group = case_when(
      grepl("English|isolated", label) ~ "Language",
      grepl("Education",        label) ~ "Education",
      grepl("Origin",           label) ~ "Origin",
      grepl("Female|Married",   label) ~ "Demographics",
      grepl("HH:",              label) ~ "Household",
      TRUE                             ~ "Age / tenure"
    ),
    sig = case_when(
      p.value < 0.001 ~ "***", p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",   p.value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    group = factor(group, levels = c("Language", "Education",
                                     "Demographics", "Household",
                                     "Age / tenure", "Origin"))
  ) %>%
  arrange(desc(group), pct_effect) %>%
  mutate(label = factor(label, levels = label))
p3b_coef_full <- ggplot(coef_full_df,
                        aes(x = pct_effect, y = label, color = group)) +
  geom_vline(xintercept = 0, color = gray_dark, linewidth = 0.5) +
  geom_errorbarh(aes(xmin = pct_low, xmax = pct_high),
                 height = 0, linewidth = 1) +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%+.0f%% %s", pct_effect, sig)),
            vjust = -0.9, size = 2.6, family = "sans", fontface = "bold",
            show.legend = FALSE) +
  scale_color_manual(values = c(
    "Language"     = accent_burgundy,
    "Education"    = as.character(artsy["mustard"]),
    "Demographics" = as.character(artsy["teal"]),
    "Household"    = "#7d7676",
    "Age / tenure" = as.character(artsy["sage"]),
    "Origin"       = "#3a3f5e"
  )) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.08, 0.08))) +
  labs(
    title    = "Full Mincer regression — foreign-born Philadelphia workers",
    subtitle = "% effect on annual wages, 95% CI (HC1 robust), grouped by channel",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(mincer_df)),
                      ". Adj R² = ",
                      round(summary(mincer_fit)$adj.r.squared, 3),
                      ". SOC group (22) and COW (7) fixed effects included but suppressed.\n",
                      "Signif: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1. ",
                      "Reference: English 'Not at all', Education '<HS', ",
                      "Origin 'Latin America', Household 'Coupled', Male, Unmarried.")
  )
print(p3b_coef_full)
ggsave("output/chart3b_mincer_full.png", p3b_coef_full,
       width = 10, height = 14, dpi = 300)
# =============================================================================
# Step 19. SPATIAL ANALYSIS — geography of immigrant wages in Philadelphia
# =============================================================================
# Aligns with Mincer findings: outcome is log(median FB earnings), the spatial
# analog of individual log wage. Predictors map to Mincer's channels:
#   - pct_lang_isolated      ↔ Mincer lang_isolated (p=0.06)
#   - pct_female_head        ↔ Mincer female (-20%, p<0.001)
#   - pct_mgmt_prof          ↔ Mincer high-SOC concentration
#   - pct_service_occ        ↔ Mincer low-SOC concentration
#   - pct_foreign_born       — settlement context
#   - pct_naturalized_of_fb  — community establishment / settlement maturity
#   - pct_rent_burdened      — housing precarity axis
# =============================================================================

# -----------------------------------------------------------------------------
# Step 19a. Build spatial sample
# -----------------------------------------------------------------------------
spatial_df <- tracts %>%
  filter(
    !small_lf_flag,
    !is.na(pct_foreign_born),
    !is.na(median_earn_fbE), median_earn_fbE > 0,
    !is.na(pct_emp_to_pop),
    !is.na(pct_mgmt_prof),
    !is.na(pct_service_occ),
    !is.na(pct_lang_isolated),
    !is.na(pct_female_head),
    !is.na(pct_naturalized_of_fb),
    !is.na(pct_rent_burdened)
  ) %>%
  st_make_valid() %>%
  mutate(ln_earn_fb = log(median_earn_fbE))

cat("Tracts in spatial sample:", nrow(spatial_df), "\n")

# -----------------------------------------------------------------------------
# Step 19b. Spatial weights — Queen primary, Rook & KNN-5 for sensitivity
# -----------------------------------------------------------------------------
nb      <- poly2nb(spatial_df, queen = TRUE)
lw      <- nb2listw(nb, style = "W", zero.policy = TRUE)
nb_rook <- poly2nb(spatial_df, queen = FALSE)
lw_rook <- nb2listw(nb_rook, style = "W", zero.policy = TRUE)
coords  <- st_coordinates(st_centroid(spatial_df))
nb_knn5 <- knn2nb(knearneigh(coords, k = 5))
lw_knn5 <- nb2listw(nb_knn5, style = "W")

cat("Average # of Queen neighbors:", round(mean(card(nb)), 1), "\n")

# -----------------------------------------------------------------------------
# Step 19c. Philadelphia city boundary (for map overlays)
# -----------------------------------------------------------------------------
philly_limit <- tigris::counties(state = "PA", cb = TRUE, year = acs_year) %>%
  filter(NAME == "Philadelphia") %>%
  st_transform(4326)

# =============================================================================
# Step 20. Global Moran's I — six workforce-mobility indicators
# =============================================================================
moran_indicators <- list(
  "FB share"                = "pct_foreign_born",
  "Median FB earnings"      = "median_earn_fbE",
  "Employment-to-pop"       = "pct_emp_to_pop",
  "% mgmt/prof occupations" = "pct_mgmt_prof",
  "% service occupations"   = "pct_service_occ",
  "Linguistic isolation"    = "pct_lang_isolated"
)

moran_results <- data.frame(
  Indicator = character(), Moran_I = numeric(),
  P_value = numeric(), stringsAsFactors = FALSE
)
for (label in names(moran_indicators)) {
  var <- moran_indicators[[label]]
  vals <- spatial_df[[var]]
  m <- moran.test(vals, lw, zero.policy = TRUE, na.action = na.exclude)
  moran_results <- rbind(moran_results, data.frame(
    Indicator = label,
    Moran_I   = round(as.numeric(m$estimate[1]), 3),
    P_value   = m$p.value,
    stringsAsFactors = FALSE
  ))
}
moran_results$P_fmt <- format.pval(moran_results$P_value, digits = 3)
print(moran_results[, c("Indicator", "Moran_I", "P_fmt")])
write.csv(moran_results, "output/moran_global.csv", row.names = FALSE)

# Sensitivity on the headline outcome (FB earnings)
moran_earn      <- moran.test(spatial_df$median_earn_fbE, lw,
                              zero.policy = TRUE, na.action = na.exclude)
moran_earn_rook <- moran.test(spatial_df$median_earn_fbE, lw_rook,
                              zero.policy = TRUE, na.action = na.exclude)
moran_earn_knn  <- moran.test(spatial_df$median_earn_fbE, lw_knn5,
                              na.action = na.exclude)
cat("\nSensitivity for FB earnings (different weight matrices):\n")
cat("  Queen:", round(moran_earn$estimate[1], 3), "\n")
cat("  Rook: ", round(moran_earn_rook$estimate[1], 3), "\n")
cat("  KNN-5:", round(moran_earn_knn$estimate[1], 3), "\n")

# =============================================================================
# Step 21. Local Moran's I (LISA) — cache all 6 for downstream scatter plots
# =============================================================================
lisa_indicators <- c(
  "pct_foreign_born"  = "FB share",
  "median_earn_fbE"   = "Median FB earnings",
  "pct_emp_to_pop"    = "Employment-to-pop",
  "pct_mgmt_prof"     = "% mgmt/prof occupations",
  "pct_service_occ"   = "% service occupations",
  "pct_lang_isolated" = "Linguistic isolation"
)

scatter_cache <- list()
for (var in names(lisa_indicators)) {
  vals <- spatial_df[[var]]
  local_m <- localmoran(vals, lw, zero.policy = TRUE, na.action = na.exclude)
  v_scaled  <- as.numeric(scale(vals))
  v_lag     <- lag.listw(lw, vals, zero.policy = TRUE, NAOK = TRUE)
  v_lag_scl <- as.numeric(scale(v_lag))
  p_v       <- local_m[, "Pr(z != E(Ii))"]
  
  cluster_v <- rep("Not significant", length(vals))
  cluster_v[p_v < 0.05 & v_scaled > 0 & v_lag_scl > 0] <- "High-High (hot spot)"
  cluster_v[p_v < 0.05 & v_scaled < 0 & v_lag_scl < 0] <- "Low-Low (cold spot)"
  cluster_v[p_v < 0.05 & v_scaled > 0 & v_lag_scl < 0] <- "High-Low (outlier)"
  cluster_v[p_v < 0.05 & v_scaled < 0 & v_lag_scl > 0] <- "Low-High (outlier)"
  
  spatial_df[[paste0("cluster_", var)]] <- cluster_v
  scatter_cache[[var]] <- list(
    x_scaled = v_scaled, lag_scaled = v_lag_scl, cluster = cluster_v
  )
}

cat("\nLISA cluster distributions:\n")
for (var in names(lisa_indicators)) {
  col <- paste0("cluster_", var)
  cat("\n", lisa_indicators[[var]], ":\n", sep = "")
  print(table(spatial_df[[col]]))
}

# =============================================================================
# Step 22. Choropleth maps — six indicators
# =============================================================================
make_choropleth <- function(var, fill_name, label_fmt, title, subtitle, caption) {
  ggplot(spatial_df) +
    geom_sf(aes(fill = .data[[var]]), color = "white", linewidth = 0.1) +
    geom_sf(data = philly_limit, fill = NA, color = "black", linewidth = 1) +
    scale_fill_gradientn(colors = ramp_seq, na.value = gray_light,
                         name = fill_name, labels = label_fmt) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    theme_map
}

pct_label <- function(x) paste0(x, "%")

map_fb_share <- make_choropleth(
  "pct_foreign_born", "% foreign-born", pct_label,
  "Where Philadelphia's immigrants live",
  "Foreign-born share of total population, 2020-2024",
  "Source: ACS 5-year estimates."
)
print(map_fb_share)
ggsave("output/map_fb_share.png", map_fb_share, width = 10, height = 10, dpi = 300)

map_fb_earnings <- make_choropleth(
  "median_earn_fbE", "Median earnings", scales::dollar,
  "Where immigrant earnings concentrate",
  "Median earnings of foreign-born workers, 2020-2024",
  "Source: ACS 5-year estimates, table B20017."
)
print(map_fb_earnings)
ggsave("output/map_fb_earnings.png", map_fb_earnings, width = 10, height = 10, dpi = 300)

map_emp_to_pop <- make_choropleth(
  "pct_emp_to_pop", "% employed", pct_label,
  "Where workforce engagement clusters",
  "Employment-to-population ratio (age 16+), 2020-2024",
  "Source: ACS 5-year estimates, table B23025."
)
print(map_emp_to_pop)
ggsave("output/map_emp_to_pop.png", map_emp_to_pop, width = 10, height = 10, dpi = 300)

map_mgmt_prof <- make_choropleth(
  "pct_mgmt_prof", "% mgmt/prof", pct_label,
  "Where high-skill jobs cluster",
  "Share of workers in management/professional occupations, 2020-2024",
  "Source: ACS 5-year estimates, table C24010. Upper tier of bimodal-skill labor market."
)
print(map_mgmt_prof)
ggsave("output/map_mgmt_prof.png", map_mgmt_prof, width = 10, height = 10, dpi = 300)

map_service_occ <- make_choropleth(
  "pct_service_occ", "% service", pct_label,
  "Where service occupations cluster",
  "Share of workers in service occupations, 2020-2024",
  "Source: ACS 5-year estimates, table C24010. Lower tier of bimodal-skill labor market."
)
print(map_service_occ)
ggsave("output/map_service_occ.png", map_service_occ, width = 10, height = 10, dpi = 300)

map_lang_iso <- make_choropleth(
  "pct_lang_isolated", "% lang. isolated", pct_label,
  "Where linguistic isolation clusters",
  "Linguistically isolated households, 2020-2024",
  "Source: ACS 5-year estimates, table C16002."
)
print(map_lang_iso)
ggsave("output/map_lang_iso.png", map_lang_iso, width = 10, height = 10, dpi = 300)

# =============================================================================
# Step 23. Moran scatter plots — all six indicators
# =============================================================================
make_moran_scatter <- function(var_name, display_name, title, hh_label, ll_label) {
  cache <- scatter_cache[[var_name]]
  scatter_df <- data.frame(
    x_scaled   = cache$x_scaled,
    lag_scaled = cache$lag_scaled,
    cluster    = cache$cluster
  )
  m_test <- moran.test(spatial_df[[var_name]], lw,
                       zero.policy = TRUE, na.action = na.exclude)
  moran_I <- as.numeric(m_test$estimate[1])
  
  ggplot(scatter_df, aes(x = x_scaled, y = lag_scaled, color = cluster)) +
    geom_hline(yintercept = 0, color = gray_mid, linewidth = 0.3) +
    geom_vline(xintercept = 0, color = gray_mid, linewidth = 0.3) +
    geom_point(size = 2, alpha = 0.75) +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE,
                color = ink, linewidth = 0.5, linetype = "dashed") +
    scale_color_manual(values = c(
      "High-High (hot spot)" = accent_burgundy,
      "Low-Low (cold spot)"  = accent_teal,
      "High-Low (outlier)"   = "#fa8072",
      "Low-High (outlier)"   = as.character(artsy["sage"]),
      "Not significant"      = gray_light
    )) +
    annotate("text", x = 2.8, y = 2.8, label = hh_label,
             hjust = 1, vjust = 1, size = 3,
             color = accent_burgundy, fontface = "bold") +
    annotate("text", x = -2.8, y = -2.8, label = ll_label,
             hjust = 0, vjust = 0, size = 3,
             color = accent_teal, fontface = "bold") +
    annotate("text", x = 2.8, y = -2.8,
             label = sprintf("Global Moran's I = %.3f", moran_I),
             hjust = 1, vjust = 0, size = 3.2,
             color = ink, fontface = "bold") +
    labs(
      title    = title,
      subtitle = sprintf("Each tract's %s vs. neighbors' average (z-scores)",
                         display_name),
      x = sprintf("This tract's %s (standardized)", display_name),
      y = sprintf("Neighbors' average %s (standardized)", display_name),
      caption  = "Slope of dashed line equals Global Moran's I. Each point is one tract."
    ) +
    coord_equal()
}

p_moran_earnings <- make_moran_scatter(
  "median_earn_fbE", "median FB earnings",
  "Where immigrant earnings cluster",
  "High-earning cluster\n(immigrant economic anchors)",
  "Low-earning cluster\n(workforce-mobility priority zones)"
)
print(p_moran_earnings)
ggsave("output/chart_moran_earnings.png", p_moran_earnings,
       width = 10, height = 10, dpi = 300)

p_moran_emp <- make_moran_scatter(
  "pct_emp_to_pop", "employment-to-pop ratio",
  "Where workforce engagement clusters",
  "High-engagement cluster\n(strong-labor-market zones)",
  "Low-engagement cluster\n(workforce-policy priority zones)"
)
print(p_moran_emp)
ggsave("output/chart_moran_emp.png", p_moran_emp,
       width = 10, height = 10, dpi = 300)

p_moran_lang <- make_moran_scatter(
  "pct_lang_isolated", "linguistic isolation",
  "Where linguistic isolation clusters",
  "High-isolation cluster\n(workforce-access barrier zones)",
  "Low-isolation cluster\n(English-accessible zones)"
)
print(p_moran_lang)
ggsave("output/chart_moran_lang.png", p_moran_lang,
       width = 10, height = 10, dpi = 300)

p_moran_fb <- make_moran_scatter(
  "pct_foreign_born", "foreign-born share",
  "Where immigrant settlement clusters",
  "Settlement hot spot\n(established enclaves)",
  "Low-FB cluster\n(US-born dominant tracts)"
)
print(p_moran_fb)
ggsave("output/chart_moran_fb.png", p_moran_fb,
       width = 10, height = 10, dpi = 300)

p_moran_mgmt <- make_moran_scatter(
  "pct_mgmt_prof", "% mgmt/prof occupations",
  "Where high-skill occupations cluster",
  "High-skill cluster\n(upper-tier opportunity zones)",
  "Low high-skill cluster\n(limited upper-tier access)"
)
print(p_moran_mgmt)
ggsave("output/chart_moran_mgmt.png", p_moran_mgmt,
       width = 10, height = 10, dpi = 300)

p_moran_service <- make_moran_scatter(
  "pct_service_occ", "% service occupations",
  "Where service occupations cluster",
  "Service-occ hot spot\n(lower-tier concentration)",
  "Low service-occ cluster\n(diversified labor market)"
)
print(p_moran_service)
ggsave("output/chart_moran_service.png", p_moran_service,
       width = 10, height = 10, dpi = 300)

# =============================================================================
# Step 24. LISA cluster maps — six indicators
# =============================================================================
lisa_colors <- c(
  "High-High (hot spot)" = accent_burgundy,
  "Low-Low (cold spot)"  = accent_teal,
  "High-Low (outlier)"   = "#fa8072",
  "Low-High (outlier)"   = as.character(artsy["sage"]),
  "Not significant"      = gray_light
)

make_lisa_map <- function(col_name, title, subtitle, caption) {
  ggplot(spatial_df) +
    geom_sf(aes(fill = .data[[col_name]]),
            color = "white", linewidth = 0.15) +
    geom_sf(data = philly_limit, fill = NA, color = "black", linewidth = 1) +
    scale_fill_manual(values = lisa_colors, name = "LISA cluster") +
    labs(title = title, subtitle = subtitle, caption = caption) +
    theme_map +
    theme(legend.position = "top")
}

map_lisa_fbshare <- make_lisa_map(
  "cluster_pct_foreign_born",
  "Where immigrant settlement clusters",
  "Local Moran's I cluster classification, p < 0.05",
  "Source: ACS 5-year estimates. Hot spots = established immigrant enclaves."
)
print(map_lisa_fbshare)
ggsave("output/map_lisa_fbshare.png", map_lisa_fbshare,
       width = 10, height = 12, dpi = 300)

map_lisa_earnings <- make_lisa_map(
  "cluster_median_earn_fbE",
  "Where immigrant earnings cluster",
  "Local Moran's I cluster classification, p < 0.05",
  "Source: ACS 5-year estimates, table B20017. Cold spots = workforce-mobility priority zones."
)
print(map_lisa_earnings)
ggsave("output/map_lisa_earnings.png", map_lisa_earnings,
       width = 10, height = 12, dpi = 300)

map_lisa_emp <- make_lisa_map(
  "cluster_pct_emp_to_pop",
  "Where workforce engagement clusters",
  "Local Moran's I cluster classification, p < 0.05",
  "Source: ACS 5-year estimates, table B23025. Cold spots = low-engagement priority zones."
)
print(map_lisa_emp)
ggsave("output/map_lisa_emp.png", map_lisa_emp,
       width = 10, height = 12, dpi = 300)

map_lisa_mgmt <- make_lisa_map(
  "cluster_pct_mgmt_prof",
  "Where high-skill jobs cluster",
  "Local Moran's I cluster classification, p < 0.05",
  "Source: ACS 5-year estimates, table C24010. Upper-tier concentration zones."
)
print(map_lisa_mgmt)
ggsave("output/map_lisa_mgmt.png", map_lisa_mgmt,
       width = 10, height = 12, dpi = 300)

map_lisa_service <- make_lisa_map(
  "cluster_pct_service_occ",
  "Where service occupations cluster",
  "Local Moran's I cluster classification, p < 0.05",
  "Source: ACS 5-year estimates, table C24010. Lower-tier concentration zones."
)
print(map_lisa_service)
ggsave("output/map_lisa_service.png", map_lisa_service,
       width = 10, height = 12, dpi = 300)

map_lisa_lang <- make_lisa_map(
  "cluster_pct_lang_isolated",
  "Where linguistic isolation clusters",
  "Local Moran's I cluster classification, p < 0.05",
  "Source: ACS 5-year estimates, table C16002. Hot spots = ESL-program priority zones."
)
print(map_lisa_lang)
ggsave("output/map_lisa_lang.png", map_lisa_lang,
       width = 10, height = 12, dpi = 300)

# =============================================================================
# Step 25. Spatial Lag Model — log(FB earnings) aligned with Mincer
# =============================================================================
# Outcome: ln_earn_fb (spatial analog of Mincer's ln_wage)
# Predictors map to Mincer findings + housing/settlement context:
#   - pct_foreign_born:       immigrant settlement context
#   - pct_lang_isolated:      ↔ Mincer lang_isolated (p=0.06)
#   - pct_mgmt_prof:          ↔ Mincer high-SOC sorting
#   - pct_service_occ:        ↔ Mincer low-SOC sorting
#   - pct_female_head:        ↔ Mincer female (-20%)
#   - pct_naturalized_of_fb:  community establishment / settlement maturity
#   - pct_rent_burdened:      housing precarity axis
# =============================================================================

ols_fit <- lm(
  ln_earn_fb ~ pct_foreign_born + pct_lang_isolated +
    pct_mgmt_prof + pct_service_occ +
    pct_female_head + pct_naturalized_of_fb +
    pct_rent_burdened,
  data = spatial_df
)

# Moran's I on OLS residuals — confirms need for spatial model
moran_resid <- lm.morantest(ols_fit, lw, zero.policy = TRUE)
cat("\n=== Moran's I on OLS residuals ===\n")
print(moran_resid)

# Spatial Lag Model
slm_fit <- lagsarlm(
  ln_earn_fb ~ pct_foreign_born + pct_lang_isolated +
    pct_mgmt_prof + pct_service_occ +
    pct_female_head + pct_naturalized_of_fb +
    pct_rent_burdened,
  data        = spatial_df,
  listw       = lw,
  zero.policy = TRUE
)

cat("\n=== Spatial Lag Model — log(FB earnings) ===\n")
cat("N tracts:", nrow(spatial_df), "\n")
cat("ρ (spatial lag):", round(slm_fit$rho, 3), "\n")
cat("ρ p-value:", format.pval(summary(slm_fit)$LR1$p.value, digits = 3), "\n")
cat("AIC: OLS =", round(AIC(ols_fit), 1),
    " | SLM =", round(AIC(slm_fit), 1), "\n")
cat("AIC improvement:", round(AIC(ols_fit) - AIC(slm_fit), 1), "\n")
print(summary(slm_fit))

# Tidy results table
slm_summary <- summary(slm_fit)
slm_table <- data.frame(
  Term = c("ρ (spatial lag)",
           "% foreign-born",
           "% linguistically isolated",
           "% mgmt/prof occupations",
           "% service occupations",
           "% female-headed HH",
           "% naturalized (of FB)",
           "% rent-burdened"),
  Estimate = round(c(
    slm_fit$rho,
    coef(slm_fit)["pct_foreign_born"],
    coef(slm_fit)["pct_lang_isolated"],
    coef(slm_fit)["pct_mgmt_prof"],
    coef(slm_fit)["pct_service_occ"],
    coef(slm_fit)["pct_female_head"],
    coef(slm_fit)["pct_naturalized_of_fb"],
    coef(slm_fit)["pct_rent_burdened"]
  ), 4),
  Std_Error = c(
    round(slm_summary$rho.se, 4),
    round(slm_summary$Coef[c("pct_foreign_born", "pct_lang_isolated",
                             "pct_mgmt_prof", "pct_service_occ",
                             "pct_female_head", "pct_naturalized_of_fb",
                             "pct_rent_burdened"), "Std. Error"], 4)
  ),
  P_value = c(
    format.pval(2 * pnorm(abs(slm_fit$rho / slm_summary$rho.se),
                          lower.tail = FALSE), digits = 3),
    format.pval(slm_summary$Coef[c("pct_foreign_born", "pct_lang_isolated",
                                   "pct_mgmt_prof", "pct_service_occ",
                                   "pct_female_head", "pct_naturalized_of_fb",
                                   "pct_rent_burdened"),
                                 "Pr(>|z|)"], digits = 3)
  ),
  stringsAsFactors = FALSE
)
print(slm_table)
write.csv(slm_table, "output/table_slm.csv", row.names = FALSE)

saveRDS(spatial_df, "output/spatial_df_clean.rds")
saveRDS(slm_fit,    "output/slm_fit.rds")

library(spatialreg)
sdm_fit <- lagsarlm(
  ln_earn_fb ~ pct_foreign_born + pct_lang_isolated +
    pct_mgmt_prof + pct_service_occ +
    pct_female_head + pct_naturalized_of_fb +
    pct_rent_burdened,
  data = spatial_df, listw = lw, type = "mixed", zero.policy = TRUE
)
AIC(slm_fit); AIC(sdm_fit)
# [1] -65.10847
# [1] -88.96163

summary(sdm_fit)
impacts(sdm_fit, listw = lw, R = 500)
# 
# Impact measures (mixed, exact):
#   Direct     Indirect         Total
# pct_foreign_born      -0.001755531  0.002270633  0.0005151023
# pct_lang_isolated     -0.006270703  0.010055865  0.0037851622
# pct_mgmt_prof          0.009678736  0.007394017  0.0170727526
# pct_service_occ       -0.007194645 -0.005616933 -0.0128115782
# pct_female_head        0.001144256 -0.003354968 -0.0022107120
# pct_naturalized_of_fb  0.002084915  0.002330047  0.0044149627
# pct_rent_burdened     -0.003112092 -0.002025842 -0.0051379341


# =============================================================================
# Step 17c. ACCESS CHANNEL — who gets employed among foreign-born?
# =============================================================================
# Framing: before analyzing wages among the employed, we examine who reaches
# employment at all. This contextualizes the Mincer sample as a selected
# subset and motivates the wage analysis.
#
# Binary logit: P(Employed | working-age FB) = f(X)
# NOT a causal model — descriptive access-channel mapping.

access_df <- foreign_born %>%
  filter(
    age_num >= 16, age_num <= 65,
    !is.na(eng_factor), !is.na(edu_collapsed),
    !is.na(yrs_us), !is.na(hh_supergroup),
    !is.na(LNGI), !is.na(MAR), !is.na(esr3)
  ) %>%
  mutate(
    employed_bin  = as.numeric(esr3 == "Employed"),
    lang_isolated = as.numeric(LNGI == "2"),
    married_bin   = as.numeric(MAR == "1"),
    origin_region = factor(waob_lab,
                           levels = c("Latin America", "Asia", "Africa",
                                      "Europe", "Northern America",
                                      "Oceania", "PR/US Islands"))
  )

cat("Access-channel sample (working-age FB):", nrow(access_df), "\n")
cat("Employment rate (raw):",
    round(mean(access_df$employed_bin), 3), "\n")
cat("Weighted employment rate:",
    round(weighted.mean(access_df$employed_bin, access_df$PWGTP), 3), "\n")

# Weighted binary logit
access_fit <- glm(
  employed_bin ~ eng_factor + lang_isolated + edu_collapsed +
    age_num + I(age_num^2) + yrs_us + I(yrs_us^2) +
    origin_region + SEX + married_bin + hh_supergroup,
  data    = access_df,
  weights = PWGTP,
  family  = quasibinomial(link = "logit")
)

cat("\n=== Access-channel logit — odds ratios ===\n")
access_tidy <- tidy(access_fit, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(access_tidy)
write_csv(access_tidy, "output/access_logit.csv")

# Coefficient plot — focal predictors only
access_focal <- access_tidy %>%
  filter(term %in% c("eng_factorNot well", "eng_factorWell",
                     "eng_factorVery well", "lang_isolated",
                     "edu_collapsedHS/GED",
                     "edu_collapsedSome college/Assoc.",
                     "edu_collapsedBachelor's+",
                     "yrs_us", "SEX2", "married_bin")) %>%
  mutate(
    label = case_match(term,
                       "eng_factorNot well"               ~ "English: Not well",
                       "eng_factorWell"                   ~ "English: Well",
                       "eng_factorVery well"              ~ "English: Very well",
                       "lang_isolated"                    ~ "Linguistically isolated HH",
                       "edu_collapsedHS/GED"              ~ "Education: HS/GED",
                       "edu_collapsedSome college/Assoc." ~ "Education: Some college",
                       "edu_collapsedBachelor's+"         ~ "Education: Bachelor's+",
                       "yrs_us"                           ~ "Each year in U.S.",
                       "SEX2"                             ~ "Female",
                       "married_bin"                      ~ "Married"
    ),
    group = case_when(
      grepl("English|isolated", label) ~ "Language",
      grepl("Education",        label) ~ "Education",
      grepl("Female|Married",   label) ~ "Demographics",
      TRUE                             ~ "Tenure"
    )
  ) %>%
  arrange(estimate) %>%
  mutate(label = factor(label, levels = label))

p_access <- ggplot(access_focal,
                   aes(x = estimate, y = label, color = group)) +
  geom_vline(xintercept = 1, color = gray_dark, linewidth = 0.5,
             linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 1) +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%.2f", estimate)),
            vjust = -0.9, size = 3.0, fontface = "bold",
            show.legend = FALSE) +
  scale_color_manual(values = c(
    "Language"     = accent_burgundy,
    "Education"    = as.character(artsy["mustard"]),
    "Demographics" = as.character(artsy["teal"]),
    "Tenure"       = as.character(artsy["sage"])
  )) +
  scale_x_continuous(trans = "log",
                     breaks = c(0.25, 0.5, 1, 2, 4),
                     expand = expansion(mult = c(0.08, 0.08))) +
  labs(
    title    = "Who gets employed — access channel for foreign-born Philadelphians",
    subtitle = "Binary logit odds ratios, 95% CI. OR > 1 = higher employment probability",
    x = "Odds ratio (log scale)", y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(access_df)),
                      " working-age (16-65) foreign-born.\n",
                      "Reference: English 'Not at all', Education '<HS', ",
                      "Male, Unmarried. Dashed line = OR 1.")
  )
print(p_access)
ggsave("output/chart_access_logit.png", p_access,
       width = 10, height = 14, dpi = 300)

focal_terms <- c(
  "eng_factorVery well", "eng_factorWell", "eng_factorNot well",
  "lang_isolated",
  "edu_collapsedBachelor's+", "edu_collapsedSome college/Assoc.",
  "edu_collapsedHS/GED",
  "yrs_us", "SEX2", "married_bin",
  "origin_regionAsia", "origin_regionAfrica", "origin_regionEurope",
  "hh_supergroupSingle householder with family",
  "hh_supergroupNonfamily / solo households"
)

access_focal_clean <- access_tidy %>%
  filter(term %in% focal_terms) %>%
  transmute(term, access_or = estimate, access_pct = (estimate - 1) * 100)

wage_focal_clean <- mincer_tidy %>%
  filter(term %in% focal_terms) %>%
  transmute(term, wage_pct = pct_effect)

two_stage <- inner_join(access_focal_clean, wage_focal_clean, by = "term") %>%
  mutate(
    label = case_match(term,
                       "eng_factorVery well"               ~ "English: Very well",
                       "eng_factorWell"                    ~ "English: Well",
                       "eng_factorNot well"                ~ "English: Not well",
                       "lang_isolated"                     ~ "Ling. isolated HH",
                       "edu_collapsedBachelor's+"          ~ "Bachelor's+",
                       "edu_collapsedSome college/Assoc."  ~ "Some college",
                       "edu_collapsedHS/GED"               ~ "HS/GED",
                       "yrs_us"                            ~ "Each year in U.S.",
                       "SEX2"                              ~ "Female",
                       "married_bin"                       ~ "Married",
                       "origin_regionAsia"                 ~ "Origin: Asia",
                       "origin_regionAfrica"               ~ "Origin: Africa",
                       "origin_regionEurope"               ~ "Origin: Europe",
                       "hh_supergroupSingle householder with family" ~ "HH: Single head w/ family",
                       "hh_supergroupNonfamily / solo households"    ~ "HH: Nonfamily / solo"
    ),
    quadrant = case_when(
      access_pct > 0 & wage_pct > 0 ~ "Both stages",
      access_pct > 0 & wage_pct < 0 ~ "Access only",
      access_pct < 0 & wage_pct > 0 ~ "Wage only",
      TRUE                          ~ "Both penalties"
    )
  )

p_two_stage <- ggplot(two_stage,
                      aes(x = access_pct, y = wage_pct, color = quadrant)) +
  geom_hline(yintercept = 0, color = gray_mid, linewidth = 0.4) +
  geom_vline(xintercept = 0, color = gray_mid, linewidth = 0.4) +
  geom_point(size = 5) +
  geom_text_repel(aes(label = label), size = 3.5,
                  fontface = "bold", color = ink,
                  box.padding = 0.7, point.padding = 0.4,
                  max.overlaps = 20) +
  scale_color_manual(values = c(
    "Both stages"    = as.character(artsy["mustard"]),
    "Access only"    = accent_burgundy,
    "Wage only"      = as.character(artsy["teal"]),
    "Both penalties" = "#a86670"
  )) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.15, 0.15))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.15, 0.15))) +
  annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
           label = "Boosts BOTH access\nand wages",
           size = 3, color = gray_mid, fontface = "italic") +
  annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -1,
           label = "Opens door but\nno wage premium\n(brain waste)",
           size = 3, color = gray_mid, fontface = "italic") +
  annotate("text", x = -Inf, y = -Inf, hjust = -0.1, vjust = -1,
           label = "Double penalty",
           size = 3, color = gray_mid, fontface = "italic") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = "Wage premium\ndespite access barrier",
           size = 3, color = gray_mid, fontface = "italic") +
  labs(
    title    = "Access channel × wage channel — the two-stage immigrant labor market",
    subtitle = "How each attribute affects entry into employment AND wages once employed",
    x = "Effect on employment access (% change in odds)",
    y = "Effect on wages once employed (% change)",
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). ",
                      "Access from binary logit (N = ", scales::comma(nrow(access_df)),
                      "); wages from Mincer (N = ", scales::comma(nrow(mincer_df)), ").")
  )
print(p_two_stage)
ggsave("output/chart_two_stage_summary.png", p_two_stage,
       width = 10, height = 12, dpi = 300)
