# =============================================================================
# IMMIGRANT WORKFORCE OPPORTUNITY IN PHILADELPHIA — v4
#
# Changes from v3:
#   1. POBP code mapping corrected against 2020-2024 PUMS data dictionary
#      (China=207, HK=209, India=210, Korea=217, Vietnam=247, DR=329, etc.)
#   2. POBP region classification (waob_lab) revised:
#      - PR/US Islands range narrowed to {60, 66, 69, 72, 78} from 60:99
#      - Caucasus countries (Armenia 158, Azerbaijan 159, Georgia 161)
#        explicitly placed in Asia (geographic), with comment explaining
#        the deviation from Census's Europe-block default
#      - Northern America simplified to 300:301 (300=Bermuda, 301=Canada)
#   3. Industry classification switched from 3-digit subsector to 2-digit
#      NAICS sector (cleaner, larger bins, handles M-suffix PUMS codes)
#   4. Dead-code mapping entries removed (PUMS-truncated keys that never fire)
#   5. Added verification step for PUMA codes
# =============================================================================

# =============================================================================
# PART 1. SETUP & DATA PREPARATION
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
library(ggrepel)
library(ggcorrplot)
library(treemapify)
library(gt)
library(htmltools)
library(car)
library(sandwich)
library(lmtest)
library(sampleSelection)

# -----------------------------------------------------------------------------
# Step 2. Editorial palette and ggplot theme  (unchanged from v3)
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
# Step 3. Project constants
# -----------------------------------------------------------------------------
# Philadelphia County 2020-vintage PUMAs (used in 2023+ PUMS releases)
# These were verified against the 2020-2024 5-year PUMS sample.
philly_pumas <- c("03216", "03221", "03222", "03223", "03224",
                  "03225", "03227", "03228", "03229", "03230", "03231")

acs_year   <- 2024
options(tigris_use_cache = TRUE, scipen = 999)
set.seed(2025)
if (!dir.exists("output")) dir.create("output")

weighted_median <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  x <- x[ok]; w <- w[ok]
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

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
  "PUMA", "COW", "WAOB", "HHLDRRAC1P", "MAR", "MIG", "HHT2", "HINCP", "WKHP"
)

pums_raw <- get_pums(
  variables = pums_vars,
  state     = "PA",
  survey    = "acs5",
  year      = acs_year,
  recode    = TRUE
)
cat("PUMS rows (PA):", nrow(pums_raw), "\n")

# -----------------------------------------------------------------------------
# Step 5. Filter PUMS to Philadelphia County + sanity check on PUMAs
# -----------------------------------------------------------------------------
pums_philly <- pums_raw[pums_raw$PUMA %in% philly_pumas, ]

# Verification: confirm PUMAs are non-empty and yield expected total population
cat("\n=== PUMA sanity check ===\n")
puma_counts <- pums_philly %>%
  group_by(PUMA) %>%
  summarise(raw_n = n(),
            weighted_pop = sum(PWGTP, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(PUMA)
print(puma_counts)

cat("\nTotal Philadelphia weighted pop:",
    scales::comma(sum(pums_philly$PWGTP, na.rm = TRUE)), "\n")
cat("(Expected ~1.55M for 2020-2024 5-year)\n")

cat("\nPUMS rows (Philly):", nrow(pums_philly), "\n")
cat("Foreign-born (raw n):",
    sum(pums_philly$NATIVITY == 2), "\n")
cat("Foreign-born (weighted pop):",
    scales::comma(sum(pums_philly$PWGTP[pums_philly$NATIVITY == 2])), "\n")

# -----------------------------------------------------------------------------
# Step 6. Pull ACS tract data  (unchanged from v3)
# -----------------------------------------------------------------------------
tract_vars <- c(
  total_pop          = "B05002_001",
  total_foreign_born = "B05002_013",
  naturalized        = "B05002_014",
  noncitizen         = "B05002_021",
  fb_pov_universe    = "B06012_017",
  fb_below_pov       = "B06012_018",
  total_hh           = "B11001_001",
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
# Step 7. Compute derived tract indicators  (unchanged from v3)
# -----------------------------------------------------------------------------
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

tracts$small_lf_flag <- tracts$lf_totalE < 50
cat("Tracts flagged (LF < 50):", sum(tracts$small_lf_flag, na.rm = TRUE), "\n")

# -----------------------------------------------------------------------------
# Step 8. Recode individual-level PUMS variables
# -----------------------------------------------------------------------------
# English, education, employment, age, tenure, wages — unchanged from v3
pums_philly$eng_factor <- NA_character_
pums_philly$eng_factor[pums_philly$ENG == "1"] <- "Very well"
pums_philly$eng_factor[pums_philly$ENG == "2"] <- "Well"
pums_philly$eng_factor[pums_philly$ENG == "3"] <- "Not well"
pums_philly$eng_factor[pums_philly$ENG == "4"] <- "Not at all"
pums_philly$eng_factor <- factor(
  pums_philly$eng_factor,
  levels = c("Not at all", "Not well", "Well", "Very well"))

schl_num <- suppressWarnings(as.numeric(as.character(pums_philly$SCHL)))
pums_philly$edu_collapsed <- NA_character_
pums_philly$edu_collapsed[schl_num <= 15] <- "<HS"
pums_philly$edu_collapsed[pums_philly$SCHL %in% c("16", "17")] <- "HS/GED"
pums_philly$edu_collapsed[pums_philly$SCHL %in% c("18", "19", "20")] <- "Some college/Assoc."
pums_philly$edu_collapsed[pums_philly$SCHL %in% c("21", "22", "23", "24")] <- "Bachelor's+"
pums_philly$edu_collapsed <- factor(
  pums_philly$edu_collapsed,
  levels = c("<HS", "HS/GED", "Some college/Assoc.", "Bachelor's+"))

pums_philly$foreign_born <- pums_philly$NATIVITY == 2
pums_philly$employed     <- pums_philly$ESR %in% c("1", "2")
pums_philly$in_lf        <- pums_philly$ESR %in% c("1", "2", "3")
pums_philly$esr3 <- NA_character_
pums_philly$esr3[pums_philly$ESR %in% c("1", "2")] <- "Employed"
pums_philly$esr3[pums_philly$ESR == "3"]           <- "Unemployed"
pums_philly$esr3[pums_philly$ESR == "6"]           <- "Not in LF"
pums_philly$esr3 <- factor(
  pums_philly$esr3,
  levels = c("Employed", "Unemployed", "Not in LF"))

# -----------------------------------------------------------------------------
# REGION OF BIRTH (waob_lab) — REVISED for v4
# -----------------------------------------------------------------------------
# Following 2020-2024 PUMS POBP dictionary structure:
#   001-056  US states
#   060-078  US territories/outlying areas (Am. Samoa, Guam, CNMI, PR, USVI)
#   100-169  Europe + Caucasus (Census's "Europe" block)
#   200-254  Asia (East/South/Southeast/West Asia)
#   300-302  Northern America (Bermuda, Canada)
#   303      Mexico
#   310-344  Central America + Caribbean
#   360-399  South America
#   400-499  Africa
#   500-554  Oceania
#
# Deviation from Census default: Armenia (158), Azerbaijan (159), and
# Georgia (161) are placed in Asia (Caucasus is geographically Asia, and
# their migration patterns to Philadelphia align with Central Asian rather
# than European flows). Census places them in the 100-169 "Europe" block by
# legacy convention.
# -----------------------------------------------------------------------------
pobp_num <- suppressWarnings(as.numeric(as.character(pums_philly$POBP)))

pums_philly$waob_lab <- NA_character_

# US-born (50 states + DC)
pums_philly$waob_lab[pobp_num >= 1 & pobp_num <= 56] <- "US-born"

# US territories/outlying areas (specific codes only, not 60:99 range)
pums_philly$waob_lab[pobp_num %in% c(60, 66, 69, 72, 78)] <- "PR/US Islands"

# Europe (excluding Caucasus 158, 159, 161 which go to Asia)
pums_philly$waob_lab[pobp_num %in% c(100:157, 160, 162:169)] <- "Europe"

# Asia (200-254 + Caucasus countries)
pums_philly$waob_lab[pobp_num %in% c(158, 159, 161, 200:254)] <- "Asia"

# Northern America (Bermuda, Canada — 300, 301; 302 is unused)
pums_philly$waob_lab[pobp_num %in% c(300, 301)] <- "Northern America"

# Latin America (Mexico 303 + Central America 310-316 + Caribbean 321-344
# + South America 360-399)
pums_philly$waob_lab[pobp_num %in% c(303, 310:399)] <- "Latin America"

# Africa
pums_philly$waob_lab[pobp_num %in% c(400:499)] <- "Africa"

# Oceania (500-554; American Samoa 60 stays in PR/US Islands)
pums_philly$waob_lab[pobp_num %in% c(500:554)] <- "Oceania"

# Verification
cat("\n=== POBP region classification ===\n")
region_check <- pums_philly %>%
  filter(foreign_born) %>%
  group_by(waob_lab) %>%
  summarise(raw_n = n(),
            weighted_n = sum(PWGTP, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(pct = weighted_n / sum(weighted_n) * 100) %>%
  arrange(desc(weighted_n))
print(region_check)

# Mincer regressors  (unchanged)
pums_philly$age_num <- as.numeric(pums_philly$AGEP)
pums_philly$age_sq  <- pums_philly$age_num^2

yoep_num <- suppressWarnings(as.numeric(as.character(pums_philly$YOEP)))
yoep_num[pums_philly$NATIVITY == 1] <- NA
pums_philly$yrs_us    <- pmax(acs_year - yoep_num, 0)
pums_philly$yrs_us_sq <- pums_philly$yrs_us^2
pums_philly$recent_arrival <- pums_philly$yrs_us <= 5 & !is.na(pums_philly$yrs_us)

pums_philly$wage_num  <- as.numeric(pums_philly$WAGP)
pums_philly$hh_income <- suppressWarnings(as.numeric(as.character(pums_philly$HINCP)))
pums_philly$wkhp_num  <- suppressWarnings(as.numeric(as.character(pums_philly$WKHP)))

# -----------------------------------------------------------------------------
# Step 9. NAICS 2-digit SECTOR (revised from 3-digit subsector in v3)
# -----------------------------------------------------------------------------
# Switched to 2-digit NAICS sectors for cleaner aggregation:
#   - Avoids the M-suffix problem (PUMS's 92M1, 22S, 33M, 4MS, 42S codes)
#   - Larger bins per sector → more stable estimates with N=2,420 employed FB
#   - Standard NAICS 2-digit sectors are internationally comparable
#
# Note: Three NAICS sectors split across two 2-digit codes — we merge them
# back to a single conceptual sector:
#   31, 32, 33  →  Manufacturing
#   44, 45      →  Retail Trade
#   48, 49      →  Transportation & Warehousing
# (This follows standard BLS/Census practice for sector-level reporting.)
# -----------------------------------------------------------------------------
naicsp_str <- as.character(pums_philly$NAICSP)
naicsp_2   <- substr(naicsp_str, 1, 2)

naicsp2_to_sector <- c(
  "11" = "Agriculture, Forestry, Fishing & Hunting",
  "21" = "Mining, Quarrying, Oil & Gas Extraction",
  "22" = "Utilities",
  "23" = "Construction",
  "31" = "Manufacturing",
  "32" = "Manufacturing",
  "33" = "Manufacturing",
  "3M" = "Manufacturing",         # PUMS's "Not Specified Manufacturing"
  "42" = "Wholesale Trade",
  "44" = "Retail Trade",
  "45" = "Retail Trade",
  "4M" = "Retail Trade",          # PUMS's "Not Specified Retail Trade"
  "48" = "Transportation & Warehousing",
  "49" = "Transportation & Warehousing",
  "51" = "Information",
  "52" = "Finance & Insurance",
  "53" = "Real Estate, Rental & Leasing",
  "54" = "Professional, Scientific & Technical Services",
  "55" = "Management of Companies",
  "56" = "Administrative & Support / Waste Management",
  "61" = "Educational Services",
  "62" = "Health Care & Social Assistance",
  "71" = "Arts, Entertainment & Recreation",
  "72" = "Accommodation & Food Services",
  "81" = "Other Services",
  "92" = "Public Administration"
)

pums_philly$sector <- naicsp2_to_sector[naicsp_2]

# Handle "Not in labor force / Military / Unmapped"
pums_philly$sector[is.na(pums_philly$sector) &
                     (naicsp_str == "0" | naicsp_str == "" |
                        is.na(naicsp_str))] <- "Not in labor force / Military"

# Anything still unmapped after sector assignment — log it
unmapped_str <- naicsp_str[is.na(pums_philly$sector) &
                             !is.na(naicsp_str) & naicsp_str != "" & naicsp_str != "0"]
if (length(unmapped_str) > 0) {
  cat("\nWarning: unmapped NAICSP codes (top 10):\n")
  print(head(sort(table(unmapped_str), decreasing = TRUE), 10))
  pums_philly$sector[is.na(pums_philly$sector) &
                       !is.na(naicsp_str) & naicsp_str != "" & naicsp_str != "0"] <-
    paste0("Unmapped NAICS ", naicsp_str[is.na(pums_philly$sector) &
                                           !is.na(naicsp_str) & naicsp_str != "" & naicsp_str != "0"])
}

# Class of worker (COW) — unchanged
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
             "Self-employed (unincorporated)", "Unpaid family worker"))

# SOC occupation group — unchanged
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

# Citizenship — unchanged
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
             "Not a U.S. citizen"))

pums_philly$is_naturalized <- case_when(
  cit_num == 4 ~ "Naturalized",
  cit_num == 5 ~ "Non-citizen",
  TRUE         ~ NA_character_
)

# Household type — unchanged
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
  ))

pums_philly$hh_supergroup <- case_when(
  grepl("^(Married|Cohabiting) couple", pums_philly$hh_type) ~ "Coupled households",
  grepl("(with children <18|with other relatives)$", pums_philly$hh_type) ~ "Single householder with family",
  grepl("(living alone|nonrelatives only)$", pums_philly$hh_type) ~ "Nonfamily / solo households"
)
pums_philly$hh_supergroup <- factor(
  pums_philly$hh_supergroup,
  levels = c("Coupled households", "Single householder with family",
             "Nonfamily / solo households"))

# -----------------------------------------------------------------------------
# Step 10. Foreign-born subset
# -----------------------------------------------------------------------------
foreign_born <- pums_philly[pums_philly$foreign_born, ]
cat("Foreign-born records:", nrow(foreign_born), "\n")
cat("Foreign-born weighted population:",
    scales::comma(sum(foreign_born$PWGTP)), "\n")

# =============================================================================
# PART 2. DESCRIPTIVE FOUNDATIONS
# =============================================================================

# -----------------------------------------------------------------------------
# Step 11. Decadal foreign-born population trend  (unchanged from v3)
# -----------------------------------------------------------------------------
years_to_pull <- 2014:2024
trend_rows    <- vector("list", length(years_to_pull))

for (i in seq_along(years_to_pull)) {
  y <- years_to_pull[i]
  trend_rows[[i]] <- get_acs(
    geography = "county", state = "PA", county = "Philadelphia",
    survey = "acs5",
    variables = c(total_pop = "B05002_001", foreign_born = "B05002_013"),
    output = "wide", year = y
  )
  trend_rows[[i]]$year <- y
}
philly_trend <- do.call(rbind, trend_rows)
philly_trend$pct_foreign_born <- philly_trend$foreign_bornE /
  philly_trend$total_popE * 100

baseline_pop <- philly_trend$foreign_bornE[philly_trend$year == 2014]
final_pop    <- philly_trend$foreign_bornE[philly_trend$year == 2024]
decadal_growth_pct <- (final_pop / baseline_pop - 1) * 100
decadal_growth_n   <- final_pop - baseline_pop

# Print values for paper writing reference
cat("\n=== Decadal trend for paper introduction ===\n")
cat("2014 FB pop:", scales::comma(baseline_pop), "\n")
cat("2024 FB pop:", scales::comma(final_pop), "\n")
cat("Decadal growth pct:", round(decadal_growth_pct, 1), "%\n")
cat("Decadal growth n:", scales::comma(decadal_growth_n), "\n")

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
                           scales::comma(decadal_growth_n), decadal_growth_pct),
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
      "Note: each point pools 5 years of data; consecutive vintages share ",
      "4 of 5 sample years."
    )
  )
print(p1_trend)
ggsave("output/chart1_pop_trend.png", p1_trend,
       width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Step 12. Birthplace treemap — REVISED pobp_codes (dictionary-verified)
# -----------------------------------------------------------------------------
# All codes verified against 2020-2024 PUMS POBP dictionary.
# -----------------------------------------------------------------------------
pobp_codes <- c(
  # Asia
  "200" = "Afghanistan",  "202" = "Bangladesh",   "203" = "Bhutan",
  "205" = "Myanmar",      "206" = "Cambodia",     "207" = "China",
  "209" = "Hong Kong",    "210" = "India",        "211" = "Indonesia",
  "212" = "Iran",         "213" = "Iraq",         "214" = "Israel",
  "215" = "Japan",        "216" = "Jordan",       "217" = "Korea",
  "218" = "Kazakhstan",   "219" = "Kyrgyzstan",   "222" = "Kuwait",
  "223" = "Laos",         "224" = "Lebanon",      "226" = "Malaysia",
  "228" = "Mongolia",     "229" = "Nepal",        "231" = "Pakistan",
  "233" = "Philippines",  "235" = "Saudi Arabia", "236" = "Singapore",
  "238" = "Sri Lanka",    "239" = "Syria",        "240" = "Taiwan",
  "242" = "Thailand",     "243" = "Turkey",       "245" = "United Arab Emirates",
  "246" = "Uzbekistan",   "247" = "Vietnam",      "248" = "Yemen",
  # Caucasus (geographically Asia, in Census's 100-169 block)
  "158" = "Armenia",      "159" = "Azerbaijan",   "161" = "Georgia",
  # Europe
  "100" = "Albania",      "102" = "Austria",      "103" = "Belgium",
  "104" = "Bulgaria",     "105" = "Czechoslovakia", "106" = "Denmark",
  "108" = "Finland",      "109" = "France",       "110" = "Germany",
  "116" = "Greece",       "117" = "Hungary",      "118" = "Iceland",
  "119" = "Ireland",      "120" = "Italy",        "126" = "Netherlands",
  "127" = "Norway",       "128" = "Poland",       "129" = "Portugal",
  "130" = "Azores Islands", "132" = "Romania",    "134" = "Spain",
  "136" = "Sweden",       "137" = "Switzerland",  "138" = "United Kingdom",
  "139" = "England",      "140" = "Scotland",     "142" = "Northern Ireland",
  "147" = "Yugoslavia",   "148" = "Czech Republic", "149" = "Slovakia",
  "150" = "Bosnia and Herzegovina", "151" = "Croatia", "152" = "Macedonia",
  "154" = "Serbia",       "156" = "Latvia",       "157" = "Lithuania",
  "160" = "Belarus",      "162" = "Moldova",      "163" = "Russia",
  "164" = "Ukraine",      "167" = "Kosovo",       "168" = "Montenegro",
  # Americas (Northern America, Central America, Caribbean, South America)
  "300" = "Bermuda",      "301" = "Canada",       "303" = "Mexico",
  "310" = "Belize",       "311" = "Costa Rica",   "312" = "El Salvador",
  "313" = "Guatemala",    "314" = "Honduras",     "315" = "Nicaragua",
  "316" = "Panama",
  "321" = "Antigua and Barbuda", "323" = "Bahamas", "324" = "Barbados",
  "327" = "Cuba",         "328" = "Dominica",     "329" = "Dominican Republic",
  "330" = "Grenada",      "332" = "Haiti",        "333" = "Jamaica",
  "338" = "St. Kitts-Nevis", "339" = "St. Lucia",
  "340" = "St. Vincent and the Grenadines", "341" = "Trinidad and Tobago",
  "360" = "Argentina",    "361" = "Bolivia",      "362" = "Brazil",
  "363" = "Chile",        "364" = "Colombia",     "365" = "Ecuador",
  "368" = "Guyana",       "369" = "Paraguay",     "370" = "Peru",
  "372" = "Uruguay",      "373" = "Venezuela",
  # Africa
  "400" = "Algeria",      "407" = "Cameroon",     "408" = "Cabo Verde",
  "412" = "Congo",        "414" = "Egypt",        "416" = "Ethiopia",
  "417" = "Eritrea",      "420" = "Gambia",       "421" = "Ghana",
  "423" = "Guinea",       "425" = "Ivory Coast",  "427" = "Kenya",
  "429" = "Liberia",      "430" = "Libya",        "436" = "Morocco",
  "440" = "Nigeria",      "442" = "Rwanda",       "444" = "Senegal",
  "447" = "Sierra Leone", "448" = "Somalia",      "449" = "South Africa",
  "451" = "Sudan",        "453" = "Tanzania",     "454" = "Togo",
  "457" = "Uganda",       "459" = "Democratic Republic of Congo (Zaire)",
  "460" = "Zambia",       "461" = "Zimbabwe"
)

foreign_born$country_label <- pobp_codes[as.character(foreign_born$POBP)]
foreign_born$country_label[is.na(foreign_born$country_label)] <- "Other"

# Diagnostic: print top raw POBP codes BEFORE mapping (sanity check)
cat("\n=== Top 20 raw POBP codes (foreign-born sample) ===\n")
foreign_born %>%
  group_by(POBP) %>%
  summarise(weighted_pop = sum(PWGTP, na.rm = TRUE),
            mapped_label = first(country_label),
            .groups = "drop") %>%
  arrange(desc(weighted_pop)) %>%
  head(20) %>%
  print()

top_countries <- foreign_born %>%
  filter(country_label != "Other") %>%
  group_by(country_label) %>%
  summarise(weighted_pop = sum(PWGTP, na.rm = TRUE),
            waob_lab     = first(waob_lab), .groups = "drop") %>%
  arrange(desc(weighted_pop)) %>%
  slice_head(n = 15) %>%
  mutate(
    pct = weighted_pop / sum(weighted_pop) * 100,
    label = sprintf("%s\n%s (%.1f%%)",
                    country_label,
                    scales::comma(round(weighted_pop, -2)), pct)
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
                      " FB records (weighted: ",
                      scales::comma(sum(foreign_born$PWGTP)), ").")
  ) +
  theme(legend.position = "top",
        panel.grid = element_blank(),
        axis.line.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text = element_blank(), axis.title = element_blank())
print(p_origins)
ggsave("output/chart2_origin_treemap.png", p_origins,
       width = 10, height = 7, dpi = 300)

# -----------------------------------------------------------------------------
# Step 13. Raw English-wage gap  (unchanged from v3)
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
            hjust = -0.15, size = 3.6, color = ink, fontface = "bold") +
  scale_fill_identity() +
  scale_y_continuous(labels = scales::dollar,
                     expand = expansion(mult = c(0.02, 0.18))) +
  coord_flip() +
  labs(
    title = sprintf("The $%s raw English-proficiency wage gap",
                    scales::comma(round(gap_dollars, -2))),
    subtitle = "PWGTP-weighted mean annual wages, employed foreign-born",
    x = NULL, y = NULL,
    caption = paste0("Source: ACS 5-year PUMS (2020-2024). Raw N = ",
                     scales::comma(nrow(employed_fb)),
                     " employed FB with positive wages. Multivariate ",
                     "estimates in Stage B below.")
  )
print(p2_wage)
ggsave("output/chart3_wage_by_eng.png", p2_wage,
       width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Step 14. Industry distribution — 2-DIGIT SECTOR version (revised)
# -----------------------------------------------------------------------------
workers_only <- pums_philly[
  pums_philly$ESR %in% c("1", "2") &
    !is.na(pums_philly$sector) &
    pums_philly$sector != "Not in labor force / Military", ]

industry_dist <- workers_only %>%
  group_by(sector) %>%
  summarise(
    fb_workers    = sum(PWGTP[foreign_born], na.rm = TRUE),
    us_workers    = sum(PWGTP[!foreign_born], na.rm = TRUE),
    n_records     = n(),
    .groups       = "drop"
  ) %>%
  filter(n_records >= 20) %>%
  mutate(
    sector_share_of_fb = fb_workers / sum(fb_workers) * 100,
    sector_share_of_us = us_workers / sum(us_workers) * 100,
    fb_vs_us_ratio     = sector_share_of_fb / sector_share_of_us
  ) %>%
  arrange(desc(sector_share_of_fb))

# Save for reference / paper Section 3
cat("\n=== Industry distribution by sector (2-digit NAICS) ===\n")
print(industry_dist)
write_csv(industry_dist, "output/sector_distribution.csv")

industry_long <- industry_dist %>%
  select(sector, `Foreign-born` = sector_share_of_fb,
         `U.S.-born` = sector_share_of_us) %>%
  pivot_longer(cols = -sector, names_to = "group", values_to = "share")

industry_long$sector <- factor(industry_long$sector,
                               levels = rev(industry_dist$sector))

p_industry <- ggplot(industry_long,
                     aes(x = share, y = sector, fill = group)) +
  geom_col(width = 0.7, position = position_dodge(width = 0.75)) +
  geom_text(aes(label = sprintf("%.1f%%", share)),
            position = position_dodge(width = 0.75),
            hjust = -0.15, size = 2.8, color = ink, fontface = "bold") +
  scale_fill_manual(values = c(
    "Foreign-born" = accent_burgundy,
    "U.S.-born"    = as.character(artsy["mustard"])
  )) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title    = "Where Philadelphia's immigrants work",
    subtitle = "NAICS 2-digit sector by foreign-born vs. U.S.-born employment share",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(workers_only)), " employed workers.\n",
                      "Sectors with fewer than 20 raw sample records suppressed.")
  )
print(p_industry)
ggsave("output/chart4_industry_fb_vs_us.png", p_industry,
       width = 10, height = 11, dpi = 300)

# -----------------------------------------------------------------------------
# Step 15. PRIMARY correlation matrix (individual-level)
# -----------------------------------------------------------------------------
# Industry indicators updated to use the new 2-digit sector classification.
# -----------------------------------------------------------------------------
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
    # Industry: use 2-digit sectors
    ind_health      = as.numeric(sector == "Health Care & Social Assistance"),
    ind_food        = as.numeric(sector == "Accommodation & Food Services"),
    ind_construction= as.numeric(sector == "Construction"),
    ind_transport   = as.numeric(sector == "Transportation & Warehousing"),
    ind_professional= as.numeric(sector == "Professional, Scientific & Technical Services"),
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
    `Age` = age_num, `Years in U.S.` = yrs_us,
    `English: Not well` = eng_not_well,
    `English: Well` = eng_well, `English: Very well` = eng_very_well,
    `Linguistically isolated` = lang_isolated,
    `Education: HS/GED` = edu_hs_ged,
    `Education: Some college` = edu_some_col,
    `Education: Bachelor's+` = edu_bachelors,
    `Female` = is_female, `Naturalized` = is_naturalized_num,
    `In labor force` = in_lf_num, `Employed (in LF)` = is_employed,
    `Industry: Healthcare` = ind_health, `Industry: Food/Accom.` = ind_food,
    `Industry: Construction` = ind_construction,
    `Industry: Transport/Wareh.` = ind_transport,
    `Industry: Prof/Sci/Tech` = ind_professional,
    `COW: Private for-profit` = cow_private_fp,
    `COW: Private nonprofit` = cow_private_np,
    `COW: Government` = cow_government,
    `COW: Self-emp (incorporated)` = cow_self_inc,
    `COW: Self-emp (unincorporated)` = cow_self_unic,
    `HH: Single head w/ family` = hh_single_fam,
    `HH: Nonfamily / solo` = hh_solo,
    `Log annual wage` = ln_wage_ind,
    `Log household income` = ln_hh_inc
  )

cor_matrix_ind <- cor(fb_ind, use = "pairwise.complete.obs")

p_cor_ind <- ggcorrplot(
  cor_matrix_ind, hc.order = FALSE, type = "lower",
  lab = TRUE, lab_size = 2.0, outline.color = "white",
  colors = c("#c5d68a", "#f5f0e8", "#c98590"),
  ggtheme = theme_editorial
) +
  labs(
    title    = "Individual-level correlations among Philadelphia's immigrants",
    subtitle = "Person-level attributes of foreign-born residents",
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). ",
                      "N = ", scales::comma(nrow(fb_ind)), " foreign-born individuals.\n",
                      "Tract-level co-variation reported separately to avoid ",
                      "ecological-fallacy conflation (Robinson 1950).")
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
print(p_cor_ind)
ggsave("output/chart5_correlation_individual_PRIMARY.png", p_cor_ind,
       width = 10, height = 13, dpi = 300)

# -----------------------------------------------------------------------------
# Step 16. SUPPLEMENTARY correlation matrix (tract-level)  unchanged
# -----------------------------------------------------------------------------
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
    `Employment-to-pop rate`    = pct_emp_to_pop,
    `% married-couple HH`       = pct_married_couple,
    `% homeowner`               = pct_homeowner,
    `% rent-burdened`           = pct_rent_burdened,
    `Median rent ($)`           = median_gross_rentE,
    `Median HH income ($)`      = median_hh_incomeE
  )

cor_matrix_tract <- cor(cor_df_tract, use = "pairwise.complete.obs")

p_cor_tract <- ggcorrplot(
  cor_matrix_tract, hc.order = FALSE, type = "lower",
  lab = TRUE, lab_size = 2.4, outline.color = "white",
  colors = c("#c5d68a", "#f5f0e8", "#c98590"),
  ggtheme = theme_editorial
) +
  labs(
    title    = "Tract-level co-variation of workforce indicators in Philadelphia",
    subtitle = "Neighborhood-context view across census tracts",
    caption  = paste0("Source: ACS 5-year estimates (2024 vintage). ",
                      "N = ", nrow(cor_df_tract), " tracts. ",
                      "Tract-level correlations describe places, not individuals.")
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
print(p_cor_tract)
ggsave("output/chart6_correlation_tract_SUPP.png", p_cor_tract,
       width = 10, height = 11, dpi = 300)

# =============================================================================
# =============================================================================
# STAGE A — ACCESS CHANNEL: Who gets employed?
# =============================================================================
# =============================================================================
# (Unchanged from v3 — the access logit doesn't depend on POBP-derived
# country labels or industry codes; it uses origin_region from waob_lab
# which is unchanged in its substantive grouping.)
# =============================================================================

# -----------------------------------------------------------------------------
# Step A1. Build access-channel sample and fit logit
# -----------------------------------------------------------------------------
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

cat("\n=== STAGE A: Access channel ===\n")
cat("Working-age FB sample:", nrow(access_df), "\n")
cat("Raw employment rate:    ",
    round(mean(access_df$employed_bin), 3), "\n")
cat("Weighted employment rate:",
    round(weighted.mean(access_df$employed_bin, access_df$PWGTP), 3), "\n")

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
write_csv(access_tidy, "output/1_access_logit.csv")

# -----------------------------------------------------------------------------
# Step A2. Access-channel coefficient plot  (unchanged)
# -----------------------------------------------------------------------------
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
    title    = "Who reaches employment? Access channels for Philadelphia's foreign-born residents",
    subtitle = "Binary logit odds ratios with 95% confidence intervals; OR > 1 indicates higher employment probability",
    x = "Odds ratio (log scale)", y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(access_df)),
                      " working-age (16-65) foreign-born.\n",
                      "Controls: age, age², yrs_us², origin, household. ",
                      "Reference categories: English 'Not at all', ",
                      "Education '<HS', Male, Unmarried.")
  )
print(p_access)
ggsave("output/chart7_access_logit.png", p_access,
       width = 10, height = 14, dpi = 300)

# =============================================================================
# =============================================================================
# STAGE B — WAGE CHANNEL  (logic unchanged from v3)
# =============================================================================
# =============================================================================
# Note: Mincer regression uses origin_region (4 broad groups) and soc_group
# (22 SOC groups) — NOT the 2-digit sector variable. Sector is used only
# for descriptive Chart 4. Regression specification unchanged.
# =============================================================================

# -----------------------------------------------------------------------------
# Step B1. Build Mincer sample
# -----------------------------------------------------------------------------
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
mincer_df$origin_region <- factor(
  mincer_df$waob_lab,
  levels = c("Latin America", "Asia", "Africa", "Europe",
             "Northern America", "Oceania", "PR/US Islands")
)
cat("\n=== STAGE B: Wage channel ===\n")
cat("Mincer sample size:", nrow(mincer_df), "\n")

# -----------------------------------------------------------------------------
# Step B2. Fit Mincer regression
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
mincer_robust <- coeftest(mincer_fit, vcov = vcovHC(mincer_fit, type = "HC1"))

cat("\n=== Mincer regression (HC1 robust SE) ===\n")
print(mincer_robust)

cat("\n=== VIF diagnostic ===\n")
print(car::vif(mincer_fit))

cat("\nR²:", round(summary(mincer_fit)$r.squared, 4),
    "| Adj. R²:", round(summary(mincer_fit)$adj.r.squared, 4),
    "| RMSE:", round(sqrt(mean(mincer_fit$residuals^2)), 4),
    "| N:", nrow(mincer_df), "\n")

mincer_tidy <- tidy(mincer_robust, conf.int = TRUE) %>%
  mutate(
    pct_effect = (exp(estimate)  - 1) * 100,
    pct_low    = (exp(conf.low)  - 1) * 100,
    pct_high   = (exp(conf.high) - 1) * 100
  )
write_csv(mincer_tidy, "output/2_mincer_coefficients.csv")

# -----------------------------------------------------------------------------
# Step B3. Mincer focal coefficient plot  (unchanged from v3)
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

p_mincer <- ggplot(coef_plot_df,
                   aes(x = pct_effect, y = label, color = group)) +
  geom_vline(xintercept = 0, color = gray_dark, linewidth = 0.5) +
  geom_errorbarh(aes(xmin = pct_low, xmax = pct_high),
                 height = 0, linewidth = 1) +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%+.0f%%", pct_effect)),
            vjust = -0.9, size = 3.0, fontface = "bold",
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
    title    = "What shapes wages? Four channels of variation among employed foreign-born workers",
    subtitle = "Mincer regression: percent effect on annual wages with 95% CI (HC1-robust standard errors)",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(mincer_df)),
                      ". Adjusted R² = ",
                      round(summary(mincer_fit)$adj.r.squared, 3),
                      ". Controls: age, age², yrs_us², origin, SOC group, ",
                      "class of worker, household.\n",
                      "Reference categories: English 'Not at all', ",
                      "Education '<HS', Male, Unmarried.")
  )
print(p_mincer)
ggsave("output/chart8_mincer_focal.png", p_mincer,
       width = 10, height = 12, dpi = 300)

# -----------------------------------------------------------------------------
# Step B4. Mincer full coefficient plot  (unchanged)
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

p_mincer_full <- ggplot(coef_full_df,
                        aes(x = pct_effect, y = label, color = group)) +
  geom_vline(xintercept = 0, color = gray_dark, linewidth = 0.5) +
  geom_errorbarh(aes(xmin = pct_low, xmax = pct_high),
                 height = 0, linewidth = 1) +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%+.0f%% %s", pct_effect, sig)),
            vjust = -0.9, size = 2.6, fontface = "bold",
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
    title    = "The full wage equation for Philadelphia's foreign-born workers",
    subtitle = "Mincer regression coefficients with 95% CI (HC1-robust), grouped by channel",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(mincer_df)),
                      ". Adjusted R² = ",
                      round(summary(mincer_fit)$adj.r.squared, 3),
                      ". SOC group (22 levels) and class-of-worker (7 levels) ",
                      "fixed effects included but suppressed.\n",
                      "Significance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1. ",
                      "Reference categories: English 'Not at all', ",
                      "Education '<HS', Origin 'Latin America', Household 'Coupled'.")
  )
print(p_mincer_full)
ggsave("output/chart9_mincer_full.png", p_mincer_full,
       width = 10, height = 12, dpi = 300)

# -----------------------------------------------------------------------------
# Step B5. Heckman two-step  (unchanged from v3)
# -----------------------------------------------------------------------------
foreign_born$married_bin <- as.numeric(foreign_born$MAR == "1")
foreign_born$has_young_children <- as.numeric(
  grepl("with children <18", foreign_born$hh_type)
)

heckman_data <- foreign_born %>%
  filter(
    !is.na(edu_collapsed), !is.na(eng_factor), !is.na(waob_lab),
    !is.na(yrs_us), !is.na(hh_supergroup), !is.na(esr3),
    !is.na(MAR), !is.na(occ_soc), !is.na(cow_detailed),
    age_num >= 16, age_num <= 65
  ) %>%
  mutate(
    employed_bin  = as.numeric(esr3 == "Employed"),
    ln_wage       = ifelse(wage_num > 1000, log(wage_num), NA),
    ln_hours      = ifelse(!is.na(wkhp_num) & wkhp_num > 0,
                           log(wkhp_num), NA),
    female        = as.numeric(SEX == "2"),
    lang_isolated = as.numeric(LNGI == "2"),
    origin_region = factor(waob_lab),
    edu_factor    = edu_collapsed,
    eng_factor_use= eng_factor,
    soc_group     = factor(occ_soc),
    cow_factor    = cow_detailed
  )

cat("\n=== STAGE B — Heckman robustness check ===\n")
cat("N total (ages 16-65):     ", nrow(heckman_data), "\n")
cat("N employed (Stage 2 obs): ", sum(heckman_data$employed_bin), "\n")
cat("Employment rate (raw):    ",
    round(mean(heckman_data$employed_bin), 3), "\n")

heckman_fit <- selection(
  selection = employed_bin ~ edu_factor + age_num + I(age_num^2) +
    yrs_us + I(yrs_us^2) +
    eng_factor_use + female + origin_region +
    married_bin + has_young_children + hh_supergroup,
  outcome   = ln_wage     ~ edu_factor + age_num + I(age_num^2) +
    yrs_us + I(yrs_us^2) +
    eng_factor_use + lang_isolated + female +
    origin_region + soc_group + cow_factor,
  data    = heckman_data,
  weights = heckman_data$PWGTP,
  method  = "2step"
)

cat("\n=== Heckman two-step results ===\n")
print(summary(heckman_fit))

smry <- summary(heckman_fit)$estimate
imr_idx <- grep("invMillsRatio|IMR|lambda|rho",
                rownames(smry), ignore.case = TRUE)
theta   <- smry[imr_idx[1], "Estimate"]
theta_p <- smry[imr_idx[1], "Pr(>|t|)"]

cat("\n--- Selection diagnostic ---\n")
cat(sprintf("Inverse Mills Ratio coefficient θ = %.4f (p = %.4f)\n",
            theta, theta_p))

cat("\n--- OLS vs Heckman-corrected coefficients (key terms) ---\n")
ols_coefs    <- coef(mincer_fit)
heck_coefs   <- coef(heckman_fit, part = "outcome")

compare_specs <- list(
  list(label = "Bachelor's+",      ols = "edu_collapsedBachelor's+",      heck = "edu_factorBachelor's+"),
  list(label = "Some college",     ols = "edu_collapsedSome college/Assoc.", heck = "edu_factorSome college/Assoc."),
  list(label = "HS/GED",           ols = "edu_collapsedHS/GED",           heck = "edu_factorHS/GED"),
  list(label = "English: Very well", ols = "eng_factorVery well",          heck = "eng_factor_useVery well"),
  list(label = "English: Well",    ols = "eng_factorWell",                heck = "eng_factor_useWell"),
  list(label = "English: Not well", ols = "eng_factorNot well",           heck = "eng_factor_useNot well"),
  list(label = "Lang. isolated",   ols = "lang_isolated",                 heck = "lang_isolated"),
  list(label = "Years in U.S.",    ols = "yrs_us",                        heck = "yrs_us"),
  list(label = "Female",           ols = "SEX2",                          heck = "female"),
  list(label = "Origin: Europe",   ols = "origin_regionEurope",           heck = "origin_regionEurope"),
  list(label = "Origin: Africa",   ols = "origin_regionAfrica",           heck = "origin_regionAfrica"),
  list(label = "Origin: Asia",     ols = "origin_regionAsia",             heck = "origin_regionAsia")
)

heckman_compare <- do.call(rbind, lapply(compare_specs, function(s) {
  if (s$ols %in% names(ols_coefs) && s$heck %in% names(heck_coefs)) {
    data.frame(
      Variable   = s$label,
      OLS        = round(ols_coefs[s$ols], 4),
      Heckman    = round(heck_coefs[s$heck], 4),
      Difference = round(heck_coefs[s$heck] - ols_coefs[s$ols], 4),
      OLS_pct    = round((exp(ols_coefs[s$ols])  - 1) * 100, 1),
      Heckman_pct= round((exp(heck_coefs[s$heck]) - 1) * 100, 1)
    )
  }
}))

print(heckman_compare, row.names = FALSE)
write.csv(heckman_compare, "output/3_heckman_comparison.csv", row.names = FALSE)
saveRDS(heckman_fit, "output/4_heckman_fit.rds")

# -----------------------------------------------------------------------------
# Step B6. Two-stage summary chart (OLS version)  unchanged
# -----------------------------------------------------------------------------
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
    title    = "Access and wages — Philadelphia's two-channel immigrant labor market",
    subtitle = "How each attribute affects entry into employment AND wages once employed",
    x = "Effect on employment access (% change in odds)",
    y = "Effect on wages once employed (% change)",
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). ",
                      "Access estimates from binary logit (N = ",
                      scales::comma(nrow(access_df)),
                      "); wage estimates from Mincer regression (N = ",
                      scales::comma(nrow(mincer_df)), ").")
  )
print(p_two_stage)
ggsave("output/chart10_two_stage_summary.png", p_two_stage,
       width = 10, height = 12, dpi = 300)

# -----------------------------------------------------------------------------
# Step B6b. Heckman-corrected two-channel chart  unchanged
# -----------------------------------------------------------------------------
heck_coefs_named <- coef(heckman_fit, part = "outcome")

heck_to_standard <- c(
  "eng_factor_useVery well"            = "eng_factorVery well",
  "eng_factor_useWell"                 = "eng_factorWell",
  "eng_factor_useNot well"             = "eng_factorNot well",
  "lang_isolated"                      = "lang_isolated",
  "edu_factorBachelor's+"              = "edu_collapsedBachelor's+",
  "edu_factorSome college/Assoc."      = "edu_collapsedSome college/Assoc.",
  "edu_factorHS/GED"                   = "edu_collapsedHS/GED",
  "yrs_us"                             = "yrs_us",
  "female"                             = "SEX2",
  "origin_regionAsia"                  = "origin_regionAsia",
  "origin_regionAfrica"                = "origin_regionAfrica",
  "origin_regionEurope"                = "origin_regionEurope"
)

heck_wage <- tibble::tibble(
  heck_term = names(heck_coefs_named),
  estimate  = as.numeric(heck_coefs_named)
) %>%
  filter(heck_term %in% names(heck_to_standard)) %>%
  mutate(
    term     = heck_to_standard[heck_term],
    wage_pct = (exp(estimate) - 1) * 100
  ) %>%
  select(term, wage_pct)

two_stage_heck <- inner_join(
  access_focal_clean %>% select(term, access_or, access_pct),
  heck_wage,
  by = "term"
) %>%
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
                       "origin_regionAsia"                 ~ "Origin: Asia",
                       "origin_regionAfrica"               ~ "Origin: Africa",
                       "origin_regionEurope"               ~ "Origin: Europe"
    ),
    quadrant = case_when(
      access_pct > 0 & wage_pct > 0 ~ "Both stages",
      access_pct > 0 & wage_pct < 0 ~ "Access only",
      access_pct < 0 & wage_pct > 0 ~ "Wage only",
      TRUE                          ~ "Both penalties"
    )
  )

p_two_stage_heck <- ggplot(two_stage_heck,
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
    title    = "What changes when we correct for who reaches employment",
    subtitle = "Access channel × Heckman-corrected wage channel",
    x = "Effect on employment access (% change in odds)",
    y = "Effect on wages once employed, selection-corrected (% change)",
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). ",
                      "Access from binary logit (N = ",
                      scales::comma(nrow(access_df)),
                      "); wages from Heckman two-step correction ",
                      "(N = ", scales::comma(nrow(heckman_data)),
                      " selection; ", scales::comma(sum(heckman_data$employed_bin)),
                      " outcome).\n",
                      "Married and household structure used as Heckman ",
                      "exclusion restrictions, hence omitted from wage equation.")
  )
print(p_two_stage_heck)
ggsave("output/chart10.1_two_stage_heckman.png", p_two_stage_heck,
       width = 10, height = 11, dpi = 300)

# =============================================================================
# =============================================================================
# STAGE C — SPATIAL CHANNEL  (unchanged from v3)
# =============================================================================
# Spatial regression uses only tract-level variables (which come from ACS
# table data, not PUMS recodes), so no POBP/NAICSP-related changes apply.
# =============================================================================

# -----------------------------------------------------------------------------
# Step C1. Build spatial sample and weights
# -----------------------------------------------------------------------------
spatial_df <- tracts %>%
  filter(
    !small_lf_flag,
    !is.na(pct_foreign_born),
    !is.na(median_earn_fbE), median_earn_fbE > 0,
    !is.na(pct_emp_to_pop),
    !is.na(pct_mgmt_prof), !is.na(pct_service_occ),
    !is.na(pct_lang_isolated), !is.na(pct_female_head),
    !is.na(pct_naturalized_of_fb), !is.na(pct_rent_burdened)
  ) %>%
  st_make_valid() %>%
  mutate(ln_earn_fb = log(median_earn_fbE))

cat("\n=== STAGE C: Spatial channel ===\n")
cat("Tracts in spatial sample:", nrow(spatial_df), "\n")

nb      <- poly2nb(spatial_df, queen = TRUE)
lw      <- nb2listw(nb, style = "W", zero.policy = TRUE)

nb_rook <- poly2nb(spatial_df, queen = FALSE)
lw_rook <- nb2listw(nb_rook, style = "W", zero.policy = TRUE)

coords  <- st_coordinates(st_centroid(spatial_df))
nb_knn5 <- knn2nb(knearneigh(coords, k = 5))
lw_knn5 <- nb2listw(nb_knn5, style = "W")

cat("Average # of Queen neighbors:", round(mean(card(nb)), 1), "\n")

philly_limit <- tigris::counties(state = "PA", cb = TRUE, year = acs_year) %>%
  filter(NAME == "Philadelphia") %>%
  st_transform(4326)

# -----------------------------------------------------------------------------
# Steps C2-C8: Moran's I, LISA, choropleth, SDM   (UNCHANGED from v3)
# -----------------------------------------------------------------------------
# (To save context space, these sections are identical to v3 — copy them
# verbatim from your existing script. The substantive logic doesn't depend
# on any of the v4 changes above. Outputs:
#   - Step C2: moran_results, output/5_moran_global.csv
#   - Step C3: LISA cluster columns on spatial_df
#   - Step C4: map1-map6 choropleth maps
#   - Step C5: chart11-chart15 Moran scatters
#   - Step C6: map7-map12 LISA cluster maps
#   - Step C7: ols_fit, slm_fit, sdm_fit (saved as RDS)
#   - Step C8: SDM impacts decomposition
# -----------------------------------------------------------------------------

# Placeholder marker — paste Steps C2-C8 from v3 here verbatim
cat("\n*** Steps C2-C8 unchanged from v3 — paste from existing script ***\n")
cat("*** Saving v3-compatible spatial outputs requires no modifications ***\n")