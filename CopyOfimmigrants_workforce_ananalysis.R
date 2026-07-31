# =============================================================================
# IMMIGRANT WORKFORCE OPPORTUNITY IN PHILADELPHIA — v5 (full script)
#
# Changes from v4:
#   1. ADJINC inflation adjustment applied to WAGP and HINCP (all dollar
#      figures now in constant final-survey-year dollars)
#   2. ENG universe fix: LANX pulled; immigrants who speak only English at
#      home get their own "English only" level instead of being dropped
#   3. Design-based inference: person replicate weights (PWGTP1-80, SDR)
#      via survey/srvyr; svyglm replaces glm/lm+HC1 for Stages A and B
#   4. Stage A split into two parts: labor force participation, then
#      employment conditional on being in the labor force
#   5. Stage B rebuilt as a specification ladder (M1 human capital ->
#      M2 +PUMA FE -> M3 +occupation -> M4 +COW/household), with log hours
#      control; M1 is the headline (total returns), M3-M4 shown to expose
#      the occupational-sorting (brain waste) channel
#   6. New: Oaxaca-Blinder twofold decomposition of the US-born/FB wage gap
#   7. Heckman demoted to robustness appendix (invalid exclusion
#      restrictions: marriage/children have direct wage effects)
#   8. Stage C: CV reliability screen on median FB earnings; island tracts
#      patched with k=1 nearest neighbor (no more silent zero.policy drops)
#   9. Steps C2-C8 written out in full (Moran, LISA, maps, OLS->LM->SDM)
#  10. Treemap pct denominator = all foreign-born (was top-15 only)
#  11. Census key read from environment, never hardcoded
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
library(survey)
library(srvyr)

# car is loaded: pin dplyr verbs against masking
select <- dplyr::select
filter <- dplyr::filter

# -----------------------------------------------------------------------------
# Step 2. Editorial palette and ggplot theme  (unchanged from v4)
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
census_key <- Sys.getenv("a9e713a06a0a0f8ec8531e047c9d01e7d9f507d9")
census_api_key("a9e713a06a0a0f8ec8531e047c9d01e7d9f507d9")

# Philadelphia County 2020-vintage PUMAs (used in 2023+ PUMS releases)
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
# Step 4. Pull ACS PUMS for Pennsylvania — with ADJINC, LANX, rep weights
# -----------------------------------------------------------------------------
pums_vars <- c(
  "AGEP", "SEX", "NATIVITY", "SCHL", "ESR", "CIT", "POBP", "POVPIP",
  "NP", "WAGP", "INDP", "NAICSP", "OCCP", "ENG", "HHL", "LNGI", "YOEP",
  "PUMA", "COW", "WAOB", "HHLDRRAC1P", "MAR", "MIG", "HHT2", "HINCP", "WKHP",
  "ADJINC", "LANX"
)

# Replicate weights roughly quadruple the download; cache the pull.
pums_cache <- "output/pums_raw_pa.rds"
if (file.exists(pums_cache)) {
  pums_raw <- readRDS(pums_cache)
} else {
  pums_raw <- get_pums(
    variables   = pums_vars,
    state       = "PA",
    survey      = "acs5",
    year        = acs_year,
    recode      = TRUE,
    rep_weights = "person"   # PWGTP1-PWGTP80 for SDR design-based SEs
  )
  saveRDS(pums_raw, pums_cache)
}
cat("PUMS rows (PA):", nrow(pums_raw), "\n")

# -----------------------------------------------------------------------------
# Step 5. Filter PUMS to Philadelphia County + sanity check on PUMAs
# -----------------------------------------------------------------------------
pums_philly <- pums_raw[pums_raw$PUMA %in% philly_pumas, ]
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
# Step 6. Pull ACS tract data  (unchanged from v4)
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
# Step 7. Compute derived tract indicators  (unchanged from v4)
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
# 8a. ADJINC inflation adjustment — RUN BEFORE any wage/income use.
# ADJINC arrives as e.g. "1042311" meaning 1.042311.
pums_philly$adj <- suppressWarnings(
  as.numeric(as.character(pums_philly$ADJINC))) / 1e6
stopifnot(all(pums_philly$adj > 0.9 & pums_philly$adj < 1.5, na.rm = TRUE))
cat("ADJINC range:", paste(round(range(pums_philly$adj, na.rm = TRUE), 4),
                           collapse = " - "), "\n")

pums_philly$wage_num  <- as.numeric(pums_philly$WAGP) * pums_philly$adj
pums_philly$hh_income <- suppressWarnings(
  as.numeric(as.character(pums_philly$HINCP))) * pums_philly$adj
pums_philly$wkhp_num  <- suppressWarnings(
  as.numeric(as.character(pums_philly$WKHP)))
# All dollar thresholds below (e.g. wage_num > 1000) are now constant
# final-survey-year dollars; state this in the methods text.

# 8b. English proficiency — ENG universe fix.
# ENG is asked only of people who speak a non-English language at home
# (LANX == 1). LANX == 2 respondents speak only English at home and were
# previously dropped by !is.na(eng_factor); they get their own level.
pums_philly$eng_factor <- NA_character_
pums_philly$eng_factor[pums_philly$LANX == "2"] <- "English only"
pums_philly$eng_factor[pums_philly$ENG == "1"] <- "Very well"
pums_philly$eng_factor[pums_philly$ENG == "2"] <- "Well"
pums_philly$eng_factor[pums_philly$ENG == "3"] <- "Not well"
pums_philly$eng_factor[pums_philly$ENG == "4"] <- "Not at all"
pums_philly$eng_factor <- factor(
  pums_philly$eng_factor,
  levels = c("Not at all", "Not well", "Well", "Very well", "English only"))

cat("\n=== eng_factor among foreign-born (weighted) ===\n")
pums_philly %>%
  filter(NATIVITY == 2) %>%
  group_by(eng_factor) %>%
  summarise(raw_n = n(), weighted_n = sum(PWGTP), .groups = "drop") %>%
  mutate(pct = weighted_n / sum(weighted_n) * 100) %>%
  print()
# Expect a nontrivial "English only" share (Anglophone Caribbean, West
# Africa, India). Remaining NA should be almost entirely age < 5.

# 8c. Education, employment status  (unchanged from v4)
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
# REGION OF BIRTH (waob_lab)  (unchanged from v4)
# -----------------------------------------------------------------------------
pobp_num <- suppressWarnings(as.numeric(as.character(pums_philly$POBP)))

pums_philly$waob_lab <- NA_character_
pums_philly$waob_lab[pobp_num >= 1 & pobp_num <= 56] <- "US-born"
pums_philly$waob_lab[pobp_num %in% c(60, 66, 69, 72, 78)] <- "PR/US Islands"
pums_philly$waob_lab[pobp_num %in% c(100:157, 160, 162:169)] <- "Europe"
pums_philly$waob_lab[pobp_num %in% c(158, 159, 161, 200:254)] <- "Asia"
pums_philly$waob_lab[pobp_num %in% c(300, 301)] <- "Northern America"
pums_philly$waob_lab[pobp_num %in% c(303, 310:399)] <- "Latin America"
pums_philly$waob_lab[pobp_num %in% c(400:499)] <- "Africa"
pums_philly$waob_lab[pobp_num %in% c(500:554)] <- "Oceania"

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

# Mincer regressors
pums_philly$age_num <- as.numeric(pums_philly$AGEP)
pums_philly$age_sq  <- pums_philly$age_num^2

yoep_num <- suppressWarnings(as.numeric(as.character(pums_philly$YOEP)))
yoep_num[pums_philly$NATIVITY == 1] <- NA
pums_philly$yrs_us    <- pmax(acs_year - yoep_num, 0)
pums_philly$yrs_us_sq <- pums_philly$yrs_us^2
pums_philly$recent_arrival <- pums_philly$yrs_us <= 5 & !is.na(pums_philly$yrs_us)

# Arrival-cohort factor (descriptive; a single cross-section cannot
# separate assimilation from cohort quality — Borjas 1985 — say so in text)
pums_philly$arrival_cohort <- cut(
  yoep_num,
  breaks = c(-Inf, 1989, 1999, 2009, 2014, 2019, Inf),
  labels = c("Pre-1990", "1990s", "2000s", "2010-14", "2015-19", "2020+"))

# -----------------------------------------------------------------------------
# Step 9. NAICS 2-digit SECTOR  (unchanged from v4)
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
  "3M" = "Manufacturing",
  "42" = "Wholesale Trade",
  "44" = "Retail Trade",
  "45" = "Retail Trade",
  "4M" = "Retail Trade",
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

pums_philly$sector[is.na(pums_philly$sector) &
                     (naicsp_str == "0" | naicsp_str == "" |
                        is.na(naicsp_str))] <- "Not in labor force / Military"

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

# Class of worker (COW)
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

# SOC occupation group
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

# Citizenship
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

# Household type
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
# Step 9b. Consolidated numeric model variables — created ONCE on
# pums_philly, BEFORE the survey-design conversion in Stage A.
# -----------------------------------------------------------------------------
pums_philly$employed_bin  <- as.numeric(pums_philly$esr3 == "Employed")
pums_philly$in_lf_num     <- as.numeric(pums_philly$esr3 %in%
                                          c("Employed", "Unemployed"))
pums_philly$married_bin   <- as.numeric(pums_philly$MAR == "1")
pums_philly$lang_isolated <- as.numeric(pums_philly$LNGI == "2")
pums_philly$female        <- as.numeric(pums_philly$SEX == "2")
pums_philly$origin_region <- factor(
  pums_philly$waob_lab,
  levels = c("Latin America", "Asia", "Africa", "Europe",
             "Northern America", "Oceania", "PR/US Islands"))
pums_philly$has_young_children <- as.numeric(
  grepl("with children <18", pums_philly$hh_type))

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
# Step 11. Decadal foreign-born population trend  (unchanged from v4)
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
# Step 12. Birthplace treemap — pct denominator fixed to ALL foreign-born
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
  # Americas
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

cat("\n=== Top 20 raw POBP codes (foreign-born sample) ===\n")
foreign_born %>%
  group_by(POBP) %>%
  summarise(weighted_pop = sum(PWGTP, na.rm = TRUE),
            mapped_label = first(country_label),
            .groups = "drop") %>%
  arrange(desc(weighted_pop)) %>%
  head(20) %>%
  print()

total_fb_weighted <- sum(foreign_born$PWGTP, na.rm = TRUE)

top_countries <- foreign_born %>%
  filter(country_label != "Other") %>%
  group_by(country_label) %>%
  summarise(weighted_pop = sum(PWGTP, na.rm = TRUE),
            waob_lab     = first(waob_lab), .groups = "drop") %>%
  arrange(desc(weighted_pop)) %>%
  slice_head(n = 15) %>%
  mutate(
    pct = weighted_pop / total_fb_weighted * 100,   # share of ALL FB
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
    subtitle = "Top 15 countries of birth; percentages are shares of all foreign-born residents",
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
# Step 13. Raw English-wage gap — now 5 levels including "English only"
# -----------------------------------------------------------------------------
employed_fb <- foreign_born[
  foreign_born$ESR %in% c("1", "2") & foreign_born$wage_num > 1000, ]

eng_bar_colors <- c(
  "Not at all"   = "#ecd1d5",
  "Not well"     = "#dca8b0",
  "Well"         = "#c98590",
  "Very well"    = "#a86670",
  "English only" = as.character(artsy["teal"])
)

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
  mutate(bar_color = eng_bar_colors[as.character(eng_factor)])

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
    subtitle = "PWGTP-weighted mean annual wages (constant dollars), employed foreign-born",
    x = NULL, y = NULL,
    caption = paste0("Source: ACS 5-year PUMS (2020-2024), ADJINC-adjusted. ",
                     "Raw N = ", scales::comma(nrow(employed_fb)),
                     " employed FB with positive wages.\n",
                     "'English only' = speaks only English at home (not asked ",
                     "the ENG item). Multivariate estimates in Stage B below.")
  )
print(p2_wage)
ggsave("output/chart3_wage_by_eng.png", p2_wage,
       width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Step 14. Industry distribution — 2-digit sector  (unchanged from v4)
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
# Adds an "English only" indicator; ln wage/income now ADJINC-adjusted.
fb_ind <- foreign_born %>%
  mutate(
    eng_very_well   = as.numeric(eng_factor == "Very well"),
    eng_well        = as.numeric(eng_factor == "Well"),
    eng_not_well    = as.numeric(eng_factor == "Not well"),
    eng_english_only= as.numeric(eng_factor == "English only"),
    edu_hs_ged      = as.numeric(edu_collapsed == "HS/GED"),
    edu_some_col    = as.numeric(edu_collapsed == "Some college/Assoc."),
    edu_bachelors   = as.numeric(edu_collapsed == "Bachelor's+"),
    is_naturalized_num = as.numeric(is_naturalized == "Naturalized"),
    is_employed     = as.numeric(esr3 == "Employed"),
    hh_single_fam   = as.numeric(hh_supergroup == "Single householder with family"),
    hh_solo         = as.numeric(hh_supergroup == "Nonfamily / solo households"),
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
    `English only at home` = eng_english_only,
    `Linguistically isolated` = lang_isolated,
    `Education: HS/GED` = edu_hs_ged,
    `Education: Some college` = edu_some_col,
    `Education: Bachelor's+` = edu_bachelors,
    `Female` = female, `Naturalized` = is_naturalized_num,
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
# Step 16. SUPPLEMENTARY correlation matrix (tract-level)  (unchanged)
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
# STAGE A — ACCESS CHANNEL: participation, then employment given seeking
# =============================================================================
# v5 change: the v4 employed-vs-everyone logit conflated labor force
# participation with job-finding. Two parts now:
#   Part 1: in labor force vs not          (participation)
#   Part 2: employed vs unemployed | in LF (job-finding)
# All SEs are design-based (SDR, 80 person replicate weights).
# =============================================================================

# -----------------------------------------------------------------------------
# Step A0. Survey design object (person replicate weights)
# -----------------------------------------------------------------------------
philly_svy <- to_survey(pums_philly, type = "person", design = "rep_weights")

# -----------------------------------------------------------------------------
# Step A1. Two-part access models
# -----------------------------------------------------------------------------
access_svy <- philly_svy %>%
  filter(
    foreign_born,
    age_num >= 16, age_num <= 65,
    !is.na(eng_factor), !is.na(edu_collapsed),
    !is.na(yrs_us), !is.na(hh_supergroup),
    !is.na(lang_isolated), !is.na(married_bin), !is.na(esr3),
    !is.na(origin_region)
  )

cat("\n=== STAGE A (v5): two-part access, design-based SEs ===\n")
cat("Working-age FB sample:", nrow(access_svy$variables), "\n")
cat("Weighted LF participation rate:",
    round(svymean(~in_lf_num, access_svy)[1], 3), "\n")

# Part 1 — labor force participation
part_fit <- svyglm(
  in_lf_num ~ eng_factor + lang_isolated + edu_collapsed +
    age_num + I(age_num^2) + yrs_us + I(yrs_us^2) +
    origin_region + female + married_bin + hh_supergroup,
  design = access_svy, family = quasibinomial(link = "logit")
)
part_tidy <- tidy(part_fit, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
cat("\n--- Part 1: participation (odds ratios) ---\n")
print(part_tidy)
write_csv(part_tidy, "output/1a_participation_logit.csv")

# Part 2 — employment conditional on being in the labor force
emp_svy <- access_svy %>% filter(in_lf_num == 1)
cat("\nIn-LF FB sample:", nrow(emp_svy$variables), "\n")
cat("Weighted employment rate | in LF:",
    round(svymean(~employed_bin, emp_svy)[1], 3), "\n")

emp_fit <- svyglm(
  employed_bin ~ eng_factor + lang_isolated + edu_collapsed +
    age_num + I(age_num^2) + yrs_us + I(yrs_us^2) +
    origin_region + female + married_bin + hh_supergroup,
  design = emp_svy, family = quasibinomial(link = "logit")
)
emp_tidy <- tidy(emp_fit, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
cat("\n--- Part 2: employment | in LF (odds ratios) ---\n")
print(emp_tidy)
write_csv(emp_tidy, "output/1b_employment_in_lf_logit.csv")

# Downstream (chart 10) uses the job-finding stage as "access":
access_tidy <- emp_tidy
# Caption there notes: access = employment conditional on labor force
# participation; participation effects shown in chart 7.

# -----------------------------------------------------------------------------
# Step A2. Two-panel access chart: participation vs employment | LF
# -----------------------------------------------------------------------------
focal_recode <- function(df) {
  df %>%
    filter(term %in% c("eng_factorNot well", "eng_factorWell",
                       "eng_factorVery well", "eng_factorEnglish only",
                       "lang_isolated",
                       "edu_collapsedHS/GED",
                       "edu_collapsedSome college/Assoc.",
                       "edu_collapsedBachelor's+",
                       "yrs_us", "female", "married_bin")) %>%
    mutate(
      label = case_match(term,
                         "eng_factorNot well"               ~ "English: Not well",
                         "eng_factorWell"                   ~ "English: Well",
                         "eng_factorVery well"              ~ "English: Very well",
                         "eng_factorEnglish only"           ~ "English only at home",
                         "lang_isolated"                    ~ "Linguistically isolated HH",
                         "edu_collapsedHS/GED"              ~ "Education: HS/GED",
                         "edu_collapsedSome college/Assoc." ~ "Education: Some college",
                         "edu_collapsedBachelor's+"         ~ "Education: Bachelor's+",
                         "yrs_us"                           ~ "Each year in U.S.",
                         "female"                           ~ "Female",
                         "married_bin"                      ~ "Married"
      ),
      group = case_when(
        grepl("English|isolated", label) ~ "Language",
        grepl("Education",        label) ~ "Education",
        grepl("Female|Married",   label) ~ "Demographics",
        TRUE                             ~ "Tenure"
      )
    )
}

access_two_panel <- bind_rows(
  focal_recode(part_tidy) %>% mutate(stage = "Part 1: In labor force"),
  focal_recode(emp_tidy)  %>% mutate(stage = "Part 2: Employed | in LF")
) %>%
  mutate(stage = factor(stage, levels = c("Part 1: In labor force",
                                          "Part 2: Employed | in LF")))

label_order <- access_two_panel %>%
  filter(stage == "Part 1: In labor force") %>%
  arrange(estimate) %>% pull(label)
access_two_panel$label <- factor(access_two_panel$label, levels = label_order)

p_access <- ggplot(access_two_panel,
                   aes(x = estimate, y = label, color = group)) +
  geom_vline(xintercept = 1, color = gray_dark, linewidth = 0.5,
             linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~ stage) +
  scale_color_manual(values = c(
    "Language"     = accent_burgundy,
    "Education"    = as.character(artsy["mustard"]),
    "Demographics" = as.character(artsy["teal"]),
    "Tenure"       = as.character(artsy["sage"])
  )) +
  scale_x_continuous(trans = "log",
                     breaks = c(0.25, 0.5, 1, 2, 4),
                     labels = c("0.25", "0.5", "1", "2", "4"),
                     expand = expansion(mult = c(0.08, 0.08))) +
  labs(
    title    = "Two doors to work: who participates, and who finds a job once seeking",
    subtitle = "Odds ratios with 95% CI (design-based SEs); OR > 1 = higher probability",
    x = "Odds ratio (log scale)", y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024), SDR replicate ",
                      "weights. Working-age (16-65) foreign-born; Part 2 ",
                      "restricted to labor force.\n",
                      "Controls: age, age\u00b2, yrs_us\u00b2, origin, household. ",
                      "Reference: English 'Not at all', Education '<HS', ",
                      "Male, Unmarried.")
  )
print(p_access)
ggsave("output/chart7_access_two_part.png", p_access,
       width = 12, height = 9, dpi = 300)

# =============================================================================
# =============================================================================
# STAGE B — WAGE CHANNEL: specification ladder
# =============================================================================
# M1: human capital + demographics + log hours   (TOTAL returns — headline)
# M2: + PUMA fixed effects                       (within-area)
# M3: + SOC occupation group                     (within-occupation)
# M4: + class of worker + household              (full v4-style spec)
# The Bachelor's+ attenuation from M2 to M3 is the return that runs
# through occupational sorting — the brain-waste channel. M3/M4 education
# coefficients are within-occupation returns and must NOT be read as
# total returns to a degree.
# =============================================================================

# -----------------------------------------------------------------------------
# Step B1. Build Mincer survey subset
# -----------------------------------------------------------------------------
mincer_svy <- philly_svy %>%
  filter(
    foreign_born,
    ESR %in% c("1", "2"),
    wage_num > 1000,
    !is.na(wkhp_num), wkhp_num > 0,
    !is.na(eng_factor), !is.na(edu_collapsed), !is.na(yrs_us),
    !is.na(occ_soc), !is.na(cow_detailed), !is.na(hh_supergroup),
    !is.na(lang_isolated), !is.na(married_bin), !is.na(origin_region),
    age_num >= 16, age_num <= 75
  ) %>%
  mutate(
    ln_wage    = log(wage_num),
    ln_hours   = log(wkhp_num),
    soc_group  = factor(occ_soc),
    cow_factor = cow_detailed,
    puma_f     = factor(PUMA)
  )

cat("\n=== STAGE B (v5): wage channel, spec ladder ===\n")
cat("Mincer sample size:", nrow(mincer_svy$variables), "\n")

# -----------------------------------------------------------------------------
# Step B2. Fit the ladder
# -----------------------------------------------------------------------------
m1 <- svyglm(
  ln_wage ~ eng_factor + lang_isolated + edu_collapsed +
    age_num + I(age_num^2) + yrs_us + I(yrs_us^2) +
    origin_region + female + married_bin + ln_hours,
  design = mincer_svy)
m2 <- update(m1, . ~ . + puma_f)
m3 <- update(m2, . ~ . + soc_group)
m4 <- update(m3, . ~ . + cow_factor + hh_supergroup)

spec_ladder <- bind_rows(
  tidy(m1, conf.int = TRUE) %>% mutate(spec = "M1: Human capital"),
  tidy(m2, conf.int = TRUE) %>% mutate(spec = "M2: + PUMA FE"),
  tidy(m3, conf.int = TRUE) %>% mutate(spec = "M3: + Occupation"),
  tidy(m4, conf.int = TRUE) %>% mutate(spec = "M4: + COW + household")
) %>%
  mutate(
    pct_effect = (exp(estimate)  - 1) * 100,
    pct_low    = (exp(conf.low)  - 1) * 100,
    pct_high   = (exp(conf.high) - 1) * 100
  )
write_csv(spec_ladder, "output/2_mincer_spec_ladder.csv")

# Collinearity diagnostic on an unweighted lm analog of M4 (car::vif does
# not accept svyglm; this is a diagnostic, not inference)
vif_lm <- lm(formula(m4), data = mincer_svy$variables)
cat("\n=== VIF diagnostic (unweighted lm analog of M4) ===\n")
print(car::vif(vif_lm))

# -----------------------------------------------------------------------------
# Step B3. Spec ladder chart
# -----------------------------------------------------------------------------
ladder_focal <- spec_ladder %>%
  filter(term %in% c("edu_collapsedBachelor's+",
                     "edu_collapsedSome college/Assoc.",
                     "eng_factorVery well", "eng_factorEnglish only",
                     "eng_factorWell", "female")) %>%
  mutate(
    term_lab = case_match(term,
                          "edu_collapsedBachelor's+"         ~ "Bachelor's+",
                          "edu_collapsedSome college/Assoc." ~ "Some college",
                          "eng_factorVery well"              ~ "English: Very well",
                          "eng_factorEnglish only"           ~ "English only at home",
                          "eng_factorWell"                   ~ "English: Well",
                          "female"                           ~ "Female"),
    spec = factor(spec, levels = c(
      "M1: Human capital", "M2: + PUMA FE",
      "M3: + Occupation", "M4: + COW + household"))
  )

p_ladder <- ggplot(ladder_focal,
                   aes(x = spec, y = pct_effect, group = term_lab)) +
  geom_hline(yintercept = 0, color = gray_mid, linewidth = 0.4) +
  geom_line(color = gray_light, linewidth = 0.6) +
  geom_pointrange(aes(ymin = pct_low, ymax = pct_high),
                  color = accent_burgundy, size = 0.5) +
  facet_wrap(~ term_lab, scales = "free_y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "How returns change as controls absorb the sorting channel",
    subtitle = "The Bachelor's+ drop from M2 to M3 is the return that runs through occupation — the brain-waste channel",
    x = NULL, y = "Percent effect on annual wages",
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024), ADJINC-adjusted, ",
                      "SDR design-based SEs. N = ",
                      scales::comma(nrow(mincer_svy$variables)),
                      " employed foreign-born.\n",
                      "All models control log usual hours; reference ",
                      "categories: English 'Not at all', Education '<HS'.")
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
print(p_ladder)
ggsave("output/chart8b_spec_ladder.png", p_ladder,
       width = 11, height = 8, dpi = 300)

# -----------------------------------------------------------------------------
# Step B4. Focal coefficient plot from M1 (total returns — headline)
# -----------------------------------------------------------------------------
mincer_tidy <- tidy(m1, conf.int = TRUE) %>%
  mutate(pct_effect = (exp(estimate)  - 1) * 100,
         pct_low    = (exp(conf.low)  - 1) * 100,
         pct_high   = (exp(conf.high) - 1) * 100)
write_csv(mincer_tidy, "output/2_mincer_m1_coefficients.csv")

coef_plot_df <- mincer_tidy %>%
  filter(term %in% c("eng_factorNot well", "eng_factorWell",
                     "eng_factorVery well", "eng_factorEnglish only",
                     "lang_isolated",
                     "edu_collapsedHS/GED",
                     "edu_collapsedSome college/Assoc.",
                     "edu_collapsedBachelor's+",
                     "yrs_us", "female", "married_bin")) %>%
  mutate(
    label = case_match(term,
                       "eng_factorNot well"               ~ "English: Not well",
                       "eng_factorWell"                   ~ "English: Well",
                       "eng_factorVery well"              ~ "English: Very well",
                       "eng_factorEnglish only"           ~ "English only at home",
                       "lang_isolated"                    ~ "Linguistically isolated HH",
                       "edu_collapsedHS/GED"              ~ "Education: HS/GED",
                       "edu_collapsedSome college/Assoc." ~ "Education: Some college",
                       "edu_collapsedBachelor's+"         ~ "Education: Bachelor's+",
                       "yrs_us"                           ~ "Each year in U.S.",
                       "female"                           ~ "Female",
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
    title    = "What shapes wages? Total returns among employed foreign-born workers",
    subtitle = "M1 (no occupation controls): percent effect on annual wages with 95% CI, design-based SEs",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024), ADJINC-adjusted. ",
                      "N = ", scales::comma(nrow(mincer_svy$variables)),
                      ". Controls: age, age\u00b2, yrs_us\u00b2, origin, log hours.\n",
                      "Occupation deliberately excluded: total returns include ",
                      "the sorting channel (see spec ladder). Reference: ",
                      "English 'Not at all', Education '<HS', Male, Unmarried.")
  )
print(p_mincer)
ggsave("output/chart8_mincer_focal.png", p_mincer,
       width = 10, height = 12, dpi = 300)

# -----------------------------------------------------------------------------
# Step B5. Full coefficient plot from M4 (within-occupation spec)
# -----------------------------------------------------------------------------
m4_tidy <- tidy(m4, conf.int = TRUE) %>%
  mutate(pct_effect = (exp(estimate)  - 1) * 100,
         pct_low    = (exp(conf.low)  - 1) * 100,
         pct_high   = (exp(conf.high) - 1) * 100)
write_csv(m4_tidy, "output/2_mincer_m4_coefficients.csv")

coef_full_df <- m4_tidy %>%
  filter(term != "(Intercept)",
         !grepl("I\\(age_num\\^2\\)|I\\(yrs_us\\^2\\)", term),
         !grepl("^soc_group|^cow_factor|^puma_f", term)) %>%
  mutate(
    label = case_match(term,
                       "eng_factorNot well"               ~ "English: Not well",
                       "eng_factorWell"                   ~ "English: Well",
                       "eng_factorVery well"              ~ "English: Very well",
                       "eng_factorEnglish only"           ~ "English only at home",
                       "lang_isolated"                    ~ "Linguistically isolated HH",
                       "edu_collapsedHS/GED"              ~ "Education: HS/GED",
                       "edu_collapsedSome college/Assoc." ~ "Education: Some college",
                       "edu_collapsedBachelor's+"         ~ "Education: Bachelor's+",
                       "age_num"                          ~ "Each year of age",
                       "yrs_us"                           ~ "Each year in U.S.",
                       "ln_hours"                         ~ "Log usual hours",
                       "origin_regionAsia"                ~ "Origin: Asia",
                       "origin_regionAfrica"              ~ "Origin: Africa",
                       "origin_regionEurope"              ~ "Origin: Europe",
                       "origin_regionNorthern America"    ~ "Origin: N. America",
                       "origin_regionOceania"             ~ "Origin: Oceania",
                       "female"                           ~ "Female",
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
      TRUE                             ~ "Age / tenure / hours"
    ),
    sig = case_when(
      p.value < 0.001 ~ "***", p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",   p.value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    group = factor(group, levels = c("Language", "Education",
                                     "Demographics", "Household",
                                     "Age / tenure / hours", "Origin"))
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
    "Language"             = accent_burgundy,
    "Education"            = as.character(artsy["mustard"]),
    "Demographics"         = as.character(artsy["teal"]),
    "Household"            = "#7d7676",
    "Age / tenure / hours" = as.character(artsy["sage"]),
    "Origin"               = "#3a3f5e"
  )) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.08, 0.08))) +
  labs(
    title    = "The full within-occupation wage equation (M4)",
    subtitle = "Coefficients with 95% CI (design-based SEs), grouped by channel — read as within-occupation returns",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024), ADJINC-adjusted. ",
                      "N = ", scales::comma(nrow(mincer_svy$variables)),
                      ". PUMA (11), SOC group (22), and class-of-worker ",
                      "fixed effects included but suppressed.\n",
                      "Significance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1. ",
                      "Reference: English 'Not at all', Education '<HS', ",
                      "Origin 'Latin America', Household 'Coupled'.")
  )
print(p_mincer_full)
ggsave("output/chart9_mincer_full_m4.png", p_mincer_full,
       width = 10, height = 12, dpi = 300)

# -----------------------------------------------------------------------------
# Step B6. Oaxaca-Blinder twofold decomposition: US-born vs foreign-born
# -----------------------------------------------------------------------------
# US-born coefficients as reference ("what FB workers would earn at native
# returns"). Point estimates via PWGTP; descriptive. Occupation is
# deliberately excluded here — occupational sorting is part of what the
# decomposition should attribute, not absorb. An occupation-controlled
# version belongs in the appendix as a contrast.
# -----------------------------------------------------------------------------
oax_form <- ln_wage ~ edu_collapsed + age_num + age_sq + female +
  married_bin + ln_hours

oax_base <- pums_philly %>%
  filter(ESR %in% c("1", "2"), wage_num > 1000,
         !is.na(wkhp_num), wkhp_num > 0,
         !is.na(edu_collapsed), !is.na(married_bin),
         age_num >= 16, age_num <= 75) %>%
  mutate(ln_wage = log(wage_num), ln_hours = log(wkhp_num))

fb_df <- oax_base %>% filter(foreign_born)
us_df <- oax_base %>% filter(!foreign_born)
cat("\n=== Oaxaca-Blinder samples ===\n")
cat("FB employed:", nrow(fb_df), "| US-born employed:", nrow(us_df), "\n")

fit_fb <- lm(oax_form, data = fb_df, weights = PWGTP)
fit_us <- lm(oax_form, data = us_df, weights = PWGTP)

wmean_mm <- function(fit, df) {
  mm <- model.matrix(fit)
  w  <- df$PWGTP[as.numeric(rownames(mm))]
  apply(mm, 2, weighted.mean, w = w)
}
xbar_fb <- wmean_mm(fit_fb, fb_df)
xbar_us <- wmean_mm(fit_us, us_df)

gap <- weighted.mean(us_df$ln_wage, us_df$PWGTP) -
  weighted.mean(fb_df$ln_wage, fb_df$PWGTP)
explained   <- sum((xbar_us - xbar_fb) * coef(fit_us))
unexplained <- gap - explained

cat("\n=== Oaxaca-Blinder: US-born minus foreign-born (log wages) ===\n")
cat(sprintf("Total log-wage gap:      %+.4f  (%+.1f%%)\n",
            gap, (exp(gap) - 1) * 100))
cat(sprintf("Explained (endowments):  %+.4f  (%.0f%% of gap)\n",
            explained, 100 * explained / gap))
cat(sprintf("Unexplained (returns):   %+.4f  (%.0f%% of gap)\n",
            unexplained, 100 * unexplained / gap))

oax_detail <- tibble(
  term           = names(coef(fit_us)),
  endow_contrib  = (xbar_us - xbar_fb) * coef(fit_us),
  return_contrib = xbar_fb * (coef(fit_us) - coef(fit_fb))
) %>% filter(term != "(Intercept)")
print(oax_detail)
write_csv(oax_detail, "output/6_oaxaca_detail.csv")

oax_plot_df <- oax_detail %>%
  pivot_longer(cols = c(endow_contrib, return_contrib),
               names_to = "component", values_to = "contrib") %>%
  mutate(
    component = case_match(component,
                           "endow_contrib"  ~ "Endowments (composition)",
                           "return_contrib" ~ "Returns (coefficients)"),
    term_lab = case_match(term,
                          "edu_collapsedHS/GED"              ~ "HS/GED",
                          "edu_collapsedSome college/Assoc." ~ "Some college",
                          "edu_collapsedBachelor's+"         ~ "Bachelor's+",
                          "age_num"                          ~ "Age",
                          "age_sq"                           ~ "Age\u00b2",
                          "female"                           ~ "Female",
                          "married_bin"                      ~ "Married",
                          "ln_hours"                         ~ "Log hours",
                          .default = term)
  )

p_oax <- ggplot(oax_plot_df,
                aes(x = contrib, y = reorder(term_lab, contrib),
                    fill = component)) +
  geom_col(width = 0.65, position = position_dodge(width = 0.72)) +
  geom_vline(xintercept = 0, color = gray_dark, linewidth = 0.5) +
  scale_fill_manual(values = c(
    "Endowments (composition)" = as.character(artsy["mustard"]),
    "Returns (coefficients)"   = accent_burgundy
  )) +
  labs(
    title    = "Decomposing the US-born / foreign-born wage gap",
    subtitle = sprintf(
      "Total gap %+.1f%%: endowments explain %.0f%%, differential returns %.0f%%",
      (exp(gap) - 1) * 100, 100 * explained / gap, 100 * unexplained / gap),
    x = "Contribution to log-wage gap", y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024), ADJINC-adjusted, ",
                      "PWGTP-weighted. Twofold Oaxaca-Blinder, US-born ",
                      "coefficients as reference.\n",
                      "Occupation deliberately excluded so occupational ",
                      "sorting loads on the returns component.")
  )
print(p_oax)
ggsave("output/chart8c_oaxaca.png", p_oax,
       width = 10, height = 7, dpi = 300)

# -----------------------------------------------------------------------------
# Step B7. Two-stage summary chart: job-finding access x total-return wages
# -----------------------------------------------------------------------------
focal_terms <- c(
  "eng_factorVery well", "eng_factorWell", "eng_factorNot well",
  "eng_factorEnglish only", "lang_isolated",
  "edu_collapsedBachelor's+", "edu_collapsedSome college/Assoc.",
  "edu_collapsedHS/GED",
  "yrs_us", "female", "married_bin",
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
                       "eng_factorEnglish only"            ~ "English only at home",
                       "lang_isolated"                     ~ "Ling. isolated HH",
                       "edu_collapsedBachelor's+"          ~ "Bachelor's+",
                       "edu_collapsedSome college/Assoc."  ~ "Some college",
                       "edu_collapsedHS/GED"               ~ "HS/GED",
                       "yrs_us"                            ~ "Each year in U.S.",
                       "female"                            ~ "Female",
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
           label = "Boosts BOTH job-finding\nand wages",
           size = 3, color = gray_mid, fontface = "italic") +
  annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -1,
           label = "Opens door but\nno wage premium",
           size = 3, color = gray_mid, fontface = "italic") +
  annotate("text", x = -Inf, y = -Inf, hjust = -0.1, vjust = -1,
           label = "Double penalty",
           size = 3, color = gray_mid, fontface = "italic") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = "Wage premium\ndespite access barrier",
           size = 3, color = gray_mid, fontface = "italic") +
  labs(
    title    = "Job-finding and wages — Philadelphia's two-channel immigrant labor market",
    subtitle = "Access = employment odds conditional on labor force participation; wages = total returns (M1)",
    x = "Effect on job-finding | in labor force (% change in odds)",
    y = "Effect on wages once employed (% change)",
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024), design-based SEs. ",
                      "Access from Part 2 logit; wages from M1 (no occupation ",
                      "controls, so sorting is included).\n",
                      "Participation effects shown separately in chart 7; ",
                      "brain-waste evidence shown in the spec ladder ",
                      "(chart 8b), not this quadrant.")
  )
print(p_two_stage)
ggsave("output/chart10_two_stage_summary.png", p_two_stage,
       width = 10, height = 12, dpi = 300)

# -----------------------------------------------------------------------------
# Step B8. Arrival-cohort descriptive chart
# -----------------------------------------------------------------------------
cohort_wages <- foreign_born %>%
  filter(ESR %in% c("1", "2"), wage_num > 1000, !is.na(arrival_cohort)) %>%
  group_by(arrival_cohort) %>%
  summarise(
    median_wage = weighted_median(wage_num, PWGTP),
    weighted_n  = sum(PWGTP), raw_n = n(), .groups = "drop")

p_cohort <- ggplot(cohort_wages,
                   aes(x = arrival_cohort, y = median_wage)) +
  geom_col(width = 0.62, fill = accent_burgundy) +
  geom_text(aes(label = scales::dollar(round(median_wage, -2))),
            vjust = -0.5, size = 3.4, color = ink, fontface = "bold") +
  scale_y_continuous(labels = scales::dollar,
                     expand = expansion(mult = c(0.02, 0.12))) +
  labs(
    title    = "Median wages by arrival cohort",
    subtitle = "Employed foreign-born, constant dollars",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024), ADJINC-adjusted.\n",
                      "Descriptive only: a single cross-section cannot ",
                      "separate assimilation from cohort quality ",
                      "(Borjas 1985).")
  )
print(p_cohort)
ggsave("output/chart8d_arrival_cohorts.png", p_cohort,
       width = 10, height = 5, dpi = 300)

# =============================================================================
# APPENDIX — Heckman two-step (ROBUSTNESS ONLY, demoted from v4 main text)
# =============================================================================
# Caveat for the paper: marriage and children have direct wage effects
# (marriage premium, motherhood penalty), so the exclusion restrictions
# are not valid and identification leans on the IMR's functional form.
# sampleSelection's 2-step weight handling is also not full design-based
# inference. Present as sensitivity; do not headline.
# =============================================================================
heckman_data <- foreign_born %>%
  filter(
    !is.na(edu_collapsed), !is.na(eng_factor), !is.na(waob_lab),
    !is.na(yrs_us), !is.na(hh_supergroup), !is.na(esr3),
    !is.na(MAR), !is.na(occ_soc), !is.na(cow_detailed),
    age_num >= 16, age_num <= 65
  ) %>%
  mutate(
    ln_wage       = ifelse(wage_num > 1000, log(wage_num), NA),
    origin_region2= factor(waob_lab),
    edu_factor    = edu_collapsed,
    eng_factor_use= eng_factor,
    soc_group     = factor(occ_soc),
    cow_factor    = cow_detailed
  )

cat("\n=== APPENDIX: Heckman robustness check ===\n")
cat("N total (ages 16-65):     ", nrow(heckman_data), "\n")
cat("N employed (Stage 2 obs): ", sum(heckman_data$employed_bin), "\n")

heckman_fit <- selection(
  selection = employed_bin ~ edu_factor + age_num + I(age_num^2) +
    yrs_us + I(yrs_us^2) +
    eng_factor_use + female + origin_region2 +
    married_bin + has_young_children + hh_supergroup,
  outcome   = ln_wage     ~ edu_factor + age_num + I(age_num^2) +
    yrs_us + I(yrs_us^2) +
    eng_factor_use + lang_isolated + female +
    origin_region2 + soc_group + cow_factor,
  data    = heckman_data,
  weights = heckman_data$PWGTP,
  method  = "2step"
)

print(summary(heckman_fit))

smry <- summary(heckman_fit)$estimate
imr_idx <- grep("invMillsRatio|IMR|lambda|rho",
                rownames(smry), ignore.case = TRUE)
theta   <- smry[imr_idx[1], "Estimate"]
theta_p <- smry[imr_idx[1], "Pr(>|t|)"]
cat(sprintf("\nInverse Mills Ratio coefficient theta = %.4f (p = %.4f)\n",
            theta, theta_p))

heck_coefs <- coef(heckman_fit, part = "outcome")
m1_coefs   <- coef(m1)
compare_specs <- list(
  list(label = "Bachelor's+",        m1 = "edu_collapsedBachelor's+",         heck = "edu_factorBachelor's+"),
  list(label = "Some college",       m1 = "edu_collapsedSome college/Assoc.", heck = "edu_factorSome college/Assoc."),
  list(label = "HS/GED",             m1 = "edu_collapsedHS/GED",              heck = "edu_factorHS/GED"),
  list(label = "English: Very well", m1 = "eng_factorVery well",              heck = "eng_factor_useVery well"),
  list(label = "English: Well",      m1 = "eng_factorWell",                   heck = "eng_factor_useWell"),
  list(label = "English only",       m1 = "eng_factorEnglish only",           heck = "eng_factor_useEnglish only"),
  list(label = "Lang. isolated",     m1 = "lang_isolated",                    heck = "lang_isolated"),
  list(label = "Years in U.S.",      m1 = "yrs_us",                           heck = "yrs_us"),
  list(label = "Female",             m1 = "female",                           heck = "female")
)
heckman_compare <- do.call(rbind, lapply(compare_specs, function(s) {
  if (s$m1 %in% names(m1_coefs) && s$heck %in% names(heck_coefs)) {
    data.frame(
      Variable    = s$label,
      M1          = round(m1_coefs[s$m1], 4),
      Heckman     = round(heck_coefs[s$heck], 4),
      Difference  = round(heck_coefs[s$heck] - m1_coefs[s$m1], 4),
      M1_pct      = round((exp(m1_coefs[s$m1])   - 1) * 100, 1),
      Heckman_pct = round((exp(heck_coefs[s$heck]) - 1) * 100, 1)
    )
  }
}))
print(heckman_compare, row.names = FALSE)
write.csv(heckman_compare, "output/3_heckman_comparison.csv", row.names = FALSE)
saveRDS(heckman_fit, "output/4_heckman_fit.rds")
# Note the comparison mixes specifications (Heckman outcome includes
# occupation/COW, M1 does not) — read directionally only, or compare to
# M4 instead when writing the appendix.

# =============================================================================
# =============================================================================
# STAGE C — SPATIAL CHANNEL
# =============================================================================
# v5: CV reliability screen on the DV; island tracts patched with k=1
# nearest neighbor; Steps C2-C8 written out in full.
# All tract-level associations are ecological — they describe places, not
# individuals — and the SDM is associational, not causal.
# =============================================================================

# -----------------------------------------------------------------------------
# Step C1. Build spatial sample, screen, and weights
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
cat("Tracts before CV screen:", nrow(spatial_df), "\n")

# CV reliability screen: median FB earnings is noisy where FB counts are
# small, and that noise is correlated with pct_foreign_born (attenuation
# plus a spurious spatial pattern). 90% MOE -> SE via /1.645.
spatial_df <- spatial_df %>%
  mutate(cv_earn = (median_earn_fbM / 1.645) / median_earn_fbE) %>%
  filter(!is.na(cv_earn), cv_earn <= 0.40)
cat("Tracts after CV <= 0.40 screen:", nrow(spatial_df), "\n")
# Report the screened count and threshold in the methods text.
# Sensitivity: rerun the SDM at CV <= 0.30 and unscreened; report stability.

# Queen contiguity with island k=1 patch (no silent zero.policy drops)
nb  <- poly2nb(spatial_df, queen = TRUE)
iso <- which(card(nb) == 0)
coords <- st_coordinates(st_centroid(spatial_df))
if (length(iso) > 0) {
  cat("Isolate tracts patched with k=1 nearest neighbor:", length(iso), "\n")
  knn1 <- knn2nb(knearneigh(coords, k = 1))
  for (i in iso) nb[[i]] <- knn1[[i]]
  nb <- make.sym.nb(nb)
}
lw <- nb2listw(nb, style = "W")

# Alternative weights for robustness
nb_rook <- poly2nb(spatial_df, queen = FALSE)
iso_r <- which(card(nb_rook) == 0)
if (length(iso_r) > 0) {
  knn1 <- knn2nb(knearneigh(coords, k = 1))
  for (i in iso_r) nb_rook[[i]] <- knn1[[i]]
  nb_rook <- make.sym.nb(nb_rook)
}
lw_rook <- nb2listw(nb_rook, style = "W")

nb_knn5 <- knn2nb(knearneigh(coords, k = 5))
lw_knn5 <- nb2listw(nb_knn5, style = "W")

cat("Average # of Queen neighbors:", round(mean(card(nb)), 1), "\n")

philly_limit <- tigris::counties(state = "PA", cb = TRUE, year = acs_year) %>%
  filter(NAME == "Philadelphia") %>%
  st_transform(4326)

# -----------------------------------------------------------------------------
# Step C2. Global Moran's I — key indicators x three weights matrices
# -----------------------------------------------------------------------------
moran_vars <- c("ln_earn_fb", "pct_foreign_born", "pct_lang_isolated",
                "pct_naturalized_of_fb", "unemp_rate", "pct_rent_burdened")
moran_grid <- expand.grid(variable = moran_vars,
                          weights  = c("Queen", "Rook", "kNN-5"),
                          stringsAsFactors = FALSE)
moran_rows <- vector("list", nrow(moran_grid))
for (i in seq_len(nrow(moran_grid))) {
  v  <- moran_grid$variable[i]
  w  <- switch(moran_grid$weights[i],
               "Queen" = lw, "Rook" = lw_rook, "kNN-5" = lw_knn5)
  mt <- moran.test(spatial_df[[v]], w)
  moran_rows[[i]] <- data.frame(
    variable = v, weights = moran_grid$weights[i],
    moran_i  = round(unname(mt$estimate["Moran I statistic"]), 4),
    expected = round(unname(mt$estimate["Expectation"]), 4),
    p_value  = signif(mt$p.value, 4)
  )
}
moran_results <- do.call(rbind, moran_rows)
cat("\n=== Global Moran's I (robustness across weights) ===\n")
print(moran_results)
write_csv(moran_results, "output/5_moran_global.csv")

# -----------------------------------------------------------------------------
# Step C3. LISA — local Moran for ln_earn_fb and pct_foreign_born
# -----------------------------------------------------------------------------
lisa_classify <- function(x, w, alpha = 0.05) {
  lm_out <- localmoran(x, w)
  z      <- as.numeric(scale(x))
  lag_z  <- lag.listw(w, z)
  p      <- lm_out[, "Pr(z != E(Ii))"]
  cl <- case_when(
    p >= alpha            ~ "Not significant",
    z >= 0 & lag_z >= 0   ~ "High-High",
    z <  0 & lag_z <  0   ~ "Low-Low",
    z >= 0 & lag_z <  0   ~ "High-Low",
    TRUE                  ~ "Low-High"
  )
  factor(cl, levels = c("High-High", "Low-Low", "High-Low",
                        "Low-High", "Not significant"))
}

spatial_df$lisa_earn <- lisa_classify(spatial_df$ln_earn_fb, lw)
spatial_df$lisa_fb   <- lisa_classify(spatial_df$pct_foreign_born, lw)

cat("\n=== LISA cluster counts ===\n")
cat("-- ln_earn_fb --\n");        print(table(spatial_df$lisa_earn))
cat("-- pct_foreign_born --\n");  print(table(spatial_df$lisa_fb))

lisa_colors <- c(
  "High-High"       = "#a86670",
  "Low-Low"         = as.character(artsy["teal"]),
  "High-Low"        = "#dca8b0",
  "Low-High"        = "#9aa3d6",
  "Not significant" = gray_light
)

# -----------------------------------------------------------------------------
# Step C4. Choropleth maps
# -----------------------------------------------------------------------------
make_choro <- function(var, title, subtitle, legend_title, fname,
                       labels_fn = waiver()) {
  p <- ggplot(spatial_df) +
    geom_sf(aes(fill = .data[[var]]), color = "white", linewidth = 0.08) +
    geom_sf(data = philly_limit, fill = NA, color = ink, linewidth = 0.5) +
    scale_fill_gradientn(colors = ramp_seq, name = legend_title,
                         labels = labels_fn) +
    labs(title = title, subtitle = subtitle,
         caption = paste0("Source: ACS 5-year estimates (2024 vintage). ",
                          "Tracts screened: LF >= 50 and CV(median FB ",
                          "earnings) <= 0.40.")) +
    theme_map
  print(p)
  ggsave(paste0("output/", fname), p, width = 9, height = 9, dpi = 300)
  invisible(p)
}

map1 <- make_choro("pct_foreign_born",
                   "Where Philadelphia's immigrants live",
                   "Foreign-born share of tract population",
                   "% foreign-born", "map1_pct_foreign_born.png",
                   labels_fn = function(x) paste0(x, "%"))
map2 <- make_choro("median_earn_fbE",
                   "What foreign-born workers earn, by neighborhood",
                   "Tract median earnings, foreign-born workers",
                   "Median earnings", "map2_median_earn_fb.png",
                   labels_fn = scales::dollar)
map3 <- make_choro("pct_lang_isolated",
                   "Linguistic isolation across the city",
                   "Share of households where no member 14+ speaks English very well",
                   "% lang. isolated", "map3_lang_isolated.png",
                   labels_fn = function(x) paste0(x, "%"))
map4 <- make_choro("pct_naturalized_of_fb",
                   "Naturalization geography",
                   "Naturalized share of the foreign-born population",
                   "% naturalized", "map4_naturalized.png",
                   labels_fn = function(x) paste0(x, "%"))

# -----------------------------------------------------------------------------
# Step C5. Moran scatterplot — ln_earn_fb
# -----------------------------------------------------------------------------
spatial_df$z_earn     <- as.numeric(scale(spatial_df$ln_earn_fb))
spatial_df$lag_z_earn <- lag.listw(lw, spatial_df$z_earn)
moran_i_earn <- moran_results$moran_i[
  moran_results$variable == "ln_earn_fb" & moran_results$weights == "Queen"]

p_moran_sc <- ggplot(spatial_df, aes(x = z_earn, y = lag_z_earn)) +
  geom_hline(yintercept = 0, color = gray_mid, linewidth = 0.4) +
  geom_vline(xintercept = 0, color = gray_mid, linewidth = 0.4) +
  geom_point(aes(color = lisa_earn), size = 2, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, color = ink, linewidth = 0.7) +
  scale_color_manual(values = lisa_colors, name = "LISA cluster") +
  labs(
    title    = "Spatial clustering of foreign-born earnings",
    subtitle = sprintf("Moran scatterplot, log median FB earnings (Queen weights); global I = %.3f",
                       moran_i_earn),
    x = "Standardized log median FB earnings",
    y = "Spatial lag (neighbors' average)",
    caption  = "Source: ACS 5-year estimates (2024 vintage). LISA at p < 0.05."
  ) +
  theme(legend.position = "right")
print(p_moran_sc)
ggsave("output/chart11_moran_scatter_earn.png", p_moran_sc,
       width = 9, height = 8, dpi = 300)

# -----------------------------------------------------------------------------
# Step C6. LISA cluster maps
# -----------------------------------------------------------------------------
make_lisa_map <- function(var, title, fname) {
  p <- ggplot(spatial_df) +
    geom_sf(aes(fill = .data[[var]]), color = "white", linewidth = 0.08) +
    geom_sf(data = philly_limit, fill = NA, color = ink, linewidth = 0.5) +
    scale_fill_manual(values = lisa_colors, name = "LISA cluster") +
    labs(title = title,
         subtitle = "Local Moran clusters, Queen weights, p < 0.05",
         caption = "Source: ACS 5-year estimates (2024 vintage).") +
    theme_map
  print(p)
  ggsave(paste0("output/", fname), p, width = 9, height = 9, dpi = 300)
  invisible(p)
}

map7 <- make_lisa_map("lisa_earn",
                      "Clusters of high and low foreign-born earnings",
                      "map7_lisa_earnings.png")
map8 <- make_lisa_map("lisa_fb",
                      "Clusters of immigrant settlement",
                      "map8_lisa_foreign_born.png")

# -----------------------------------------------------------------------------
# Step C7. OLS -> LM tests -> spatial models
# -----------------------------------------------------------------------------
spatial_form <- ln_earn_fb ~ pct_foreign_born + pct_lang_isolated +
  pct_naturalized_of_fb + pct_mgmt_prof + pct_service_occ +
  pct_female_head + pct_rent_burdened

ols_fit <- lm(spatial_form, data = spatial_df)
cat("\n=== OLS baseline ===\n")
print(summary(ols_fit))
cat("\nMoran's I on OLS residuals:\n")
print(lm.morantest(ols_fit, lw))

cat("\n=== Lagrange Multiplier tests ===\n")
lm_tests <- lm.RStests(ols_fit, lw,
                       test = c("RSerr", "RSlag", "adjRSerr", "adjRSlag"))
print(summary(lm_tests))

slm_fit <- lagsarlm(spatial_form, data = spatial_df, listw = lw)
sem_fit <- errorsarlm(spatial_form, data = spatial_df, listw = lw)
sdm_fit <- lagsarlm(spatial_form, data = spatial_df, listw = lw,
                    type = "mixed")

cat("\n=== Model comparison (AIC) ===\n")
aic_tbl <- data.frame(
  model = c("OLS", "Spatial lag (SLM)", "Spatial error (SEM)",
            "Spatial Durbin (SDM)"),
  AIC   = round(c(AIC(ols_fit), AIC(slm_fit), AIC(sem_fit), AIC(sdm_fit)), 1)
)
print(aic_tbl)
write_csv(aic_tbl, "output/7_spatial_model_aic.csv")

cat("\n=== SDM summary ===\n")
print(summary(sdm_fit))
cat("\nLR test: SDM vs SEM (common factor restriction):\n")
print(LR.Sarlm(sdm_fit, sem_fit))

saveRDS(ols_fit, "output/8_ols_fit.rds")
saveRDS(slm_fit, "output/8_slm_fit.rds")
saveRDS(sem_fit, "output/8_sem_fit.rds")
saveRDS(sdm_fit, "output/8_sdm_fit.rds")

# -----------------------------------------------------------------------------
# Step C8. SDM impacts decomposition (direct / indirect / total)
# -----------------------------------------------------------------------------
W_tr <- as(lw, "CsparseMatrix")
sdm_impacts <- impacts(sdm_fit, tr = trW(W_tr, type = "mult"), R = 500)
cat("\n=== SDM impacts (direct / indirect / total) ===\n")
print(summary(sdm_impacts, zstats = TRUE, short = TRUE))

imp_sum <- summary(sdm_impacts, zstats = TRUE, short = TRUE)
impacts_df <- data.frame(
  variable = attr(sdm_impacts, "bnames"),
  direct   = round(sdm_impacts$res$direct, 4),
  indirect = round(sdm_impacts$res$indirect, 4),
  total    = round(sdm_impacts$res$total, 4)
)
print(impacts_df)
write_csv(impacts_df, "output/9_sdm_impacts.csv")

impacts_long <- impacts_df %>%
  pivot_longer(cols = c(direct, indirect, total),
               names_to = "impact_type", values_to = "estimate") %>%
  mutate(
    impact_type = factor(str_to_title(impact_type),
                         levels = c("Direct", "Indirect", "Total")),
    var_lab = case_match(variable,
                         "pct_foreign_born"      ~ "% foreign-born",
                         "pct_lang_isolated"     ~ "% lang. isolated",
                         "pct_naturalized_of_fb" ~ "% naturalized (of FB)",
                         "pct_mgmt_prof"         ~ "% mgmt/professional occ.",
                         "pct_service_occ"       ~ "% service occ.",
                         "pct_female_head"       ~ "% female-headed HH",
                         "pct_rent_burdened"     ~ "% rent-burdened",
                         .default = variable)
  )

p_impacts <- ggplot(impacts_long,
                    aes(x = estimate, y = var_lab, fill = impact_type)) +
  geom_col(width = 0.68, position = position_dodge(width = 0.75)) +
  geom_vline(xintercept = 0, color = gray_dark, linewidth = 0.5) +
  scale_fill_manual(values = c(
    "Direct"   = accent_burgundy,
    "Indirect" = as.character(artsy["mustard"]),
    "Total"    = as.character(artsy["teal"])
  )) +
  labs(
    title    = "Neighborhood spillovers in foreign-born earnings",
    subtitle = "Spatial Durbin model impacts on log median FB earnings: own-tract (direct) vs neighbor (indirect) effects",
    x = "Impact on log median FB earnings", y = NULL,
    caption  = paste0("Source: ACS 5-year estimates (2024 vintage). N = ",
                      nrow(spatial_df), " tracts (LF >= 50, CV <= 0.40), ",
                      "Queen weights with k=1 island patch.\n",
                      "Simulated impacts (R = 500). Ecological associations, ",
                      "not causal effects; z-stats in console output.")
  )
print(p_impacts)
ggsave("output/chart12_sdm_impacts.png", p_impacts,
       width = 10, height = 7, dpi = 300)

# -----------------------------------------------------------------------------
# Final: session info for reproducibility
# -----------------------------------------------------------------------------
writeLines(capture.output(sessionInfo()), "output/session_info.txt")
cat("\n=== v5 run complete ===\n")