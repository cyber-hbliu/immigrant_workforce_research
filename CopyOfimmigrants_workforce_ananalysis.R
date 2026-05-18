# IMMIGRANT EMPLOYMENT & ECONOMIC MOBILITY IN PHILADELPHIA
# analysis pipeline 

# -----------------------------------------------------------------------------
# Step 1. Load libraries
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


# -----------------------------------------------------------------------------
# Step 2. Editorial palette ("sesame cake" tones) and ggplot theme
# -----------------------------------------------------------------------------
# Variable names kept the same as the prior version for backward compatibility.
# Hex codes come from the sesame-cake reference image: fig pink (warm alarm),
# muscat green (cool/calm), blueberry (deep cool), mint, sesame gray.

artsy <- c(
  burgundy   = "#c98590",   # fig — primary alarm/accent
  terracotta = "#7d7676",   # sesame gray
  mustard    = "#c5d68a",   # muscat green — secondary accent
  sage       = "#8fa56b",   # mint leaf
  teal       = "#3a3f5e",   # blueberry — primary cool
  rose       = "#c98590"    # fig (alias)
)

accent_burgundy <- as.character(artsy["burgundy"])   # fig
accent_teal     <- as.character(artsy["teal"])       # blueberry

ink         <- "#2a2522"   # oreo cookie black
gray_dark   <- "#3b3b3b"
gray_mid    <- "#888888"
gray_light  <- "#dddddd"
paper       <- "#f5f0e8"   # cream (kept for reference; not used as bg)

# Sequential ramp — cream → fig (light → intense)
ramp_seq <- c("#f5f0e8", "#ecd1d5", "#dca8b0", "#c98590", "#a86670")

# Diverging ramp — fig ↔ cream ↔ muscat (high concerning ↔ low cool)
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

# Reusable map theme — used by every standalone map below
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
# Step 3. Project constants and output folder
# -----------------------------------------------------------------------------
philly_pumas <- c("03216", "03221", "03222", "03223", "03224",
                  "03225", "03227", "03228", "03229", "03230", "03231")

acs_year   <- 2024
lodes_year <- 2023

options(tigris_use_cache = TRUE, scipen = 999)
set.seed(2025)

if (!dir.exists("output")) dir.create("output")


# -----------------------------------------------------------------------------
# Step 4. Pull ACS PUMS for Pennsylvania
# -----------------------------------------------------------------------------
pums_vars <- c(
  "AGEP", "SEX", "NATIVITY", "SCHL", "ESR", "CIT", "POBP", "POVPIP",
  "NP", "WAGP", "INDP", "NAICSP", "OCCP", "ENG", "HHL", "LNGI", "YOEP",
  "PUMA", "COW", "WAOB", "HHLDRRAC1P", "MAR", "MIG", "HHT2", "HINCP"
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
cat("Foreign-born:",       sum(pums_philly$NATIVITY == 2), "\n")
#Foreign-born: 5955 
tract_vars <- c(
  total_pop          = "B05002_001",
  total_foreign_born = "B05002_013",
  naturalized        = "B05002_014",
  noncitizen         = "B05002_021",
  fb_pov_universe    = "B06012_017",
  fb_below_pov       = "B06012_018",
  total_hh           = "C16002_001",
  iso_spanish        = "C16002_004",
  iso_indo_euro      = "C16002_007",
  iso_api            = "C16002_010",
  iso_other          = "C16002_013",
  median_earn_fb     = "B20017_003",
  lf_total           = "B23025_002",
  lf_employed        = "B23025_004",
  lf_unemployed      = "B23025_005",
  hispanic           = "B03003_003",
  housing_total      = "B25003_001",
  owner_occ          = "B25003_002",
  median_gross_rent  = "B25064_001",
  median_hh_income   = "B19013_001",
  
  # Household type counts (B11001)
  hh_total            = "B11001_001",
  hh_married_couple   = "B11001_003",
  hh_female_no_spouse = "B11001_006",
  
  # Occupation counts (C24010)
  occ_total           = "C24010_001",
  occ_mgmt_prof       = "C24010_003",
  occ_service         = "C24010_019",
  
  # Workforce engagement (B23025)
  pop_16plus          = "B23025_001",
  
  # Renter cost burden (B25070)
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
# Step 7. Compute derived tract indicators
# -----------------------------------------------------------------------------
tracts <- tracts_raw
tracts$pct_foreign_born <- tracts$total_foreign_bornE / tracts$total_popE * 100
tracts$pct_poverty_fb <- ifelse(
  tracts$fb_pov_universeE > 0,
  tracts$fb_below_povE / tracts$fb_pov_universeE * 100, NA
)

tracts$total_isolated <- tracts$iso_spanishE + tracts$iso_indo_euroE +
  tracts$iso_apiE + tracts$iso_otherE
tracts$pct_lang_isolated <- ifelse(
  tracts$total_hhE > 0,
  tracts$total_isolated / tracts$total_hhE * 100, NA
)

tracts$unemp_rate <- ifelse(
  tracts$lf_totalE > 0,
  tracts$lf_unemployedE / tracts$lf_totalE * 100, NA
)

tracts$pct_homeowner <- ifelse(
  tracts$housing_totalE > 0,
  tracts$owner_occE / tracts$housing_totalE * 100, NA
)
tracts$pct_married_couple <- ifelse(
  tracts$hh_totalE > 0,
  tracts$hh_married_coupleE / tracts$hh_totalE * 100, NA)

tracts$pct_female_head <- ifelse(
  tracts$hh_totalE > 0,
  tracts$hh_female_no_spouseE / tracts$hh_totalE * 100, NA)

tracts$pct_mgmt_prof <- ifelse(
  tracts$occ_totalE > 0,
  tracts$occ_mgmt_profE / tracts$occ_totalE * 100, NA)

tracts$pct_service_occ <- ifelse(
  tracts$occ_totalE > 0,
  tracts$occ_serviceE / tracts$occ_totalE * 100, NA)

tracts$pct_lfp <- ifelse(
  tracts$pop_16plusE > 0,
  tracts$lf_totalE / tracts$pop_16plusE * 100, NA)

tracts$pct_emp_to_pop <- ifelse(
  tracts$pop_16plusE > 0,
  tracts$lf_employedE / tracts$pop_16plusE * 100, NA)

tracts$pct_rent_burdened <- ifelse(
  tracts$rent_burden_universeE > 0,
  (tracts$rent_30_pctE + tracts$rent_35_pctE +
     tracts$rent_40_pctE + tracts$rent_50_pctE) /
    tracts$rent_burden_universeE * 100, NA)
tracts$pct_married_couple_hh <- ifelse(
  tracts$total_hhE > 0,
  tracts$hh_married_coupleE / tracts$total_hhE * 100, NA) 

summary(tracts$pct_foreign_born)
summary(tracts$pct_poverty_fb)
summary(tracts$pct_lang_isolated)


# -----------------------------------------------------------------------------
# Step 8. Pull 11-year foreign-born population trend
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
philly_trend$yoy_change <- c(NA,
                             philly_trend$foreign_bornE[-1] /
                               philly_trend$foreign_bornE[-nrow(philly_trend)] - 1
) * 100

print(philly_trend[, c("year", "foreign_bornE", "pct_foreign_born", "yoy_change")])


# -----------------------------------------------------------------------------
# Step 9. Chart 1 — population trend (saves PNG)
# -----------------------------------------------------------------------------
# The 2023 dip is the only YoY decline in the series; 2024 rebounded.
dip_year_row <- philly_trend[which.min(philly_trend$yoy_change), ]
p1_trend <- ggplot(philly_trend, aes(x = year, y = foreign_bornE)) +
  geom_line(color = ink, linewidth = 0.7) +
  geom_point(aes(color = year == dip_year_row$year), size = 3) +
  geom_text(aes(label = scales::comma(foreign_bornE)),
            vjust = -1.3, size = 3.0, color = gray_dark, family = "sans") +
  annotate("curve",
           x = 2021.3, xend = dip_year_row$year - 0.1,
           y = dip_year_row$foreign_bornE - 9000,
           yend = dip_year_row$foreign_bornE - 2500,
           curvature = 0.3,
           arrow = arrow(length = unit(2, "mm"), type = "closed"),
           color = accent_burgundy, linewidth = 0.4) +
  annotate("text",
           x = 2021.3, y = dip_year_row$foreign_bornE - 12500,
           label = sprintf("Only YoY decline in the series\n%.1f%% in 2023, rebounding in 2024",
                           dip_year_row$yoy_change),
           hjust = 1, size = 3.4, lineheight = 0.95,
           color = accent_burgundy, family = "sans", fontface = "bold") +
  scale_color_manual(values = c("TRUE" = accent_burgundy, "FALSE" = ink),
                     guide = "none") +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0.08, 0.12))) +
  scale_x_continuous(breaks = years_to_pull) +
  labs(
    title    = "A decade of growth, with one dip in 2023",
    subtitle = "Philadelphia County foreign-born population, 2014-2024",
    x = NULL, y = NULL,
    caption  = "Source: U.S. Census Bureau, ACS 5-year estimates."
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
table(pums_philly$eng_factor, useNA = "ifany")


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
table(pums_philly$edu_collapsed, useNA = "ifany")


# -----------------------------------------------------------------------------
# Step 12. Employment status (3-category) and foreign-born flag
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
table(pums_philly$waob_lab, useNA = "ifany")


# -----------------------------------------------------------------------------
# Step 14. Mincer regressors
# -----------------------------------------------------------------------------
pums_philly$age_num <- as.numeric(pums_philly$AGEP)
pums_philly$age_sq  <- pums_philly$age_num^2

yoep_num <- suppressWarnings(as.numeric(as.character(pums_philly$YOEP)))
pums_philly$yrs_us <- pmax(acs_year - yoep_num, 0)

pums_philly$wage_num <- as.numeric(pums_philly$WAGP)

# -----------------------------------------------------------------------------
# Step 14b. Feature engineering: 3-digit NAICS subsector + detailed COW
# -----------------------------------------------------------------------------
# --- Industry subsector (NAICSP first 3 digits = NAICS subsector code) -------
naicsp_str <- as.character(pums_philly$NAICSP)
naicsp_3   <- substr(naicsp_str, 1, 3)

# 3-digit NAICS subsector labels (only ones likely to appear in Philly are
naicsp3_to_subsector <- c(
  # 2-digit sector-level codes (used when subsector detail not captured)
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
  
  # 3-digit subsectors — Agriculture & Mining
  "111" = "Crop Production",
  "112" = "Animal Production",
  "115" = "Agriculture Support Services",
  "211" = "Oil and Gas Extraction",
  
  # Construction subsectors
  "236" = "Building Construction",
  "237" = "Heavy/Civil Engineering Construction",
  "238" = "Specialty Trade Contractors",
  
  # Manufacturing subsectors
  "311" = "Food Manufacturing",
  "312" = "Beverage/Tobacco Manufacturing",
  "313" = "Textile Mills",
  "314" = "Textile Product Mills",
  "315" = "Apparel Manufacturing",
  "316" = "Leather Manufacturing",
  "321" = "Wood Product Manufacturing",
  "322" = "Paper Manufacturing",
  "323" = "Printing/Related Support",
  "324" = "Petroleum/Coal Products",
  "325" = "Chemical Manufacturing",
  "326" = "Plastics/Rubber Products",
  "327" = "Nonmetallic Mineral Products",
  "331" = "Primary Metal Manufacturing",
  "332" = "Fabricated Metal Products",
  "333" = "Machinery Manufacturing",
  "334" = "Computer/Electronic Products",
  "335" = "Electrical Equipment",
  "336" = "Transportation Equipment",
  "337" = "Furniture Manufacturing",
  "339" = "Miscellaneous Manufacturing",
  
  # Wholesale
  "423" = "Wholesale Durable Goods",
  "424" = "Wholesale Nondurable Goods",
  "425" = "Wholesale Electronic Markets",
  
  # Retail — NAICS 2022 reorganization
  "441" = "Motor Vehicle/Parts Dealers",
  "444" = "Building Material/Garden Stores",
  "445" = "Food and Beverage Stores",
  "449" = "Furniture/Home Furnishings/Electronics",
  "455" = "General Merchandise Retailers",
  "456" = "Health and Personal Care Retailers",
  "457" = "Gasoline Stations/Fuel Dealers",
  "458" = "Clothing/Apparel Retailers",
  "459" = "Sporting Goods/Hobby/Book/Misc Retailers",
  
  # Transportation
  "481" = "Air Transportation",
  "482" = "Rail Transportation",
  "483" = "Water Transportation",
  "484" = "Truck Transportation",
  "485" = "Transit/Ground Passenger Transport",
  "486" = "Pipeline Transportation",
  "487" = "Scenic/Sightseeing Transport",
  "488" = "Transportation Support Activities",
  "491" = "Postal Service",
  "492" = "Couriers and Messengers",
  "493" = "Warehousing and Storage",
  
  # Information — NAICS 2022 reorganization
  "513" = "Publishing Industries",
  "516" = "Broadcasting/Content Providers",
  "518" = "Computing Infrastructure/Data Processing",
  "519" = "Web Search/Other Information Services",
  
  # Finance/Insurance
  "522" = "Credit Intermediation",
  "523" = "Securities/Commodities/Investments",
  "524" = "Insurance Carriers",
  "525" = "Funds, Trusts, Other Vehicles",
  
  # Real Estate
  "531" = "Real Estate",
  "532" = "Rental and Leasing Services",
  "533" = "Lessors of Nonfinancial Intangibles",
  
  # Professional Services
  "541" = "Professional/Scientific/Technical",
  "551" = "Management of Companies",
  "561" = "Administrative/Support Services",
  "562" = "Waste Management/Remediation",
  
  # Education + Health
  "611" = "Educational Services",
  "621" = "Ambulatory Health Care",
  "622" = "Hospitals",
  "623" = "Nursing/Residential Care",
  "624" = "Social Assistance",
  
  # Arts + Hospitality
  "711" = "Performing Arts/Spectator Sports",
  "712" = "Museums/Historical Sites",
  "713" = "Amusement/Gambling/Recreation",
  "721" = "Accommodation",
  "722" = "Food Services/Drinking Places",
  
  # Other Services
  "811" = "Repair and Maintenance",
  "812" = "Personal and Laundry Services",
  "813" = "Religious/Civic Organizations",
  "814" = "Private Households",
  
  # Public Administration
  "921" = "Executive/Legislative Gov",
  "922" = "Justice/Public Order/Safety",
  "923" = "Human Resource Programs Admin",
  "924" = "Environmental Quality Admin",
  "925" = "Housing Programs Admin",
  "926" = "Economic Programs Admin",
  "927" = "Space Research/Technology",
  "928" = "National Security/Intl Affairs"
)

pums_philly$subsector <- naicsp3_to_subsector[naicsp_3]
unmapped <- is.na(pums_philly$subsector)
pums_philly$subsector[unmapped] <-
  naicsp3_to_subsector[naicsp_2[unmapped]]
pums_philly$subsector[is.na(pums_philly$subsector)] <- naicsp_3[is.na(pums_philly$subsector)]
pums_philly$subsector[naicsp_str == "0" | naicsp_str == ""] <-
  "Not in labor force / Military"
still_unmapped <- is.na(pums_philly$subsector)
pums_philly$subsector[still_unmapped] <-
  paste0("NAICS ", naicsp_str[still_unmapped])
pums_philly$subsector[naicsp_str == "0" |
                        naicsp_str == "" |
                        is.na(naicsp_str)] <-
  "Not in labor force / Military"

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
  levels = c("Private for-profit employee",
             "Private nonprofit employee",
             "Local government employee",
             "State government employee",
             "Federal government employee",
             "Self-employed (incorporated)",
             "Self-employed (unincorporated)",
             "Unpaid family worker")
)

# Re-build foreign_born subset to pick up new columns
foreign_born <- pums_philly[pums_philly$foreign_born, ]

# Quick checks
cat("Top 10 subsectors (foreign-born workers):\n")
print(sort(table(foreign_born$subsector[foreign_born$ESR %in% c("1","2")]),
           decreasing = TRUE)[1:10])
cat("\nClass of worker distribution (foreign-born workers):\n")
print(table(foreign_born$cow_detailed[foreign_born$ESR %in% c("1","2")]))


# -----------------------------------------------------------------------------
# Step 14c. Feature engineering: occupation (SOC prefix), citizenship,
#           household type
# -----------------------------------------------------------------------------
# OCCP code ranges verified against PUMS 2023 Data Dictionary pages 88-98.
# 25 SOC occupational groups identified by their 3-letter dictionary prefixes.
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

# --- Citizenship status (CIT) ------------------------------------------------
# Verified per PUMS dictionary:
# 1 = Born in US        2 = Born in PR/territory
# 3 = Born abroad of US parents   4 = Naturalized   5 = Non-citizen
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
             "Born abroad of U.S. parents",
             "Naturalized citizen", "Not a U.S. citizen")
)

# Binary version for the foreign-born subset
pums_philly$is_naturalized <- case_when(
  cit_num == 4 ~ "Naturalized",
  cit_num == 5 ~ "Non-citizen",
  TRUE         ~ NA_character_
)

# --- Household type (HHT2 — 12 categories with cohabiting) -------------------
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
    "Married couple, with children <18",
    "Married couple, no children <18",
    "Cohabiting couple, with children <18",
    "Cohabiting couple, no children <18",
    "Female head, with children <18",
    "Female head, with other relatives",
    "Female head, with nonrelatives only",
    "Female head, living alone",
    "Male head, with children <18",
    "Male head, with other relatives",
    "Male head, with nonrelatives only",
    "Male head, living alone"
  )
)

# Three-level household structure super-group (for regression use)
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
  levels = c("Coupled households",
             "Single householder with family",
             "Nonfamily / solo households")
)
foreign_born <- pums_philly[pums_philly$foreign_born, ]
# Quick checks
cat("Occupation SOC group (foreign-born workers):\n")
print(sort(table(foreign_born$occ_soc[foreign_born$ESR %in% c("1","2")]),
           decreasing = TRUE))
cat("\nCitizenship (all foreign-born):\n")
print(table(foreign_born$citizenship))
cat("\nHousehold type (foreign-born householders):\n")
print(table(foreign_born$hh_type))

# -----------------------------------------------------------------------------
# Step 15. Foreign-born subset
# -----------------------------------------------------------------------------
foreign_born <- pums_philly[pums_philly$foreign_born, ]
cat("Foreign-born records:", nrow(foreign_born), "\n")
cat("Foreign-born weighted population:",
    scales::comma(sum(foreign_born$PWGTP)), "\n")


# -----------------------------------------------------------------------------
# Step 15b. Birthplace breakdown — country of origin treemap (saves PNG)
# -----------------------------------------------------------------------------
pobp_codes <- c(
  "207" = "China",      "210" = "Hong Kong",  "215" = "Korea",
  "217" = "India",      "240" = "Pakistan",   "242" = "Bangladesh",
  "247" = "Vietnam",    "248" = "Cambodia",   "211" = "Indonesia",
  "245" = "Philippines","203" = "Burma/Myanmar","209" = "Taiwan",
  "138" = "Italy",      "148" = "Poland",     "150" = "Portugal",
  "156" = "Ukraine",    "126" = "Ireland",
  "120" = "United Kingdom", "127" = "Albania",
  "303" = "Mexico",     "311" = "Belize",     "327" = "Honduras",
  "328" = "Nicaragua",  "329" = "Panama",     "375" = "Dominican Republic",
  "337" = "Cuba",       "316" = "El Salvador","317" = "Guatemala",
  "338" = "Haiti",      "330" = "Jamaica",
  "374" = "Colombia",   "390" = "Ecuador",    "393" = "Peru",  "381" = "Brazil",
  "414" = "Cabo Verde", "421" = "Ethiopia",   "427" = "Ghana", "436" = "Liberia",
  "440" = "Nigeria",    "444" = "Senegal",    "451" = "Egypt", "455" = "Sudan"
)

foreign_born$country_label <- pobp_codes[as.character(foreign_born$POBP)]
foreign_born$country_label[is.na(foreign_born$country_label)] <- "Other"

# Build top 15 with weighted populations and region tags
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

# Region colors using the sesame-cake palette
region_colors <- c(
  "Asia"             = as.character(artsy["burgundy"]),   # fig
  "Latin America"    = as.character(artsy["teal"]),       # blueberry
  "Europe"           = as.character(artsy["mustard"]),    # muscat
  "Africa"           = as.character(artsy["sage"]),       # mint
  "Northern America" = as.character(artsy["terracotta"]), # sesame
  "Oceania"          = "#a86670",                          # dark fig
  "PR/US Islands"    = gray_mid
)

p_origins <- ggplot(top_countries,
                    aes(area = weighted_pop,
                        fill = waob_lab,
                        label = label,
                        subgroup = waob_lab)) +
  geom_treemap(color = "white", linewidth = 3) +
  geom_treemap_subgroup_border(color = "white", linewidth = 4) +
  geom_treemap_text(color = "white", place = "centre",
                    grow = FALSE, reflow = TRUE,
                    family = "sans", fontface = "bold", size = 11) +
  scale_fill_manual(values = region_colors, name = "World region") +
  labs(
    title    = "Where Philadelphia's immigrants come from",
    subtitle = "Top 15 countries of birth, sized by weighted PUMS population, grouped by region",
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(foreign_born)),
                      " foreign-born records.\n",
                      "Tiles sized by population estimate; color = world region. ",
                      "Smaller origin countries grouped as 'Other' and not shown.")
  ) +
  theme(
    legend.position = "top",
    panel.grid      = element_blank(),
    axis.line.x     = element_blank(),
    axis.ticks.x    = element_blank(),
    axis.text       = element_blank(),
    axis.title      = element_blank()
  )

print(p_origins)
ggsave("output/chart_origin_treemap.png", p_origins,
       width = 10, height = 9, dpi = 300)


# -----------------------------------------------------------------------------
# Step 16. Chart 2 — wage by English (saves PNG)
# -----------------------------------------------------------------------------
employed_fb <- foreign_born[
  foreign_born$ESR %in% c("1", "2") & foreign_born$wage_num > 1000, ]

wage_by_eng <- employed_fb %>%
  group_by(eng_factor) %>%
  summarise(
    mean_wage = weighted.mean(wage_num, PWGTP, na.rm = TRUE),
    n_records = n(),
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
    title = sprintf("The $%s raw language gap",
                    scales::comma(round(gap_dollars, -2))),
    subtitle = "Average annual wages, employed foreign-born, Philadelphia",
    x = NULL, y = NULL,
    caption = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                     scales::comma(nrow(employed_fb)),
                     " employed foreign-born records with positive wages.")
  )

print(p2_wage)
ggsave("output/chart2_wage_by_eng.png", p2_wage,
       width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Step 16b. Industry subsector distribution — FB vs US-born (top 15)
# -----------------------------------------------------------------------------
workers_only <- pums_philly[
  pums_philly$ESR %in% c("1", "2") &
    !is.na(pums_philly$subsector) &
    pums_philly$subsector != "Not in labor force / Military",
]

industry_dist <- workers_only %>%
  group_by(subsector) %>%
  summarise(
    fb_workers     = sum(PWGTP[foreign_born], na.rm = TRUE),
    us_workers     = sum(PWGTP[!foreign_born], na.rm = TRUE),
    total_workers  = sum(PWGTP, na.rm = TRUE),
    n_records      = n(),
    .groups        = "drop"
  ) %>%
  filter(n_records >= 20) %>%        # suppress tiny-sample subsectors
  mutate(
    sector_share_of_fb = fb_workers / sum(fb_workers) * 100,
    sector_share_of_us = us_workers / sum(us_workers) * 100,
    concentration_idx  = sector_share_of_fb / pmax(sector_share_of_us, 0.1)
  ) %>%
  arrange(desc(sector_share_of_fb)) %>%
  slice_head(n = 15)

industry_long <- industry_dist %>%
  select(subsector, `Foreign-born` = sector_share_of_fb,
         `U.S.-born`    = sector_share_of_us) %>%
  pivot_longer(cols = -subsector, names_to = "group", values_to = "share")

industry_long$subsector <- factor(
  industry_long$subsector,
  levels = rev(industry_dist$subsector)
)

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
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). ",
                      "N = ", scales::comma(nrow(workers_only)),
                      " employed workers.\n",
                      "Bars compare each group's distribution across subsectors. ",
                      "Where bars diverge = immigrant-intensive subsector.")
  )

print(p_industry)
ggsave("output/chart_industry_fb_vs_us.png", p_industry,
       width = 10, height = 14, dpi = 300)

# -----------------------------------------------------------------------------
# Step 16c. Class of worker by origin region (full 8-category detail)
# -----------------------------------------------------------------------------
cow_by_origin <- foreign_born %>%
  filter(!is.na(cow_detailed),
         waob_lab %in% c("Asia", "Latin America", "Europe", "Africa")) %>%
  group_by(waob_lab, cow_detailed) %>%
  summarise(weighted_n = sum(PWGTP, na.rm = TRUE), .groups = "drop") %>%
  group_by(waob_lab) %>%
  mutate(pct = weighted_n / sum(weighted_n) * 100) %>%
  ungroup()

# Sesame-cake palette — 8 distinct shades using the broader gamut
cow_palette <- c(
  "Private for-profit employee"    = as.character(artsy["teal"]),       # blueberry
  "Private nonprofit employee"     = "#6e7396",                          # lighter blueberry
  "Local government employee"      = as.character(artsy["sage"]),       # mint
  "State government employee"      = "#b4c98a",                          # lighter mint
  "Federal government employee"    = as.character(artsy["mustard"]),    # muscat
  "Self-employed (incorporated)"   = accent_burgundy,                    # fig
  "Self-employed (unincorporated)" = "#a86670",                          # dark fig
  "Unpaid family worker"           = gray_mid
)

p_cow <- ggplot(cow_by_origin,
                aes(x = waob_lab, y = pct, fill = cow_detailed)) +
  geom_col(width = 0.62, position = "stack") +
  geom_text(aes(label = ifelse(pct >= 5, sprintf("%.0f%%", pct), "")),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_manual(values = cow_palette) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.01, 0.03))) +
  labs(
    title    = "Class of worker by region of birth",
    subtitle = "Detailed worker classification, foreign-born Philadelphia",
    x = NULL, y = NULL, fill = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). ",
                      "Foreign-born workers only.\n",
                      "Self-employment (incorporated + unincorporated) varies sharply by origin — ",
                      "an immigrant mobility pattern not visible in employee-only analyses.")
  ) +
  theme(legend.position = "right",
        legend.text = element_text(size = 8)) +
  coord_flip()

print(p_cow)
ggsave("output/chart_cow_by_origin.png", p_cow,
       width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Step 16d. Median wage by subsector — foreign-born workers
# -----------------------------------------------------------------------------
wage_by_industry <- workers_only %>%
  filter(foreign_born, wage_num > 1000) %>%
  group_by(subsector) %>%
  summarise(
    median_wage = {
      ord  <- order(wage_num)
      vals <- wage_num[ord]
      wts  <- PWGTP[ord]
      vals[which(cumsum(wts) / sum(wts) >= 0.5)[1]]
    },
    n_records  = n(),
    weighted_n = sum(PWGTP, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  filter(n_records >= 30) %>%
  arrange(desc(weighted_n)) %>%
  slice_head(n = 15) %>%
  arrange(median_wage) %>%
  mutate(
    subsector = factor(subsector, levels = subsector),
    bar_color = colorRampPalette(ramp_seq)(nrow(.))
  )

p_wage_industry <- ggplot(wage_by_industry,
                          aes(x = median_wage, y = subsector,
                              fill = bar_color)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = scales::dollar(round(median_wage, -2))),
            hjust = -0.15, size = 3.0, color = ink, fontface = "bold") +
  scale_fill_identity() +
  scale_x_continuous(labels = scales::dollar,
                     expand = expansion(mult = c(0.02, 0.18))) +
  labs(
    title    = "Industry sets the wage ceiling for immigrants",
    subtitle = "Weighted median annual wages by 3-digit NAICS subsector, top 15 by employment",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). ",
                      "Subsectors with N < 30 records suppressed.\n",
                      "Ratio between top and bottom subsectors is the within-economy ",
                      "wage stretch immigrants face when switching industries.")
  )

print(p_wage_industry)
ggsave("output/chart_wage_by_industry.png", p_wage_industry,
       width = 10, height = 14, dpi = 300)

# -----------------------------------------------------------------------------
# Step 16f. Median wages by citizenship status × origin region
# -----------------------------------------------------------------------------
# Workforce-economic lens on citizenship: does naturalization correlate with
# higher wages, and does the gap differ by origin? Foreign-born employed only.
wage_cit_origin <- foreign_born %>%
  filter(ESR %in% c("1", "2"),
         wage_num > 1000,
         !is.na(is_naturalized),
         waob_lab %in% c("Asia", "Latin America", "Europe", "Africa")) %>%
  group_by(waob_lab, is_naturalized) %>%
  summarise(
    median_wage = {
      ord  <- order(wage_num)
      vals <- wage_num[ord]
      wts  <- PWGTP[ord]
      vals[which(cumsum(wts) / sum(wts) >= 0.5)[1]]
    },
    n_records = n(),
    .groups   = "drop"
  ) %>%
  filter(n_records >= 30)

p_wage_cit <- ggplot(wage_cit_origin,
                     aes(x = median_wage, y = waob_lab,
                         fill = is_naturalized)) +
  geom_col(width = 0.62, position = position_dodge(width = 0.7)) +
  geom_text(aes(label = scales::dollar(round(median_wage, -2))),
            position = position_dodge(width = 0.7),
            hjust = -0.15, size = 3.2, color = ink, fontface = "bold") +
  scale_fill_manual(values = c(
    "Naturalized" = as.character(artsy["mustard"]),
    "Non-citizen" = accent_burgundy
  )) +
  scale_x_continuous(labels = scales::dollar,
                     expand = expansion(mult = c(0.02, 0.20))) +
  labs(
    title    = "Wages by citizenship and origin region",
    subtitle = "Weighted median annual wages, employed foreign-born workers",
    x = NULL, y = NULL, fill = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). ",
                      "Employed foreign-born only; cells with N < 30 suppressed.\n",
                      "Compare bars within each region to see the naturalization wage gap; ",
                      "compare across regions to see origin effects.")
  ) +
  theme(legend.position = "top")

print(p_wage_cit)
ggsave("output/chart_wage_by_citizenship.png", p_wage_cit,
       width = 10, height = 5, dpi = 300)
# -----------------------------------------------------------------------------
# Step 16g. Household type composition by origin region
# -----------------------------------------------------------------------------
hincp_num <- suppressWarnings(as.numeric(as.character(pums_philly$HINCP)))
foreign_born$hh_income <- suppressWarnings(
  as.numeric(as.character(foreign_born$HINCP))
)
fb_households <- foreign_born %>%
  filter(!is.na(hh_type), !is.na(hh_income), hh_income > 0) %>%
  distinct(SERIALNO, .keep_all = TRUE)

income_by_hh <- fb_households %>%
  filter(!is.na(hh_type), !is.na(hh_income), hh_income > 0) %>%
  group_by(hh_type) %>%
  summarise(
    median_income = {
      ord  <- order(hh_income)
      vals <- hh_income[ord]
      wts  <- WGTP[ord]
      vals[which(cumsum(wts) / sum(wts) >= 0.5)[1]]
    },
    n_records = n(),
    .groups   = "drop"
  ) %>%
  filter(n_records >= 20) %>%
  mutate(
    hh_supergroup = case_when(
      grepl("^(Married|Cohabiting) couple", hh_type) ~
        "Coupled households",
      grepl("(with children <18|with other relatives)$", hh_type) ~
        "Single householder with family",
      grepl("(living alone|nonrelatives only)$", hh_type) ~
        "Nonfamily / solo households",
      TRUE ~ "Other"
    ),
    hh_supergroup = factor(hh_supergroup, levels = c(
      "Coupled households",
      "Single householder with family",
      "Nonfamily / solo households"
    ))
  ) %>%
  arrange(median_income) %>%
  mutate(
    hh_type   = factor(hh_type, levels = hh_type),
    bar_color = colorRampPalette(ramp_seq)(nrow(.))
  )

p_income_hh <- ggplot(income_by_hh,
                      aes(x = median_income, y = hh_type, fill = bar_color)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::dollar(round(median_income, -3))),
            hjust = -0.15, size = 3.2, color = ink, fontface = "bold") +
  scale_fill_identity() +
  scale_x_continuous(labels = scales::dollar,
                     expand = expansion(mult = c(0.02, 0.22))) +
  facet_grid(rows = vars(hh_supergroup),
             scales = "free_y", space = "free_y", switch = "y") +
  labs(
    title    = "Median household income by household structure",
    subtitle = "Foreign-born households, grouped by partnership status and family composition",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). ",
                      "Household-weighted medians; HH types with N < 20 suppressed.\n",
                      "Color reflects overall income rank across all 12 categories. ",
                      "Each panel groups four HHT2 categories.")
  ) +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", color = ink,
                                     size = 10, hjust = 0,
                                     margin = margin(r = 8)),
    strip.placement   = "outside",
    strip.background  = element_blank(),
    panel.spacing.y   = unit(1.5, "lines"),
    axis.text.y       = element_text(size = 9)
  )

print(p_income_hh)
ggsave("output/chart_hh_income_by_type.png", p_income_hh,
       width = 10, height = 13, dpi = 300)
# -----------------------------------------------------------------------------
# Step 16h. Employment status by citizenship and household type
# -----------------------------------------------------------------------------
esr_by_hh <- foreign_born %>%
  filter(!is.na(hh_type),
         !is.na(esr3),
         age_num >= 16, age_num <= 65) %>%
  group_by(hh_type, esr3) %>%
  summarise(weighted_n = sum(PWGTP, na.rm = TRUE), .groups = "drop") %>%
  group_by(hh_type) %>%
  mutate(
    pct        = weighted_n / sum(weighted_n) * 100,
    hh_total_n = sum(weighted_n)
  ) %>%
  ungroup() %>%
  filter(hh_total_n > 0) %>%
  mutate(
    hh_supergroup = case_when(
      grepl("^(Married|Cohabiting) couple", hh_type) ~
        "Coupled households",
      grepl("(with children <18|with other relatives)$", hh_type) ~
        "Single householder with family",
      grepl("(living alone|nonrelatives only)$", hh_type) ~
        "Nonfamily / solo households"
    ),
    hh_supergroup = factor(hh_supergroup, levels = c(
      "Coupled households",
      "Single householder with family",
      "Nonfamily / solo households"
    ))
  )

# Preserve HHT2 ordering inside each facet
esr_by_hh$hh_type <- factor(esr_by_hh$hh_type, levels = c(
  "Married couple, with children <18",
  "Married couple, no children <18",
  "Cohabiting couple, with children <18",
  "Cohabiting couple, no children <18",
  "Female head, with children <18",
  "Female head, with other relatives",
  "Male head, with children <18",
  "Male head, with other relatives",
  "Female head, living alone",
  "Female head, with nonrelatives only",
  "Male head, living alone",
  "Male head, with nonrelatives only"
))

p_esr_hh <- ggplot(esr_by_hh,
                   aes(x = pct, y = hh_type, fill = esr3)) +
  geom_col(width = 0.7, position = "stack") +
  geom_text(aes(label = ifelse(pct >= 4, sprintf("%.0f%%", pct), "")),
            position = position_stack(vjust = 0.5),
            size = 3.0, color = "white", fontface = "bold") +
  scale_fill_manual(values = c(
    "Employed"   = as.character(artsy["mustard"]),
    "Unemployed" = accent_burgundy,
    "Not in LF"  = as.character(artsy["teal"])
  )) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_discrete(limits = rev) +
  facet_grid(rows = vars(hh_supergroup),
             scales = "free_y", space = "free_y", switch = "y") +
  labs(
    title    = "Labor-force status across household structures",
    subtitle = "Working-age foreign-born Philadelphians, by HHT2 household type",
    x = NULL, y = NULL, fill = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). ",
                      "Foreign-born ages 16-65 only.\n",
                      "Each row sums to 100%. Three super-groups separate coupled, ",
                      "single-householder-family, and nonfamily/solo arrangements.")
  ) +
  theme(
    legend.position   = "top",
    strip.text.y.left = element_text(angle = 0, face = "bold", color = ink,
                                     size = 10, hjust = 0,
                                     margin = margin(r = 8)),
    strip.placement   = "outside",
    strip.background  = element_blank(),
    panel.spacing.y   = unit(1.5, "lines"),
    axis.text.y       = element_text(size = 9)
  )

print(p_esr_hh)
ggsave("output/chart_esr_by_hh.png", p_esr_hh,
       width = 10, height = 12, dpi = 300)
# -----------------------------------------------------------------------------
# Step 17. Tract-indicator correlation heatmap (saves PNG)
# -----------------------------------------------------------------------------
cor_df <- tracts %>%
  st_drop_geometry() %>%
  mutate(
    pct_naturalized_of_fb = ifelse(total_foreign_bornE > 0,
                                   naturalizedE / total_foreign_bornE * 100, NA),
    pct_hispanic          = ifelse(total_popE > 0,
                                   hispanicE / total_popE * 100, NA)
  ) %>%
  select(
    `% foreign-born`            = pct_foreign_born,
    `% naturalized (of FB)`     = pct_naturalized_of_fb,
    `% Hispanic`                = pct_hispanic,
    `% poverty (FB)`            = pct_poverty_fb,
    `Median FB earnings ($)`    = median_earn_fbE,
    `% lang. isolated`          = pct_lang_isolated,
    `Unemp. rate`               = unemp_rate,
    `Labor force partic. rate`  = pct_lfp,
    `Employment-to-pop rate`    = pct_emp_to_pop,
    `% married-couple HH`       = pct_married_couple_hh,
    `% homeowner`               = pct_homeowner,
    `% rent-burdened`           = pct_rent_burdened,
    `Median rent ($)`           = median_gross_rentE,
    `Median HH income ($)`      = median_hh_incomeE
  )

cor_matrix <- cor(cor_df, use = "pairwise.complete.obs")
p_cor <- ggcorrplot(
  cor_matrix,
  hc.order      = FALSE,
  type          = "lower",
  lab           = TRUE,
  lab_size      = 2.4,
  outline.color = "white",
  colors        = c("#c5d68a", "#f5f0e8", "#c98590"),
  ggtheme       = theme_editorial
) +
  labs(
    title    = "How workforce and socioeconomic indicators co-vary",
    subtitle = "Pearson correlations across 14 tract-level indicators, Philadelphia census tracts",
    caption  = paste0("Source: ACS 5-year estimates (2020-2024). ",
                      "N = ", nrow(cor_df), " tracts.\n",
                      "Fig pink = positive correlation, muscat green = negative. ",
                      "Look for clusters where immigrant indicators co-move with workforce/economic ones.")
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

print(p_cor)
ggsave("output/chart_correlation_heatmap.png", p_cor,
       width = 10, height = 11, dpi = 300)

# -----------------------------------------------------------------------------
# Step 17b. Individual-level correlation matrix — foreign-born residents
# -----------------------------------------------------------------------------
# All foreign-born individuals; no age filter. Categorical variables retained
# in their raw form by spreading them across binary indicators (one-hot)
# rather than collapsing to a single ordinal score. Class of worker replaces
# occupation flags to capture employer-type variation (private/government/
# self-employed) — a distinct axis from industry that maps onto immigrant
# labor-market access patterns.
fb_ind <- foreign_born %>%
  mutate(
    # English proficiency — one column per level
    eng_very_well   = as.numeric(eng_factor == "Very well"),
    eng_well        = as.numeric(eng_factor == "Well"),
    eng_not_well    = as.numeric(eng_factor == "Not well"),
    eng_not_at_all  = as.numeric(eng_factor == "Not at all"),
    
    # Education — one column per level
    edu_less_hs     = as.numeric(edu_collapsed == "<HS"),
    edu_hs_ged      = as.numeric(edu_collapsed == "HS/GED"),
    edu_some_col    = as.numeric(edu_collapsed == "Some college/Assoc."),
    edu_bachelors   = as.numeric(edu_collapsed == "Bachelor's+"),
    
    # Linguistic isolation flag at the individual level
    lang_isolated   = as.numeric(LNGI == "2"),
    
    # Demographics, citizenship
    is_female       = as.numeric(SEX == "2"),
    is_naturalized_num = as.numeric(is_naturalized == "Naturalized"),
    
    # Labor-force outcomes
    is_employed     = as.numeric(esr3 == "Employed"),
    in_lf           = as.numeric(esr3 %in% c("Employed", "Unemployed")),
    
    # Household structure — one column per supergroup
    hh_coupled      = as.numeric(hh_supergroup == "Coupled households"),
    hh_single_fam   = as.numeric(hh_supergroup == "Single householder with family"),
    hh_solo         = as.numeric(hh_supergroup == "Nonfamily / solo households"),
    
    # Industry concentration — flags for top immigrant-intensive sectors
    ind_health      = as.numeric(grepl("^(Ambulatory Health|Hospitals|Nursing/Resi)", subsector)),
    ind_food        = as.numeric(grepl("^Food (Services|and Beverage)", subsector)),
    ind_construction= as.numeric(grepl("^(Construction|Specialty Trade|Building Construction)", subsector)),
    ind_transport   = as.numeric(grepl("^(Truck|Transit/Ground|Couriers)", subsector)),
    ind_professional= as.numeric(grepl("^Professional/Scientific", subsector)),
    
    # Class of worker — flags for major employer categories
    cow_private_fp  = as.numeric(cow_detailed == "Private for-profit employee"),
    cow_private_np  = as.numeric(cow_detailed == "Private nonprofit employee"),
    cow_government  = as.numeric(cow_detailed %in% c(
      "Local government employee",
      "State government employee",
      "Federal government employee")),
    cow_self_inc    = as.numeric(cow_detailed == "Self-employed (incorporated)"),
    cow_self_unic   = as.numeric(cow_detailed == "Self-employed (unincorporated)"),
    
    # Wages and household income on log scale
    ln_wage_ind = ifelse(wage_num > 1000, log(wage_num), NA),
    ln_hh_inc   = ifelse(hh_income > 0,   log(hh_income), NA)
  ) %>%
  select(
    # Continuous demographics
    `Age`                            = age_num,
    `Years in U.S.`                  = yrs_us,
    
    # English (one-hot, "Not at all" omitted as reference)
    `English: Not well`              = eng_not_well,
    `English: Well`                  = eng_well,
    `English: Very well`             = eng_very_well,
    `Linguistically isolated`        = lang_isolated,
    
    # Education (one-hot, "<HS" omitted as reference)
    `Education: HS/GED`              = edu_hs_ged,
    `Education: Some college`        = edu_some_col,
    `Education: Bachelor's+`         = edu_bachelors,
    
    # Demographics & citizenship
    `Female`                         = is_female,
    `Naturalized`                    = is_naturalized_num,
    
    # Labor-force outcomes
    `In labor force`                 = in_lf,
    `Employed (in LF)`               = is_employed,
    
    # Industry concentration
    `Industry: Healthcare`           = ind_health,
    `Industry: Food`                 = ind_food,
    `Industry: Construction`         = ind_construction,
    `Industry: Transportation`       = ind_transport,
    `Industry: Prof/Scientific`      = ind_professional,
    
    # Class of worker
    `COW: Private for-profit`        = cow_private_fp,
    `COW: Private nonprofit`         = cow_private_np,
    `COW: Government`                = cow_government,
    `COW: Self-emp (incorporated)`   = cow_self_inc,
    `COW: Self-emp (unincorporated)` = cow_self_unic,
    
    # Household structure (one-hot, Coupled omitted as reference)
    `HH: Single head w/ family`      = hh_single_fam,
    `HH: Nonfamily / solo`           = hh_solo,
    
    # Economic outcomes
    `Log annual wage`                = ln_wage_ind,
    `Log household income`           = ln_hh_inc
  )

cor_matrix_ind <- cor(fb_ind, use = "pairwise.complete.obs")

p_cor_ind <- ggcorrplot(
  cor_matrix_ind,
  hc.order      = FALSE,
  type          = "lower",
  lab           = TRUE,
  lab_size      = 2.0,
  outline.color = "white",
  colors        = c("#c5d68a", "#f5f0e8", "#c98590"),
  ggtheme       = theme_editorial
) +
  labs(
    title    = "How individual characteristics co-vary among Philadelphia's immigrants",
    subtitle = "Pearson correlations across 27 person-level attributes, all foreign-born residents",
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). ",
                      "N = ", scales::comma(nrow(fb_ind)), " foreign-born individuals.\n",
                      "Categorical variables expanded into one-hot indicators. ",
                      "Reference categories (English 'Not at all', Education '<HS', ",
                      "HH 'Coupled') omitted to avoid mechanical collinearity.")
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

print(p_cor_ind)
ggsave("output/chart_correlation_individual.png", p_cor_ind,
       width = 10, height = 13, dpi = 300)

# -----------------------------------------------------------------------------
# Step 31. Spatial weights — Queen contiguity + sensitivity alternatives
# -----------------------------------------------------------------------------
spatial_df <- tracts[
  !is.na(tracts$pct_poverty_fb) &
    !is.na(tracts$pct_lang_isolated) &
    !is.na(tracts$pct_foreign_born),
]
spatial_df <- st_make_valid(spatial_df)

# Primary weights — Queen contiguity, row-standardized
nb <- poly2nb(spatial_df, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Sensitivity weights
nb_rook <- poly2nb(spatial_df, queen = FALSE)
lw_rook <- nb2listw(nb_rook, style = "W", zero.policy = TRUE)
coords  <- st_coordinates(st_centroid(spatial_df))
nb_knn5 <- knn2nb(knearneigh(coords, k = 5))
lw_knn5 <- nb2listw(nb_knn5, style = "W")

cat("Tracts in spatial sample:", length(nb), "\n")
cat("Average # of Queen neighbors:",
    round(mean(card(nb)), 1), "\n")

# -----------------------------------------------------------------------------
# Step 32. Global Moran's I — workforce + housing + immigrant indicators
# -----------------------------------------------------------------------------
# Six indicators chosen for substantive workforce-mobility relevance:
#   - FB poverty rate, unemployment rate: workforce disadvantage
#   - Employment-to-pop ratio: workforce engagement (cleaner than LFP)
#   - FB share, linguistic isolation: settlement/access
#   - Rent burden: housing pressure constraining mobility

moran_indicators <- list(
  "FB poverty rate"      = "pct_poverty_fb",
  "Unemployment rate"    = "unemp_rate",
  "Employment-to-pop"    = "pct_emp_to_pop",
  "FB share"             = "pct_foreign_born",
  "Linguistic isolation" = "pct_lang_isolated",
  "Rent-burdened share"  = "pct_rent_burdened"
)

moran_results <- data.frame(
  Indicator = character(),
  Moran_I   = numeric(),
  P_value   = numeric(),
  stringsAsFactors = FALSE
)

for (label in names(moran_indicators)) {
  var <- moran_indicators[[label]]
  vals <- spatial_df[[var]]
  if (sum(!is.na(vals)) < 50) next
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

# Sensitivity check: re-run poverty under three weight specs
moran_pov <- moran.test(spatial_df$pct_poverty_fb, lw,      zero.policy = TRUE)
moran_pov_rook <- moran.test(spatial_df$pct_poverty_fb, lw_rook, zero.policy = TRUE)
moran_pov_knn  <- moran.test(spatial_df$pct_poverty_fb, lw_knn5)

cat("\nSensitivity for FB poverty rate (different weight matrices):\n")
cat("  Queen contiguity: I =", round(moran_pov$estimate[1], 3), "\n")
cat("  Rook contiguity:  I =", round(moran_pov_rook$estimate[1], 3), "\n")
cat("  KNN-5 neighbors:  I =", round(moran_pov_knn$estimate[1], 3), "\n")

# -----------------------------------------------------------------------------
# Step 33. Local Moran's I (LISA) — five workforce-mobility indicators
# -----------------------------------------------------------------------------
lisa_indicators <- c(
  "pct_foreign_born"   = "FB share",
  "median_earn_fbE"    = "Median FB earnings",
  "pct_poverty_fb"     = "FB poverty rate",
  "pct_lang_isolated"  = "Linguistic isolation",
  "unemp_rate"         = "Unemployment rate",
  "median_hh_incomeE"  = "Median HH income"
)

# Cache scaled values for the three Moran scatter targets (Step 35)
scatter_cache <- list()

for (var in names(lisa_indicators)) {
  vals <- spatial_df[[var]]
  if (sum(!is.na(vals)) < 50) next
  
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
  
  if (var %in% c("median_hh_incomeE", "unemp_rate", "pct_lang_isolated")) {
    scatter_cache[[var]] <- list(
      x_scaled   = v_scaled,
      lag_scaled = v_lag_scl,
      cluster    = cluster_v
    )
  }
}

# Cluster counts for reporting
cat("\nLISA cluster distributions:\n")
for (var in names(lisa_indicators)) {
  col <- paste0("cluster_", var)
  if (col %in% names(spatial_df)) {
    cat("\n", lisa_indicators[[var]], ":\n", sep = "")
    print(table(spatial_df[[col]]))
  }
}

# -----------------------------------------------------------------------------
# Step 34. Philadelphia city boundary (used by all maps below)
# -----------------------------------------------------------------------------
philly_limit <- tigris::counties(state = "PA", cb = TRUE, year = acs_year) %>%
  filter(NAME == "Philadelphia") %>%
  st_transform(4326)

# -----------------------------------------------------------------------------
# Step 34a. Map — Foreign-born population share (saves PNG)
# -----------------------------------------------------------------------------
map_fb_share <- ggplot(spatial_df) +
  geom_sf(aes(fill = pct_foreign_born), color = "white", linewidth = 0.1) +
  geom_sf(data = philly_limit, fill = NA, color = "black", linewidth = 1) +
  scale_fill_gradientn(
    colors   = ramp_seq, na.value = gray_light,
    name     = "% foreign-born",
    labels   = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Where Philadelphia's immigrants live",
    subtitle = "Foreign-born share of total population by census tract, 2020-2024",
    caption  = paste0("Source: ACS 5-year estimates. ",
                      "Gray tracts have insufficient data to estimate reliably.\n",
                      "Heaviest concentrations: Lower Northeast, South Philadelphia, ",
                      "and pockets of West Philadelphia.")
  ) +
  theme_map

print(map_fb_share)
ggsave("output/map_fb_share.png", map_fb_share,
       width = 10, height = 10, dpi = 300)

# -----------------------------------------------------------------------------
# Step 34b. Map — Foreign-born poverty rate (saves PNG)
# -----------------------------------------------------------------------------
map_fb_poverty <- ggplot(spatial_df) +
  geom_sf(aes(fill = pct_poverty_fb), color = "white", linewidth = 0.1) +
  geom_sf(data = philly_limit, fill = NA, color = "black", linewidth = 1) +
  scale_fill_gradientn(
    colors   = ramp_seq, na.value = gray_light,
    name     = "% in poverty",
    labels   = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Where immigrant economic disadvantage concentrates",
    subtitle = "Foreign-born poverty rate by census tract, 2020-2024",
    caption  = paste0("Source: ACS 5-year estimates, table B06012. ",
                      "Universe: foreign-born population, all ages.\n",
                      "Gray tracts have too few foreign-born residents to estimate poverty rate.")
  ) +
  theme_map

print(map_fb_poverty)
ggsave("output/map_fb_poverty.png", map_fb_poverty,
       width = 10, height = 10, dpi = 300)

# -----------------------------------------------------------------------------
# Step 34c. Map — Median foreign-born earnings (FB-specific economic outcome)
# -----------------------------------------------------------------------------
map_fb_earnings <- ggplot(spatial_df) +
  geom_sf(aes(fill = median_earn_fbE), color = "white", linewidth = 0.1) +
  geom_sf(data = philly_limit, fill = NA, color = "black", linewidth = 1) +
  scale_fill_gradientn(
    colors   = ramp_seq, na.value = gray_light,
    name     = "Median earnings",
    labels   = scales::dollar
  ) +
  labs(
    title    = "Where immigrant earnings concentrate",
    subtitle = "Median earnings of foreign-born workers by census tract, 2020-2024",
    caption  = paste0("Source: ACS 5-year estimates, table B20017. ",
                      "Universe: foreign-born workers with earnings.\n",
                      "Gray tracts have too few foreign-born workers to estimate. ",
                      "Direct measure of immigrant economic outcomes — not tract-wide averages.")
  ) +
  theme_map

print(map_fb_earnings)
ggsave("output/map_fb_earnings.png", map_fb_earnings,
       width = 10, height = 10, dpi = 300)
# -----------------------------------------------------------------------------
# Step 34d. Map — Linguistic isolation (saves PNG)
# -----------------------------------------------------------------------------
map_lang_iso <- ggplot(spatial_df) +
  geom_sf(aes(fill = pct_lang_isolated), color = "white", linewidth = 0.1) +
  geom_sf(data = philly_limit, fill = NA, color = "black", linewidth = 1) +
  scale_fill_gradientn(
    colors   = ramp_seq, na.value = gray_light,
    name     = "% lang. isolated",
    labels   = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Where service access is hardest",
    subtitle = "Linguistically isolated households by census tract, 2020-2024",
    caption  = paste0("Source: ACS 5-year estimates, table C16002. ",
                      "Linguistically isolated = no household member 14+ speaks English well.\n",
                      "Strong proxy for service-access barriers in workforce, health, and education.")
  ) +
  theme_map

print(map_lang_iso)
ggsave("output/map_lang_iso.png", map_lang_iso,
       width = 10, height = 10, dpi = 300)

# -----------------------------------------------------------------------------
# Step 34e. Map — Rent burden weighted by FB share (housing pressure on
#                 immigrant neighborhoods)
# -----------------------------------------------------------------------------
# Construct FB-weighted rent burden: tracts score high only if BOTH rent
# burden is high AND foreign-born share is meaningful. This isolates housing
# pressure facing immigrant-receiving neighborhoods.
spatial_df$fb_rent_pressure <- (spatial_df$pct_rent_burdened / 100) *
  (spatial_df$pct_foreign_born / 100) * 100

map_fb_rent_pressure <- ggplot(spatial_df) +
  geom_sf(aes(fill = fb_rent_pressure), color = "white", linewidth = 0.1) +
  geom_sf(data = philly_limit, fill = NA, color = "black", linewidth = 1) +
  scale_fill_gradientn(
    colors   = ramp_seq, na.value = gray_light,
    name     = "FB × rent burden index",
    labels   = function(x) sprintf("%.1f", x)
  ) +
  labs(
    title    = "Where housing pressure falls on immigrant neighborhoods",
    subtitle = "Rent-burdened share × FB share index, by census tract, 2020-2024",
    caption  = paste0("Source: ACS 5-year estimates (B25070 × B05002). ",
                      "Index = (% rent-burdened) × (% foreign-born) / 100.\n",
                      "Identifies tracts where BOTH immigrant presence AND ",
                      "housing pressure are high — the workforce-mobility constraint zone.")
  ) +
  theme_map

print(map_fb_rent_pressure)
ggsave("output/map_fb_rent_pressure.png", map_fb_rent_pressure,
       width = 10, height = 10, dpi = 300)
# -----------------------------------------------------------------------------
# Step 34e. Map — Rent-burdened share (NEW housing-pressure indicator)
# -----------------------------------------------------------------------------
map_rent_burden <- ggplot(spatial_df) +
  geom_sf(aes(fill = pct_rent_burdened), color = "white", linewidth = 0.1) +
  geom_sf(data = philly_limit, fill = NA, color = "black", linewidth = 1) +
  scale_fill_gradientn(
    colors   = ramp_seq, na.value = gray_light,
    name     = "% rent-burdened",
    labels   = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Where housing pressure constrains mobility",
    subtitle = "Share of renters paying 30%+ of income on rent, by census tract, 2020-2024",
    caption  = paste0("Source: ACS 5-year estimates, table B25070. ",
                      "Rent burden defined as 30%+ of household income on gross rent.\n",
                      "Tracts where workers can't relocate for better employment without ",
                      "absorbing further housing pressure.")
  ) +
  theme_map

print(map_rent_burden)
ggsave("output/map_rent_burden.png", map_rent_burden,
       width = 10, height = 10, dpi = 300)

# -----------------------------------------------------------------------------
# Step 35. Moran scatter plots — three workforce/access indicators
# -----------------------------------------------------------------------------
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
      "High-High (hot spot)" = accent_burgundy,                  # fig pink
      "Low-Low (cold spot)"  = accent_teal,                      # blueberry
      "High-Low (outlier)"   = "#fa8072",  
      "Low-High (outlier)"   = as.character(artsy["sage"]),      # mint leaf
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
      caption  = paste0("Slope of dashed line equals Global Moran's I. ",
                        "Each point is one tract.\n",
                        "Quadrant pattern reveals spatial structure of the indicator.")
    ) +
    coord_equal()
}

# Median HH income
p_moran_income <- make_moran_scatter(
  "median_hh_incomeE", "median HH income",
  "Where household income clusters",
  "High-income cluster\n(neighborhood economic anchors)",
  "Low-income cluster\n(structural disadvantage zones)"
)
print(p_moran_income)
ggsave("output/chart_moran_income.png", p_moran_income,
       width = 10, height = 10, dpi = 300)

# Unemployment rate
p_moran_unemp <- make_moran_scatter(
  "unemp_rate", "unemployment rate",
  "Where unemployment clusters",
  "High-unemployment cluster\n(labor-market exclusion zones)",
  "Low-unemployment cluster\n(strong-labor-market zones)"
)
print(p_moran_unemp)
ggsave("output/chart_moran_unemp.png", p_moran_unemp,
       width = 10, height = 10, dpi = 300)

# Linguistic isolation
p_moran_lang <- make_moran_scatter(
  "pct_lang_isolated", "linguistic isolation",
  "Where linguistic isolation clusters",
  "High-isolation cluster\n(service-access barrier zones)",
  "Low-isolation cluster\n(English-accessible zones)"
)
print(p_moran_lang)
ggsave("output/chart_moran_lang.png", p_moran_lang,
       width = 10, height = 10, dpi = 300)
# -----------------------------------------------------------------------------
# Step 36. LISA maps — six workforce-relevant indicators (each saves PNG)
# -----------------------------------------------------------------------------
lisa_colors <- c(
  "High-High (hot spot)" = accent_burgundy,                  # fig pink
  "Low-Low (cold spot)"  = accent_teal,                      # blueberry
  "High-Low (outlier)"   = "#fa8072",                        # salmon
  "Low-High (outlier)"   = as.character(artsy["sage"]),      # mint leaf
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

# Step 36a. LISA — FB share
map_lisa_fbshare <- make_lisa_map(
  "cluster_pct_foreign_born",
  "Where immigrant settlement clusters",
  "Local Moran's I cluster classification, p < 0.05",
  paste0("Source: ACS 5-year estimates (2020-2024). ",
         "Queen contiguity, row-standardized weights.\n",
         "Hot spots = established immigrant enclaves. ",
         "Cold spots = tracts with consistently low immigrant presence.")
)
print(map_lisa_fbshare)
ggsave("output/map_lisa_fbshare.png", map_lisa_fbshare,
       width = 10, height = 12, dpi = 300)

# Step 36b. LISA — Median FB earnings
map_lisa_earnings <- make_lisa_map(
  "cluster_median_earn_fbE",
  "Where immigrant earnings cluster",
  "Local Moran's I cluster classification, p < 0.05",
  paste0("Source: ACS 5-year estimates (2020-2024), table B20017. ",
         "Universe: foreign-born workers with earnings.\n",
         "Hot spots = immigrant economic anchor zones. ",
         "Cold spots = workforce-mobility priority zones.")
)
print(map_lisa_earnings)
ggsave("output/map_lisa_earnings.png", map_lisa_earnings,
       width = 10, height = 12, dpi = 300)

# Step 36c. LISA — FB poverty
map_lisa_poverty <- make_lisa_map(
  "cluster_pct_poverty_fb",
  "Where immigrant poverty clusters",
  "Local Moran's I cluster classification, p < 0.05",
  paste0("Source: ACS 5-year estimates (2020-2024), table B06012. ",
         "Universe: foreign-born population, all ages.\n",
         "Hot spots = high-poverty tracts surrounded by other high-poverty tracts. ",
         "Clustering is weak (I = 0.106), indicating immigrant poverty is largely individualized.")
)
print(map_lisa_poverty)
ggsave("output/map_lisa_poverty.png", map_lisa_poverty,
       width = 10, height = 12, dpi = 300)

# Step 36d. LISA — Linguistic isolation
map_lisa_lang <- make_lisa_map(
  "cluster_pct_lang_isolated",
  "Where linguistic isolation clusters",
  "Local Moran's I cluster classification, p < 0.05",
  paste0("Source: ACS 5-year estimates (2020-2024), table C16002. ",
         "Hot spots = clustered limited-English households.\n",
         "Highest-priority zones for ESL-integrated workforce programs ",
         "and bilingual service intake.")
)
print(map_lisa_lang)
ggsave("output/map_lisa_lang.png", map_lisa_lang,
       width = 10, height = 12, dpi = 300)

# Step 36e. LISA — Unemployment
map_lisa_unemp <- make_lisa_map(
  "cluster_unemp_rate",
  "Where unemployment clusters",
  "Local Moran's I cluster classification, p < 0.05",
  paste0("Source: ACS 5-year estimates (2020-2024). ",
         "Unemployment rate is tract-level (not nativity-specific).\n",
         "Cross-reference with immigrant settlement to find tracts where ",
         "immigrants live AND unemployment is high.")
)
print(map_lisa_unemp)
ggsave("output/map_lisa_unemp.png", map_lisa_unemp,
       width = 10, height = 12, dpi = 300)

# Step 36f. LISA — Median HH income
map_lisa_income <- make_lisa_map(
  "cluster_median_hh_incomeE",
  "Where household income clusters",
  "Local Moran's I cluster classification, p < 0.05",
  paste0("Source: ACS 5-year estimates (2020-2024), table B19013. ",
         "Tract-wide median household income.\n",
         "Hot spots = neighborhood economic anchors. ",
         "Cold spots = structural disadvantage zones.")
)
print(map_lisa_income)
ggsave("output/map_lisa_income.png", map_lisa_income,
       width = 10, height = 12, dpi = 300)


# -----------------------------------------------------------------------------
# Step 37. Spatial Lag Model — FB earnings as outcome
# -----------------------------------------------------------------------------
# Switched outcome from FB poverty (I = 0.106, too weak for SLM) to FB median earnings (I = 0.540). Strong spatial signal makes spillover modeling substantively meaningful. Income predictor scaled to thousands for readable coefficients; FB earnings scaled to thousands so all dollar quantities are on a comparable scale.

slm_df <- spatial_df[!is.na(spatial_df$median_earn_fbE), ]
nb_slm <- poly2nb(slm_df, queen = TRUE)
lw_slm <- nb2listw(nb_slm, style = "W", zero.policy = TRUE)

ols_fit <- lm(
  I(median_earn_fbE / 1000) ~ pct_lang_isolated + pct_foreign_born +
    unemp_rate + pct_emp_to_pop + pct_rent_burdened +
    I(median_hh_incomeE / 1000),
  data = slm_df
)

slm_fit <- lagsarlm(
  I(median_earn_fbE / 1000) ~ pct_lang_isolated + pct_foreign_born +
    unemp_rate + pct_emp_to_pop + pct_rent_burdened +
    I(median_hh_incomeE / 1000),
  data        = slm_df,
  listw       = lw_slm,
  zero.policy = TRUE
)

cat("\n=== Spatial Lag Model — FB median earnings ===\n")
cat("Outcome: median foreign-born earnings (thousands of dollars)\n")
cat("N tracts:", nrow(slm_df), "\n\n")
cat("ρ (spatial autoregressive coefficient):",
    round(slm_fit$rho, 3), "\n")
cat("ρ significance (LR test p-value):",
    format.pval(summary(slm_fit)$LR1$p.value, digits = 3), "\n")
cat("AIC: OLS =", round(AIC(ols_fit), 1),
    "  SLM =", round(AIC(slm_fit), 1), "\n")
cat("AIC improvement:",
    round(AIC(ols_fit) - AIC(slm_fit), 1),
    "(positive = SLM preferred)\n")

slm_table <- data.frame(
  Term = c("ρ (spatial lag)",
           "% linguistically isolated",
           "% foreign-born",
           "Unemployment rate",
           "Employment-to-pop ratio",
           "Rent-burdened share",
           "Median HH income (thousands)",
           "Intercept"),
  Estimate = round(c(
    slm_fit$rho,
    coef(slm_fit)["pct_lang_isolated"],
    coef(slm_fit)["pct_foreign_born"],
    coef(slm_fit)["unemp_rate"],
    coef(slm_fit)["pct_emp_to_pop"],
    coef(slm_fit)["pct_rent_burdened"],
    coef(slm_fit)["I(median_hh_incomeE/1000)"],
    coef(slm_fit)["(Intercept)"]
  ), 3),
  stringsAsFactors = FALSE
)

slm_summary <- summary(slm_fit)
slm_table$Std_Error <- c(
  slm_summary$rho.se,
  round(slm_summary$Coef[c("pct_lang_isolated", "pct_foreign_born",
                           "unemp_rate", "pct_emp_to_pop",
                           "pct_rent_burdened",
                           "I(median_hh_incomeE/1000)",
                           "(Intercept)"), "Std. Error"], 3)
)
slm_table$P_value <- c(
  format.pval(2 * pnorm(abs(slm_fit$rho / slm_summary$rho.se),
                        lower.tail = FALSE), digits = 3),
  format.pval(slm_summary$Coef[c("pct_lang_isolated", "pct_foreign_born",
                                 "unemp_rate", "pct_emp_to_pop",
                                 "pct_rent_burdened",
                                 "I(median_hh_incomeE/1000)",
                                 "(Intercept)"), "Pr(>|z|)"],
              digits = 3)
)

print(slm_table)
write.csv(slm_table, "output/table_slm.csv", row.names = FALSE)
# -----------------------------------------------------------------------------
# Step 18. Mincer estimation sample
# -----------------------------------------------------------------------------
mincer_df <- foreign_born[
  foreign_born$ESR %in% c("1", "2") &
    foreign_born$wage_num > 1000 &
    !is.na(foreign_born$eng_factor) &
    !is.na(foreign_born$edu_collapsed) &
    !is.na(foreign_born$yrs_us) &
    !is.na(foreign_born$is_naturalized) &
    !is.na(foreign_born$hh_supergroup),
]
mincer_df$ln_wage <- log(mincer_df$wage_num)

# Set non-citizen as reference, coupled household as reference
mincer_df$is_naturalized <- factor(mincer_df$is_naturalized,
                                   levels = c("Non-citizen", "Naturalized"))

cat("Mincer sample size:", nrow(mincer_df), "\n")
# -----------------------------------------------------------------------------
# Step 19. Fit Mincer regression
# -----------------------------------------------------------------------------
mincer_fit <- lm(
  ln_wage ~ eng_factor + edu_collapsed + age_num + age_sq +
    yrs_us + waob_lab + SEX + is_naturalized + hh_supergroup,
  data    = mincer_df,
  weights = PWGTP
)
summary(mincer_fit)

# -----------------------------------------------------------------------------
# Step 20. Tidy coefficients to % effects
# -----------------------------------------------------------------------------
mincer_tidy <- tidy(mincer_fit, conf.int = TRUE)
mincer_tidy$pct_effect <- (exp(mincer_tidy$estimate) - 1) * 100
mincer_tidy$pct_low    <- (exp(mincer_tidy$conf.low)  - 1) * 100
mincer_tidy$pct_high   <- (exp(mincer_tidy$conf.high) - 1) * 100

print(mincer_tidy[, c("term", "pct_effect", "pct_low", "pct_high", "p.value")])


# -----------------------------------------------------------------------------
# Step 21. Chart 3 — Mincer coefficient plot, focal vars (saves PNG)
# -----------------------------------------------------------------------------
coef_plot_df <- mincer_tidy %>%
  filter(term %in% c("eng_factorNot well", "eng_factorWell",
                     "eng_factorVery well",
                     "edu_collapsedHS/GED",
                     "edu_collapsedSome college/Assoc.",
                     "edu_collapsedBachelor's+",
                     "yrs_us",
                     "is_naturalizedNaturalized")) %>%
  mutate(
    label = recode(term,
                   "eng_factorNot well"                = "English: Not well",
                   "eng_factorWell"                    = "English: Well",
                   "eng_factorVery well"               = "English: Very well",
                   "edu_collapsedHS/GED"               = "Education: HS/GED",
                   "edu_collapsedSome college/Assoc."  = "Education: Some college",
                   "edu_collapsedBachelor's+"          = "Education: Bachelor's+",
                   "yrs_us"                            = "Each year in U.S.",
                   "is_naturalizedNaturalized"         = "Citizenship: Naturalized"
    ),
    group = case_when(
      grepl("English",     label) ~ "English",
      grepl("Education",   label) ~ "Education",
      grepl("Citizenship", label) ~ "Citizenship",
      TRUE                        ~ "Other"
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
    "English"     = accent_burgundy,
    "Education"   = as.character(artsy["mustard"]),
    "Citizenship" = as.character(artsy["teal"]),
    "Other"       = as.character(artsy["sage"])
  )) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    title    = "Four channels of wage variation among foreign-born workers",
    subtitle = "Mincer regression: % effect on annual wages, 95% CI",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(mincer_df)),
                      ". Adj R² = ",
                      round(summary(mincer_fit)$adj.r.squared, 3), ".\n",
                      "Reference: English 'Not at all', Education '<HS', ",
                      "Citizenship 'Non-citizen'.")
  )

print(p3_coef)
ggsave("output/chart3_mincer_coefs.png", p3_coef,
       width = 10, height = 14, dpi = 300)
# -----------------------------------------------------------------------------
# Step 21b. Full Mincer coefficient plot — all variables, grouped 
# -----------------------------------------------------------------------------
coef_full_df <- mincer_tidy %>%
  filter(term != "(Intercept)", term != "age_sq") %>%
  mutate(
    label = recode(term,
                   "eng_factorNot well"                = "English: Not well",
                   "eng_factorWell"                    = "English: Well",
                   "eng_factorVery well"               = "English: Very well",
                   "edu_collapsedHS/GED"               = "Education: HS/GED",
                   "edu_collapsedSome college/Assoc."  = "Education: Some college",
                   "edu_collapsedBachelor's+"          = "Education: Bachelor's+",
                   "age_num"                           = "Each year of age",
                   "yrs_us"                            = "Each year in U.S.",
                   "waob_labAsia"                      = "Origin: Asia",
                   "waob_labEurope"                    = "Origin: Europe",
                   "waob_labLatin America"             = "Origin: Latin America",
                   "waob_labNorthern America"          = "Origin: Northern America",
                   "waob_labOceania"                   = "Origin: Oceania",
                   "SEX2"                              = "Sex: Female",
                   "is_naturalizedNaturalized"         = "Citizenship: Naturalized",
                   "hh_supergroupSingle householder with family" =
                     "HH: Single head w/ family",
                   "hh_supergroupNonfamily / solo households" =
                     "HH: Nonfamily / solo"
    ),
    group = case_when(
      grepl("English",     label) ~ "English",
      grepl("Education",   label) ~ "Education",
      grepl("Origin",      label) ~ "Origin",
      grepl("Sex",         label) ~ "Sex",
      grepl("Citizenship", label) ~ "Citizenship",
      grepl("HH:",         label) ~ "Household",
      TRUE                        ~ "Age / time"
    ),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  mutate(
    group = factor(group, levels = c("English", "Education",
                                     "Citizenship", "Household",
                                     "Age / time", "Origin", "Sex"))
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
    "English"     = accent_burgundy,
    "Education"   = as.character(artsy["mustard"]),
    "Citizenship" = as.character(artsy["teal"]),
    "Household"   = "#7d7676",                       # sesame
    "Age / time"  = as.character(artsy["sage"]),
    "Origin"      = "#3a3f5e",                       # blueberry darker
    "Sex"         = as.character(artsy["rose"])
  )) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.08, 0.08))) +
  labs(
    title    = "Full Mincer regression with workforce controls",
    subtitle = "% effect on annual wages, 95% CI, grouped by category",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(mincer_df)),
                      ". Adj R² = ",
                      round(summary(mincer_fit)$adj.r.squared, 3),
                      ". Signif: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1.\n",
                      "Reference: English 'Not at all', Education '<HS', ",
                      "Citizenship 'Non-citizen', Household 'Coupled', ",
                      "Origin 'Africa', Sex 'Male'.")
  )

print(p3b_coef_full)
ggsave("output/chart3b_mincer_full.png", p3b_coef_full,
       width = 10, height = 14, dpi = 300)
# -----------------------------------------------------------------------------
# Step 22. English × Education interaction test
# -----------------------------------------------------------------------------
# Interaction 1: English × Education (original — the bimodal-skill hypothesis)
mincer_int_engedu <- lm(
  ln_wage ~ eng_factor * edu_collapsed + age_num + age_sq + yrs_us +
    waob_lab + SEX + is_naturalized + hh_supergroup,
  data = mincer_df, weights = PWGTP)
cat("\n=== English × Education interaction ===\n")
print(anova(mincer_fit, mincer_int_engedu))

# Interaction 2: Naturalization × English
# Hypothesis: citizenship amplifies the language premium because naturalized
# fluent speakers can access federal/licensed positions non-citizens cannot.
mincer_int_natenglish <- lm(
  ln_wage ~ is_naturalized * eng_factor + edu_collapsed + age_num + age_sq +
    yrs_us + waob_lab + SEX + hh_supergroup,
  data = mincer_df, weights = PWGTP)
cat("\n=== Naturalization × English interaction ===\n")
print(anova(mincer_fit, mincer_int_natenglish))

# Interaction 3: Sex × Household structure
# Hypothesis: the female wage penalty is larger in households where women
# bear caregiving load (single-head-with-family) than in coupled households.
mincer_int_sexhh <- lm(
  ln_wage ~ SEX * hh_supergroup + eng_factor + edu_collapsed +
    age_num + age_sq + yrs_us + waob_lab + is_naturalized,
  data = mincer_df, weights = PWGTP)
cat("\n=== Sex × Household structure interaction ===\n")
print(anova(mincer_fit, mincer_int_sexhh))

# -----------------------------------------------------------------------------
# Step 23. Random Forest
# -----------------------------------------------------------------------------
rf_df <- mincer_df[, c("ln_wage", "eng_factor", "edu_collapsed",
                       "age_num", "yrs_us", "waob_lab", "SEX", "PWGTP")]
rf_df <- rf_df[complete.cases(rf_df), ]

rf_fit <- ranger(
  formula      = ln_wage ~ eng_factor + edu_collapsed + age_num +
    yrs_us + waob_lab + SEX,
  data         = rf_df,
  num.trees    = 500,
  importance   = "permutation",
  case.weights = rf_df$PWGTP,
  seed         = 2025
)
cat("Random Forest OOB R²:",  round(rf_fit$r.squared, 3), "\n")
cat("Random Forest OOB MSE:", round(rf_fit$prediction.error, 3), "\n")

# -----------------------------------------------------------------------------
# Step 23b. Classification RF — predicting labor-force status
# -----------------------------------------------------------------------------
# Non-parametric complement to the multinomial logit (Step 28).
# Uses the full working-age foreign-born sample (Employed + Unemp + NILF).
# Compare the variable importance with Step 24's wage VIP: variables that
# matter for PARTICIPATION may differ from those that matter for EARNINGS.

lfs_df <- foreign_born[
  foreign_born$age_num >= 16 &
    foreign_born$age_num <= 65 &
    !is.na(foreign_born$eng_factor) &
    !is.na(foreign_born$edu_collapsed) &
    !is.na(foreign_born$yrs_us) &
    !is.na(foreign_born$is_naturalized) &
    !is.na(foreign_born$hh_supergroup) &
    !is.na(foreign_born$esr3),
  c("esr3", "eng_factor", "edu_collapsed", "age_num", "yrs_us",
    "waob_lab", "SEX", "is_naturalized", "hh_supergroup", "PWGTP")
]
lfs_df <- lfs_df[complete.cases(lfs_df), ]

# Ensure factor structure for ranger
lfs_df$waob_lab        <- factor(lfs_df$waob_lab)
lfs_df$is_naturalized  <- factor(lfs_df$is_naturalized,
                                 levels = c("Non-citizen", "Naturalized"))

cat("Classification RF sample size:", nrow(lfs_df), "\n")
cat("Outcome distribution:\n")
print(round(prop.table(table(lfs_df$esr3)), 3))

rf_lfs <- ranger(
  formula      = esr3 ~ eng_factor + edu_collapsed + age_num + yrs_us +
    waob_lab + SEX + is_naturalized + hh_supergroup,
  data         = lfs_df,
  num.trees    = 500,
  importance   = "permutation",
  case.weights = lfs_df$PWGTP,
  probability  = FALSE,
  seed         = 2025
)

cat("\nOOB classification error:",
    round(rf_lfs$prediction.error, 3), "\n")

# Confusion matrix
oob_preds <- rf_lfs$predictions
cm <- table(predicted = oob_preds, actual = lfs_df$esr3)
cat("\nOOB confusion matrix:\n")
print(cm)
cat("\nOOB accuracy:", round(sum(diag(cm)) / sum(cm), 3), "\n")


# -----------------------------------------------------------------------------
# Step 23c. Variable importance — labor-force status classifier (saves PNG)
# -----------------------------------------------------------------------------
vip_lfs <- vip::vi(rf_lfs)
vip_lfs$Variable <- recode(vip_lfs$Variable,
                           eng_factor      = "English proficiency",
                           edu_collapsed   = "Education",
                           age_num         = "Age",
                           yrs_us          = "Years in U.S.",
                           waob_lab        = "Region of origin",
                           SEX             = "Sex",
                           is_naturalized  = "Citizenship",
                           hh_supergroup   = "Household structure"
)
print(vip_lfs)

vip_lfs <- vip_lfs %>%
  arrange(Importance) %>%
  mutate(bar_color = colorRampPalette(ramp_seq)(nrow(.)))

p_vip_lfs <- ggplot(vip_lfs,
                    aes(x = Importance, y = reorder(Variable, Importance),
                        fill = bar_color)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf("%.3f", Importance)),
            hjust = -0.2, size = 3.4, color = ink,
            family = "sans", fontface = "bold") +
  scale_fill_identity() +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title    = "What predicts labor-force status",
    subtitle = "Permutation variable importance, Random Forest classifying Employed / Unemp / NILF",
    x = NULL, y = NULL,
    caption  = paste0("Source: ranger Random Forest, 500 trees. ",
                      "N = ", scales::comma(nrow(lfs_df)),
                      " foreign-born ages 16-65.\n",
                      "OOB classification error: ",
                      round(rf_lfs$prediction.error, 3),
                      ". Compare with the wage VIP (chart4) to see which channels ",
                      "matter for participation vs earnings.")
  )

print(p_vip_lfs)
ggsave("output/chart4b_vip_lfs.png", p_vip_lfs,
       width = 10, height = 5, dpi = 300)
# -----------------------------------------------------------------------------
# Step 24. Variable importance
# -----------------------------------------------------------------------------
vip_data <- vip::vi(rf_fit)
vip_data$Variable <- recode(vip_data$Variable,
                            eng_factor    = "English proficiency",
                            edu_collapsed = "Education",
                            age_num       = "Age",
                            yrs_us        = "Years in U.S.",
                            waob_lab      = "Region of origin",
                            SEX           = "Sex"
)
print(vip_data)


# -----------------------------------------------------------------------------
# Step 25. Chart 4 — variable importance (saves PNG)
# -----------------------------------------------------------------------------
vip_data <- vip_data %>%
  arrange(Importance) %>%
  mutate(bar_color = colorRampPalette(ramp_seq)(nrow(.)))

p4_vip <- ggplot(vip_data,
                 aes(x = Importance, y = reorder(Variable, Importance),
                     fill = bar_color)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf("%.3f", Importance)),
            hjust = -0.2, size = 3.4, color = ink,
            family = "sans", fontface = "bold") +
  scale_fill_identity() +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title    = "Education edges out English in the non-linear model",
    subtitle = "Permutation variable importance, Random Forest for log wages",
    x = NULL, y = NULL,
    caption  = paste0("Source: ranger Random Forest, 500 trees. ",
                      "Out-of-bag R² = ", round(rf_fit$r.squared, 3), ".")
  )

print(p4_vip)
ggsave("output/chart4_vip.png", p4_vip,
       width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Step 26. Manual partial dependence for English
# -----------------------------------------------------------------------------
eng_levels  <- levels(rf_df$eng_factor)
pdp_results <- data.frame(
  eng_factor         = character(0),
  mean_pred_log_wage = numeric(0),
  stringsAsFactors   = FALSE
)

for (lvl in eng_levels) {
  test_df <- rf_df
  test_df$eng_factor <- factor(lvl, levels = eng_levels)
  preds <- predict(rf_fit, data = test_df)$predictions
  weighted_mean_pred <- weighted.mean(preds, w = test_df$PWGTP, na.rm = TRUE)
  pdp_results <- rbind(pdp_results, data.frame(
    eng_factor         = lvl,
    mean_pred_log_wage = weighted_mean_pred,
    stringsAsFactors   = FALSE
  ))
}

pdp_results$pred_wage_usd <- exp(pdp_results$mean_pred_log_wage)
pdp_results$eng_factor    <- factor(pdp_results$eng_factor, levels = eng_levels)
print(pdp_results)


# -----------------------------------------------------------------------------
# Step 27. Chart 5 — partial dependence (saves PNG)
# -----------------------------------------------------------------------------
p5_pdp <- ggplot(pdp_results, aes(x = eng_factor, y = pred_wage_usd,
                                  group = 1)) +
  geom_line(color = accent_burgundy, linewidth = 0.8) +
  geom_point(color = accent_burgundy, size = 4) +
  geom_text(aes(label = scales::dollar(round(pred_wage_usd, -2))),
            vjust = -1.4, size = 3.4, color = ink, fontface = "bold") +
  scale_y_continuous(labels = scales::dollar,
                     expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title    = "Predicted wages climb at every fluency tier",
    subtitle = "Random Forest partial dependence — non-parametric robustness check",
    x = NULL, y = NULL,
    caption  = "Source: Random Forest partial dependence (manual implementation)."
  )

print(p5_pdp)
ggsave("output/chart5_pdp.png", p5_pdp,
       width = 10, height = 5, dpi = 300)


# -----------------------------------------------------------------------------
# Step 28. Multinomial logit
# -----------------------------------------------------------------------------
logit_df <- foreign_born[
  foreign_born$age_num >= 16 &
    foreign_born$age_num <= 65 &
    !is.na(foreign_born$eng_factor) &
    !is.na(foreign_born$edu_collapsed) &
    !is.na(foreign_born$yrs_us),
]

multi_fit <- multinom(
  esr3 ~ eng_factor + edu_collapsed + age_num + yrs_us + waob_lab + SEX,
  data    = logit_df,
  weights = PWGTP,
  trace   = FALSE,
  MaxNWts = 5000
)

multi_tidy <- tidy(multi_fit, conf.int = TRUE, exponentiate = TRUE)
multi_tidy <- multi_tidy[multi_tidy$term != "(Intercept)", ]


# -----------------------------------------------------------------------------
# Step 29. Multinomial logit results table (saves HTML)
# -----------------------------------------------------------------------------
multi_table <- multi_tidy %>%
  mutate(
    Variable = recode(term,
                      "eng_factorNot well"                = "English: Not well",
                      "eng_factorWell"                    = "English: Well",
                      "eng_factorVery well"               = "English: Very well",
                      "edu_collapsedHS/GED"               = "Education: HS/GED",
                      "edu_collapsedSome college/Assoc."  = "Education: Some college",
                      "edu_collapsedBachelor's+"          = "Education: Bachelor's+",
                      "age_num"                           = "Each year of age",
                      "yrs_us"                            = "Each year in U.S.",
                      "waob_labAsia"                      = "Origin: Asia",
                      "waob_labEurope"                    = "Origin: Europe",
                      "waob_labLatin America"             = "Origin: Latin America",
                      "waob_labNorthern America"          = "Origin: Northern America",
                      "waob_labOceania"                   = "Origin: Oceania",
                      "SEX2"                              = "Sex: Female"
    ),
    Outcome = factor(y.level,
                     levels = c("Unemployed", "Not in LF"),
                     labels = c("Unemployed vs Employed",
                                "Not in LF vs Employed")),
    `Odds Ratio` = sprintf("%.2f", estimate),
    `95% CI` = sprintf("[%.2f, %.2f]", conf.low, conf.high),
    Sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  select(Variable, Outcome, `Odds Ratio`, `95% CI`, Sig) %>%
  arrange(Outcome, Variable)

multi_gt <- multi_table %>%
  gt(groupname_col = "Outcome") %>%
  tab_header(
    title    = "Multinomial logit: full results on labor-force status",
    subtitle = "Reference: English 'Not at all', Education '<HS', Origin 'Africa', Sex 'Male'"
  ) %>%
  tab_source_note("Source: ACS 5-year PUMS (2020-2024), foreign-born ages 16-65.") %>%
  tab_source_note("Significance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1")
 

print(multi_gt)
gtsave(multi_gt, "output/table_multinomial.html")


# -----------------------------------------------------------------------------
# Step 30. Multi-model coefficient heatmap (saves PNG)
# -----------------------------------------------------------------------------
mincer_long <- data.frame(
  term    = names(coef(mincer_fit)),
  value   = as.numeric(coef(mincer_fit)),
  Outcome = "Wages (log)",
  stringsAsFactors = FALSE
) %>%
  filter(term != "(Intercept)", term != "age_sq")

multi_long <- tidy(multi_fit, conf.int = FALSE, exponentiate = FALSE) %>%
  filter(term != "(Intercept)") %>%
  mutate(Outcome = factor(y.level,
                          levels = c("Unemployed", "Not in LF"),
                          labels = c("Unemployment (log-OR)",
                                     "Not in LF (log-OR)"))) %>%
  select(term, value = estimate, Outcome)

heat_df <- bind_rows(mincer_long, multi_long) %>%
  mutate(
    Variable = recode(term,
                      "eng_factorNot well"                = "English: Not well",
                      "eng_factorWell"                    = "English: Well",
                      "eng_factorVery well"               = "English: Very well",
                      "edu_collapsedHS/GED"               = "Education: HS/GED",
                      "edu_collapsedSome college/Assoc."  = "Education: Some college",
                      "edu_collapsedBachelor's+"          = "Education: Bachelor's+",
                      "age_num"                           = "Each year of age",
                      "yrs_us"                            = "Each year in U.S.",
                      "waob_labAsia"                      = "Origin: Asia",
                      "waob_labEurope"                    = "Origin: Europe",
                      "waob_labLatin America"             = "Origin: Latin America",
                      "waob_labNorthern America"          = "Origin: Northern America",
                      "waob_labOceania"                   = "Origin: Oceania",
                      "SEX2"                              = "Sex: Female"
    ),
    Variable = factor(Variable, levels = rev(c(
      "English: Not well",
      "English: Well",
      "English: Very well",
      "Education: HS/GED",
      "Education: Some college",
      "Education: Bachelor's+",
      "Each year of age",
      "Each year in U.S.",
      "Origin: Asia",
      "Origin: Europe",
      "Origin: Latin America",
      "Origin: Northern America",
      "Origin: Oceania",
      "Sex: Female"
    ))),
    plot_value = case_when(
      Outcome == "Wages (log)" ~ value,
      TRUE                      ~ -value     # flip NILF/Unemp so muscat = good
    )
  ) %>%
  group_by(Outcome) %>%
  mutate(
    plot_value_std = plot_value / median(abs(plot_value), na.rm = TRUE),
    plot_value_std = pmax(pmin(plot_value_std, 1), -1)
  ) %>%
  ungroup()

p6_heatmap <- ggplot(heat_df, aes(x = Outcome, y = Variable,
                                  fill = plot_value_std)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(
    aes(label = case_when(
      Outcome == "Wages (log)" ~ sprintf("%+.0f%%", (exp(value) - 1) * 100),
      TRUE                     ~ sprintf("OR %.2f", exp(value))
    )),
    color = ink, size = 3.4, fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#c98590", mid = "#f5f0e8", high = "#c5d68a",  # fig ↔ cream ↔ muscat
    midpoint = 0, limits = c(-1, 1),
    name = "Direction",
    labels = c("Worse for worker", "Neutral", "Better for worker"),
    breaks = c(-1, 0, 1)
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Multi-model summary: how each variable behaves across outcomes",
    subtitle = "Cells: Mincer % wage effect + multinomial logit odds ratios",
    x = NULL, y = NULL,
    caption  = paste0("Source: Mincer linear regression + multinomial logit. ",
                      "All effects relative to reference categories.\n",
                      "Color clipped at ±1 to prevent Oceania/N. America ",
                      "(tiny samples) from washing out the scale.")
  ) +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 9, color = gray_dark),
    axis.text.x.top = element_text(hjust = 0.5, face = "bold", color = ink),
    axis.text.y     = element_text(face = "bold", color = ink),
    panel.grid      = element_blank(),
    axis.line.x     = element_blank(),
    axis.ticks.x    = element_blank()
  )

print(p6_heatmap)
ggsave("output/chart6_model_heatmap.png", p6_heatmap,
       width = 10, height = 10, dpi = 300)

# =============================================================================
# END
# =============================================================================