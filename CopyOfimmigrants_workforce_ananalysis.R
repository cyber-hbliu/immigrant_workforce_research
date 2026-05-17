# =============================================================================
# IMMIGRANT EMPLOYMENT & ECONOMIC MOBILITY IN PHILADELPHIA
# Step-by-step analysis pipeline — runs end-to-end, saves visuals to output/
# Author: Hebe Liu
# =============================================================================

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
  "NP", "WAGP", "INDP", "NAICSP", "ENG", "HHL", "LNGI", "YOEP",
  "PUMA", "COW", "WAOB", "HHLDRRAC1P", "MAR", "MIG"
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
# Step 5. Filter PUMS to Philadelphia County
# -----------------------------------------------------------------------------
pums_philly <- pums_raw[pums_raw$PUMA %in% philly_pumas, ]
cat("PUMS rows (Philly):", nrow(pums_philly), "\n")
cat("Foreign-born:",       sum(pums_philly$NATIVITY == 2), "\n")


# -----------------------------------------------------------------------------
# Step 6. Pull tract-level ACS estimates
# -----------------------------------------------------------------------------
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
  median_hh_income   = "B19013_001"
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
# Step 17. Tract-indicator correlation heatmap (saves PNG)
# -----------------------------------------------------------------------------
cor_df <- tracts %>%
  st_drop_geometry() %>%
  mutate(
    pct_naturalized_of_fb = ifelse(total_foreign_bornE > 0,
                                   naturalizedE / total_foreign_bornE * 100, NA),
    pct_noncitizen_of_fb  = ifelse(total_foreign_bornE > 0,
                                   noncitizenE / total_foreign_bornE * 100, NA),
    pct_hispanic          = ifelse(total_popE > 0,
                                   hispanicE / total_popE * 100, NA)
  ) %>%
  select(
    `% foreign-born`            = pct_foreign_born,
    `% naturalized (of FB)`     = pct_naturalized_of_fb,
    `% non-citizen (of FB)`     = pct_noncitizen_of_fb,
    `% poverty (FB)`            = pct_poverty_fb,
    `Median FB earnings ($)`    = median_earn_fbE,
    `% lang. isolated`          = pct_lang_isolated,
    `Unemp. rate`               = unemp_rate,
    `% Hispanic`                = pct_hispanic,
    `% homeowner`               = pct_homeowner,
    `Median rent ($)`           = median_gross_rentE,
    `Median HH income ($)`      = median_hh_incomeE
  )

cor_matrix <- cor(cor_df, use = "pairwise.complete.obs")

p_cor <- ggcorrplot(
  cor_matrix,
  hc.order      = FALSE,
  type          = "lower",
  lab           = TRUE,
  lab_size      = 2.6,
  outline.color = "white",
  colors        = c("#c5d68a", "#f5f0e8", "#c98590"),  # muscat ↔ cream ↔ fig
  ggtheme       = theme_editorial
) +
  labs(
    title    = "How immigrant indicators cluster at the tract level",
    subtitle = "Pearson correlations across 11 ACS indicators, Philadelphia census tracts",
    caption  = paste0("Source: ACS 5-year estimates (2020-2024). ",
                      "N = ", nrow(cor_df), " tracts.\n",
                      "Fig pink = positive correlation, muscat green = negative. ",
                      "All variables on tract level.")
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

print(p_cor)
ggsave("output/chart_correlation_heatmap.png", p_cor,
       width = 10, height = 10, dpi = 300)
# -----------------------------------------------------------------------------
# Step 18. Mincer estimation sample
# -----------------------------------------------------------------------------
mincer_df <- foreign_born[
  foreign_born$ESR %in% c("1", "2") &
    foreign_born$wage_num > 1000 &
    !is.na(foreign_born$eng_factor) &
    !is.na(foreign_born$edu_collapsed) &
    !is.na(foreign_born$yrs_us),
]
mincer_df$ln_wage <- log(mincer_df$wage_num)
cat("Mincer sample size:", nrow(mincer_df), "\n")


# -----------------------------------------------------------------------------
# Step 19. Fit Mincer regression
# -----------------------------------------------------------------------------
mincer_fit <- lm(
  ln_wage ~ eng_factor + edu_collapsed + age_num + age_sq +
    yrs_us + waob_lab + SEX,
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
                     "yrs_us")) %>%
  mutate(
    label = recode(term,
                   "eng_factorNot well"                = "English: Not well",
                   "eng_factorWell"                    = "English: Well",
                   "eng_factorVery well"               = "English: Very well",
                   "edu_collapsedHS/GED"               = "Education: HS/GED",
                   "edu_collapsedSome college/Assoc."  = "Education: Some college",
                   "edu_collapsedBachelor's+"          = "Education: Bachelor's+",
                   "yrs_us"                            = "Each year in U.S."
    ),
    group = case_when(
      grepl("English",   label) ~ "English",
      grepl("Education", label) ~ "Education",
      TRUE                      ~ "Other"
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
    "English"   = accent_burgundy,
    "Education" = as.character(artsy["mustard"]),
    "Other"     = as.character(artsy["sage"])
  )) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    title    = "The language premium is comparable to the college premium",
    subtitle = "Mincer regression: % effect on annual wages, 95% CI",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(mincer_df)),
                      ". Adj R² = ",
                      round(summary(mincer_fit)$adj.r.squared, 3), ".\n",
                      "Reference categories: English 'Not at all', Education '<HS'.")
  )

print(p3_coef)
ggsave("output/chart3_mincer_coefs.png", p3_coef,
       width = 10, height = 9, dpi = 300)


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
                   "SEX2"                              = "Sex: Female"
    ),
    group = case_when(
      grepl("English",   label) ~ "English",
      grepl("Education", label) ~ "Education",
      grepl("Origin",    label) ~ "Origin",
      grepl("Sex",       label) ~ "Sex",
      TRUE                      ~ "Age / time"
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
            vjust = -0.9, size = 2.8, family = "sans", fontface = "bold",
            show.legend = FALSE) +
  scale_color_manual(values = c(
    "English"    = accent_burgundy,
    "Education"  = as.character(artsy["mustard"]),
    "Origin"     = as.character(artsy["teal"]),
    "Sex"        = as.character(artsy["rose"]),
    "Age / time" = as.character(artsy["sage"])
  )) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.08, 0.08))) +
  labs(
    title    = "Full Mincer regression: every coefficient",
    subtitle = "% effect on annual wages, 95% confidence interval, grouped by category",
    x = NULL, y = NULL,
    caption  = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                      scales::comma(nrow(mincer_df)),
                      ". Adj R² = ",
                      round(summary(mincer_fit)$adj.r.squared, 3),
                      ". Signif: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1.\n",
                      "Reference: English 'Not at all', Education '<HS', ",
                      "Origin 'Africa', Sex 'Male'. age_sq omitted.")
  )

print(p3b_coef_full)
ggsave("output/chart3b_mincer_full.png", p3b_coef_full,
       width = 10, height = 10, dpi = 300)


# -----------------------------------------------------------------------------
# Step 22. English × Education interaction test
# -----------------------------------------------------------------------------
mincer_interact <- lm(
  ln_wage ~ eng_factor * edu_collapsed + age_num + age_sq + yrs_us +
    waob_lab + SEX,
  data    = mincer_df,
  weights = PWGTP
)
anova(mincer_fit, mincer_interact)


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


# -----------------------------------------------------------------------------
# Step 31. Spatial weights (Queen contiguity)
# -----------------------------------------------------------------------------
spatial_df <- tracts[
  !is.na(tracts$pct_poverty_fb) &
    !is.na(tracts$pct_lang_isolated) &
    !is.na(tracts$pct_foreign_born),
]
spatial_df <- st_make_valid(spatial_df)

nb <- poly2nb(spatial_df, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
cat("Tracts in spatial sample:", length(nb), "\n")


# -----------------------------------------------------------------------------
# Step 32. Global Moran's I
# -----------------------------------------------------------------------------
moran_pov <- moran.test(spatial_df$pct_poverty_fb, lw, zero.policy = TRUE)
moran_li  <- moran.test(spatial_df$pct_lang_isolated, lw, zero.policy = TRUE)
moran_fb  <- moran.test(spatial_df$pct_foreign_born, lw, zero.policy = TRUE)
moran_un  <- moran.test(spatial_df$unemp_rate, lw, zero.policy = TRUE)

cat("Foreign-born poverty rate: I =",
    round(moran_pov$estimate[1], 3),
    "  p =", format.pval(moran_pov$p.value, digits = 3), "\n")
cat("Linguistic isolation:      I =",
    round(moran_li$estimate[1], 3),
    "  p =", format.pval(moran_li$p.value, digits = 3), "\n")
cat("Foreign-born share:        I =",
    round(moran_fb$estimate[1], 3),
    "  p =", format.pval(moran_fb$p.value, digits = 3), "\n")
cat("Unemployment rate:         I =",
    round(moran_un$estimate[1], 3),
    "  p =", format.pval(moran_un$p.value, digits = 3), "\n")


# -----------------------------------------------------------------------------
# Step 33. Local Moran's I (LISA) — four workforce-mobility indicators
# -----------------------------------------------------------------------------
# Compute LISA cluster classification for each of four indicators, plus
# preserve standardized values for the Moran scatterplot (Step 35).

lisa_indicators <- c(
  "pct_poverty_fb"     = "FB poverty rate",
  "unemp_rate"         = "Unemployment rate",
  "pct_foreign_born"   = "Foreign-born share",
  "pct_lang_isolated"  = "Linguistic isolation"
)

# Store scaled values for poverty (used by the Moran scatter)
x_scaled <- NULL; lag_scaled <- NULL

for (var in names(lisa_indicators)) {
  vals <- spatial_df[[var]]
  local_m <- localmoran(vals, lw, zero.policy = TRUE)
  
  v_scaled  <- as.numeric(scale(vals))
  v_lag     <- lag.listw(lw, vals, zero.policy = TRUE)
  v_lag_scl <- as.numeric(scale(v_lag))
  p_v       <- local_m[, "Pr(z != E(Ii))"]
  
  cluster_v <- rep("Not significant", length(vals))
  cluster_v[p_v < 0.05 & v_scaled > 0 & v_lag_scl > 0] <- "High-High (hot spot)"
  cluster_v[p_v < 0.05 & v_scaled < 0 & v_lag_scl < 0] <- "Low-Low (cold spot)"
  cluster_v[p_v < 0.05 & v_scaled > 0 & v_lag_scl < 0] <- "High-Low (outlier)"
  cluster_v[p_v < 0.05 & v_scaled < 0 & v_lag_scl > 0] <- "Low-High (outlier)"
  
  spatial_df[[paste0("cluster_", var)]] <- cluster_v
  
  if (var == "pct_poverty_fb") {
    x_scaled   <- v_scaled
    lag_scaled <- v_lag_scl
  }
}

# Keep original cluster column for backward compatibility
spatial_df$cluster <- spatial_df$cluster_pct_poverty_fb

print(table(spatial_df$cluster_pct_poverty_fb))
print(table(spatial_df$cluster_unemp_rate))
print(table(spatial_df$cluster_pct_foreign_born))
print(table(spatial_df$cluster_pct_lang_isolated))


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
# Step 34c. Map — Linguistic isolation (saves PNG)
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
# Step 35. Moran scatterplot — diagnostic behind the LISA classification
# -----------------------------------------------------------------------------
moran_scatter_df <- data.frame(
  GEOID      = spatial_df$GEOID,
  x_scaled   = x_scaled,
  lag_scaled = lag_scaled,
  cluster    = spatial_df$cluster_pct_poverty_fb
)

moran_I_value <- moran_pov$estimate[1]

p_moran <- ggplot(moran_scatter_df,
                  aes(x = x_scaled, y = lag_scaled, color = cluster)) +
  geom_hline(yintercept = 0, color = gray_mid, linewidth = 0.3) +
  geom_vline(xintercept = 0, color = gray_mid, linewidth = 0.3) +
  geom_point(size = 2, alpha = 0.75) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE,
              color = ink, linewidth = 0.5, linetype = "dashed") +
  scale_color_manual(values = c(
    "High-High (hot spot)" = accent_burgundy,
    "Low-Low (cold spot)"  = accent_teal,
    "High-Low (outlier)"   = as.character(artsy["mustard"]),
    "Low-High (outlier)"   = as.character(artsy["rose"]),
    "Not significant"      = gray_light
  )) +
  annotate("text", x = 2.8, y = 2.8,
           label = "Hot spot\n(high surrounded by high)",
           hjust = 1, vjust = 1, size = 3,
           color = accent_burgundy, fontface = "bold") +
  annotate("text", x = -2.8, y = -2.8,
           label = "Cold spot\n(low surrounded by low)",
           hjust = 0, vjust = 0, size = 3,
           color = accent_teal, fontface = "bold") +
  annotate("text", x = 2.8, y = -2.8,
           label = sprintf("Global Moran's I = %.3f\n(p < 0.001)", moran_I_value),
           hjust = 1, vjust = 0, size = 3.2,
           color = ink, fontface = "bold") +
  labs(
    title    = "Moran scatterplot — the diagnostic behind the cluster maps",
    subtitle = "Each tract's foreign-born poverty rate vs. its neighbors' average (z-scores)",
    x = "This tract's poverty rate (standardized)",
    y = "Neighbors' average poverty rate (standardized)",
    caption  = paste0("Slope of dashed line equals Global Moran's I. ",
                      "Each point is one tract.\n",
                      "Top-right (hot spots) and bottom-left (cold spots) drive the ",
                      "positive spatial autocorrelation.")
  ) +
  coord_equal()

print(p_moran)
ggsave("output/chart_moran_scatter.png", p_moran,
       width = 10, height = 10, dpi = 300)


# -----------------------------------------------------------------------------
# Step 36a. LISA map — Foreign-born poverty (saves PNG)
# -----------------------------------------------------------------------------
lisa_colors <- c(
  "High-High (hot spot)" = accent_burgundy,
  "Low-Low (cold spot)"  = accent_teal,
  "High-Low (outlier)"   = as.character(artsy["mustard"]),
  "Low-High (outlier)"   = as.character(artsy["rose"]),
  "Not significant"      = "#eeeeee"
)

map_lisa_poverty <- ggplot(spatial_df) +
  geom_sf(aes(fill = cluster_pct_poverty_fb),
          color = "white", linewidth = 0.15) +
  geom_sf(data = philly_limit, fill = NA, color = "black", linewidth = 1) +
  scale_fill_manual(values = lisa_colors, name = "LISA cluster") +
  labs(
    title    = "Where immigrant poverty clusters",
    subtitle = "Local Moran's I cluster classification, p < 0.05",
    caption  = paste0("Source: ACS 5-year estimates (2020-2024). ",
                      "Queen contiguity, row-standardized weights.\n",
                      "Hot spots = high-poverty tracts surrounded by other high-poverty tracts. ",
                      "These are workforce-policy priority zones.")
  ) +
  theme_map

print(map_lisa_poverty)
ggsave("output/map_lisa_poverty.png", map_lisa_poverty,
       width = 10, height = 10, dpi = 300)


# -----------------------------------------------------------------------------
# Step 36b. LISA map — Unemployment rate (saves PNG)
# -----------------------------------------------------------------------------
map_lisa_unemp <- ggplot(spatial_df) +
  geom_sf(aes(fill = cluster_unemp_rate),
          color = "white", linewidth = 0.15) +
  geom_sf(data = philly_limit, fill = NA, color = "black", linewidth = 1) +
  scale_fill_manual(values = lisa_colors, name = "LISA cluster") +
  labs(
    title    = "Where unemployment clusters",
    subtitle = "Local Moran's I cluster classification, p < 0.05",
    caption  = paste0("Source: ACS 5-year estimates (2020-2024). ",
                      "Unemployment rate is tract-level (not nativity-specific).\n",
                      "Compare with immigrant settlement map to find tracts where ",
                      "immigrants live AND unemployment is high.")
  ) +
  theme_map

print(map_lisa_unemp)
ggsave("output/map_lisa_unemp.png", map_lisa_unemp,
       width = 10, height = 10, dpi = 300)


# -----------------------------------------------------------------------------
# Step 36c. LISA map — Foreign-born share (saves PNG)
# -----------------------------------------------------------------------------
map_lisa_fbshare <- ggplot(spatial_df) +
  geom_sf(aes(fill = cluster_pct_foreign_born),
          color = "white", linewidth = 0.15) +
  geom_sf(data = philly_limit, fill = NA, color = "black", linewidth = 1) +
  scale_fill_manual(values = lisa_colors, name = "LISA cluster") +
  labs(
    title    = "Where immigrant settlement clusters",
    subtitle = "Local Moran's I cluster classification, p < 0.05",
    caption  = paste0("Source: ACS 5-year estimates (2020-2024). ",
                      "Hot spots identify established immigrant enclaves.\n",
                      "These clusters anchor ethnic economies and social networks ",
                      "that shape workforce trajectories.")
  ) +
  theme_map

print(map_lisa_fbshare)
ggsave("output/map_lisa_fbshare.png", map_lisa_fbshare,
       width = 10, height = 10, dpi = 300)


# -----------------------------------------------------------------------------
# Step 36d. LISA map — Linguistic isolation (saves PNG)
# -----------------------------------------------------------------------------
map_lisa_lang <- ggplot(spatial_df) +
  geom_sf(aes(fill = cluster_pct_lang_isolated),
          color = "white", linewidth = 0.15) +
  geom_sf(data = philly_limit, fill = NA, color = "black", linewidth = 1) +
  scale_fill_manual(values = lisa_colors, name = "LISA cluster") +
  labs(
    title    = "Where linguistic isolation clusters",
    subtitle = "Local Moran's I cluster classification, p < 0.05",
    caption  = paste0("Source: ACS 5-year estimates (2020-2024), table C16002. ",
                      "Hot spots = clustered limited-English households.\n",
                      "Highest-priority zones for ESL-integrated workforce programs ",
                      "and bilingual service intake.")
  ) +
  theme_map

print(map_lisa_lang)
ggsave("output/map_lisa_lang.png", map_lisa_lang,
       width = 10, height = 10, dpi = 300)


# -----------------------------------------------------------------------------
# Step 37. Spatial Lag Model
# -----------------------------------------------------------------------------
ols_fit <- lm(
  pct_poverty_fb ~ pct_lang_isolated + pct_foreign_born +
    unemp_rate + I(median_hh_incomeE / 1000),
  data = spatial_df
)

slm_fit <- lagsarlm(
  pct_poverty_fb ~ pct_lang_isolated + pct_foreign_born +
    unemp_rate + I(median_hh_incomeE / 1000),
  data        = spatial_df,
  listw       = lw,
  zero.policy = TRUE
)

cat("ρ (spatial autoregressive coefficient):",
    round(slm_fit$rho, 3), "\n")
cat("ρ significance (LR test p-value):",
    format.pval(summary(slm_fit)$LR1$p.value, digits = 3), "\n")
cat("AIC: OLS =", round(AIC(ols_fit), 1),
    "  SLM =", round(AIC(slm_fit), 1), "\n")

slm_table <- data.frame(
  Term = c("ρ (spatial lag)",
           "% linguistically isolated",
           "% foreign-born",
           "Unemployment rate",
           "Median HH income (thousands)",
           "Intercept"),
  Estimate = round(c(
    slm_fit$rho,
    coef(slm_fit)["pct_lang_isolated"],
    coef(slm_fit)["pct_foreign_born"],
    coef(slm_fit)["unemp_rate"],
    coef(slm_fit)["I(median_hh_incomeE/1000)"],
    coef(slm_fit)["(Intercept)"]
  ), 3),
  stringsAsFactors = FALSE
)
print(slm_table)
write.csv(slm_table, "output/table_slm.csv", row.names = FALSE)


cat("\n>>> Done. All visuals saved to output/ folder. <<<\n")
# =============================================================================
# END
# =============================================================================