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
library(patchwork)

library(leaflet)
library(htmlwidgets)
library(gt)
library(knitr)
library(htmltools)


# -----------------------------------------------------------------------------
# Step 2. Editorial palette and ggplot theme
# -----------------------------------------------------------------------------
artsy <- c(
  burgundy   = "#8b1f3d",
  terracotta = "#bc4838",
  mustard    = "#d4a64a",
  sage       = "#8fa886",
  teal       = "#2b7a8c",
  rose       = "#c97a8f"
)

accent_burgundy <- as.character(artsy["burgundy"])
accent_teal     <- as.character(artsy["teal"])
ink         <- "#1c1a19"
gray_dark   <- "#3b3b3b"
gray_mid    <- "#888888"
gray_light  <- "#dddddd"
paper       <- "#fcfaf5"
ramp_seq    <- c("#fdf6ec", "#f4d8b6", "#e89c7a", "#bc4838", "#5e1923")

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
last_year_row <- philly_trend[philly_trend$year == max(philly_trend$year), ]

p1_trend <- ggplot(philly_trend, aes(x = year, y = foreign_bornE)) +
  geom_line(color = ink, linewidth = 0.7) +
  geom_point(aes(color = year == max(year)), size = 3) +
  geom_text(aes(label = scales::comma(foreign_bornE)),
            vjust = -1.3, size = 3.0, color = gray_dark, family = "sans") +
  annotate("curve",
           x = 2022.5, xend = last_year_row$year - 0.1,
           y = last_year_row$foreign_bornE + 10000,
           yend = last_year_row$foreign_bornE + 2500,
           curvature = -0.3,
           arrow = arrow(length = unit(2, "mm"), type = "closed"),
           color = accent_burgundy, linewidth = 0.4) +
  annotate("text",
           x = 2022.5, y = last_year_row$foreign_bornE + 14000,
           label = sprintf("First decline in a decade\n%.1f%% vs. prior year",
                           last_year_row$yoy_change),
           hjust = 1, size = 3.4, lineheight = 0.95,
           color = accent_burgundy, family = "sans", fontface = "bold") +
  scale_color_manual(values = c("TRUE" = accent_burgundy, "FALSE" = ink),
                     guide = "none") +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0.05, 0.15))) +
  scale_x_continuous(breaks = years_to_pull) +
  labs(
    title    = "A decade of growth, then a 2024 reversal",
    subtitle = "Philadelphia County foreign-born population, 2014-2024",
    x = NULL, y = NULL,
    caption  = "Source: U.S. Census Bureau, ACS 5-year estimates"
  )

print(p1_trend)
ggsave("output/chart1_pop_trend.png", p1_trend,
       width = 8, height = 6, dpi = 200, bg = paper)


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
  filter(!is.na(eng_factor))

wage_by_eng$bar_color <- as.character(artsy["sage"])
wage_by_eng$bar_color[wage_by_eng$eng_factor == "Very well"]  <-
  as.character(artsy["burgundy"])
wage_by_eng$bar_color[wage_by_eng$eng_factor == "Not at all"] <-
  as.character(artsy["terracotta"])

gap_dollars <- wage_by_eng$mean_wage[wage_by_eng$eng_factor == "Very well"] -
  wage_by_eng$mean_wage[wage_by_eng$eng_factor == "Not at all"]

p2_wage <- ggplot(wage_by_eng,
                  aes(x = eng_factor, y = mean_wage, fill = bar_color)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = scales::dollar(round(mean_wage, -2))),
            vjust = -0.6, size = 3.6, color = ink,
            family = "sans", fontface = "bold") +
  scale_fill_identity() +
  scale_y_continuous(labels = scales::dollar,
                     expand = expansion(mult = c(0.02, 0.18))) +
  labs(
    title = sprintf("The $%s raw language gap",
                    scales::comma(round(gap_dollars, -2))),
    subtitle = "Average annual wages, employed foreign-born, Philadelphia",
    x = NULL, y = NULL,
    caption = paste0("Source: ACS 5-year PUMS (2020-2024). N = ",
                     scales::comma(nrow(employed_fb)), ".")
  )

print(p2_wage)
ggsave("output/chart2_wage_by_eng.png", p2_wage,
       width = 9, height = 5.5, dpi = 200, bg = paper)


# -----------------------------------------------------------------------------
# Step 17. Tract-indicator correlation heatmap (saves PNG)
# -----------------------------------------------------------------------------
cor_df <- tracts %>%
  st_drop_geometry() %>%
  select(
    `% foreign-born`         = pct_foreign_born,
    `% poverty (FB)`         = pct_poverty_fb,
    `% lang. isolated`       = pct_lang_isolated,
    `Unemp. rate`            = unemp_rate,
    `% homeowner`            = pct_homeowner,
    `Median HH income ($)`   = median_hh_incomeE
  )

cor_matrix <- cor(cor_df, use = "pairwise.complete.obs")

p_cor <- ggcorrplot(
  cor_matrix,
  hc.order      = FALSE,
  type          = "lower",
  lab           = TRUE,
  lab_size      = 3,
  outline.color = "white",
  colors        = c(accent_teal, "#f4ead4", accent_burgundy),
  ggtheme       = theme_editorial
) +
  labs(
    title    = "Tract-level indicator correlations",
    subtitle = "Pearson correlation, Philadelphia census tracts",
    caption  = "Source: ACS 5-year estimates (2020-2024)"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_cor)
ggsave("output/chart_correlation_heatmap.png", p_cor,
       width = 9, height = 7, dpi = 200, bg = paper)


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
# Step 21. Chart 3 — Mincer coefficient plot (saves PNG)
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
                      round(summary(mincer_fit)$adj.r.squared, 3),
                      ". Reference: English 'Not at all', Education '<HS'.")
  )

print(p3_coef)
ggsave("output/chart3_mincer_coefs.png", p3_coef,
       width = 10, height = 6, dpi = 200, bg = paper)


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
vip_data$bar_color <- as.character(artsy["sage"])
vip_data$bar_color[vip_data$Variable == "English proficiency"] <- accent_burgundy
vip_data$bar_color[vip_data$Variable == "Education"]           <-
  as.character(artsy["mustard"])

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
    caption  = paste0("Source: ranger, 500 trees. ",
                      "Out-of-bag R² = ", round(rf_fit$r.squared, 3))
  )

print(p4_vip)
ggsave("output/chart4_vip.png", p4_vip,
       width = 9, height = 5.5, dpi = 200, bg = paper)


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
    caption  = "Source: Random Forest partial dependence (manual)."
  )

print(p5_pdp)
ggsave("output/chart5_pdp.png", p5_pdp,
       width = 9, height = 5.5, dpi = 200, bg = paper)


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
  filter(grepl("eng_factor|edu_collapsed", term)) %>%
  mutate(
    Variable = recode(term,
                      "eng_factorNot well"                = "English: Not well",
                      "eng_factorWell"                    = "English: Well",
                      "eng_factorVery well"               = "English: Very well",
                      "edu_collapsedHS/GED"               = "Education: HS/GED",
                      "edu_collapsedSome college/Assoc."  = "Education: Some college",
                      "edu_collapsedBachelor's+"          = "Education: Bachelor's+"
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
      TRUE            ~ ""
    )
  ) %>%
  select(Variable, Outcome, `Odds Ratio`, `95% CI`, Sig) %>%
  arrange(Outcome, Variable)

multi_gt <- multi_table %>%
  gt(groupname_col = "Outcome") %>%
  tab_header(
    title    = "Multinomial logit: English and education effects on labor-force status",
    subtitle = "Reference categories: English 'Not at all', Education '<HS'"
  ) %>%
  tab_source_note("Source: ACS 5-year PUMS (2020-2024), foreign-born ages 16-65.") %>%
  tab_source_note("Significance: *** p<0.001, ** p<0.01, * p<0.05")

print(multi_gt)
gtsave(multi_gt, "output/table_multinomial.html")


# -----------------------------------------------------------------------------
# Step 30. Multi-model coefficient heatmap (saves PNG)
# -----------------------------------------------------------------------------
mincer_coef <- coef(mincer_fit)
mincer_long <- data.frame(
  term    = names(mincer_coef),
  value   = as.numeric(mincer_coef),
  Outcome = "Wages (log)",
  stringsAsFactors = FALSE
) %>%
  filter(grepl("eng_factor|edu_collapsed", term))

multi_long <- tidy(multi_fit, conf.int = FALSE, exponentiate = FALSE) %>%
  filter(term != "(Intercept)",
         grepl("eng_factor|edu_collapsed", term)) %>%
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
                      "edu_collapsedBachelor's+"          = "Education: Bachelor's+"
    ),
    Variable = factor(Variable, levels = c(
      "Education: Bachelor's+",
      "Education: Some college",
      "Education: HS/GED",
      "English: Very well",
      "English: Well",
      "English: Not well"
    )),
    plot_value = case_when(
      Outcome == "Wages (log)" ~ value,
      TRUE                      ~ -value
    )
  ) %>%
  group_by(Outcome) %>%
  mutate(plot_value_std = plot_value / max(abs(plot_value), na.rm = TRUE)) %>%
  ungroup()

p6_heatmap <- ggplot(heat_df, aes(x = Outcome, y = Variable,
                                  fill = plot_value_std)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(
    aes(label = case_when(
      Outcome == "Wages (log)" ~ sprintf("%+.0f%%", (exp(value) - 1) * 100),
      TRUE                     ~ sprintf("OR %.2f", exp(value))
    )),
    color = ink, size = 3.6, fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = accent_teal, mid = "#f4ead4", high = accent_burgundy,
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
    caption  = "Source: Mincer linear regression + multinomial logit."
  ) +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 9, color = gray_dark),
    axis.text.x.top = element_text(angle = 0, hjust = 0.5, face = "bold",
                                   color = ink),
    axis.text.y     = element_text(face = "bold", color = ink),
    panel.grid      = element_blank(),
    axis.line.x     = element_blank(),
    axis.ticks.x    = element_blank()
  )

print(p6_heatmap)
ggsave("output/chart6_model_heatmap.png", p6_heatmap,
       width = 11, height = 6, dpi = 200, bg = paper)


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
cat("Avg neighbors per tract:", round(mean(card(nb)), 1), "\n")


# -----------------------------------------------------------------------------
# Step 32. Global Moran's I
# -----------------------------------------------------------------------------
moran_pov <- moran.test(spatial_df$pct_poverty_fb, lw, zero.policy = TRUE)
moran_li  <- moran.test(spatial_df$pct_lang_isolated, lw, zero.policy = TRUE)
moran_fb  <- moran.test(spatial_df$pct_foreign_born, lw, zero.policy = TRUE)

cat("Foreign-born poverty rate: I =",
    round(moran_pov$estimate[1], 3),
    "  p =", format.pval(moran_pov$p.value, digits = 3), "\n")
cat("Linguistic isolation:      I =",
    round(moran_li$estimate[1], 3),
    "  p =", format.pval(moran_li$p.value, digits = 3), "\n")
cat("Foreign-born share:        I =",
    round(moran_fb$estimate[1], 3),
    "  p =", format.pval(moran_fb$p.value, digits = 3), "\n")


# -----------------------------------------------------------------------------
# Step 33. Local Moran's I (LISA)
# -----------------------------------------------------------------------------
local_m <- localmoran(spatial_df$pct_poverty_fb, lw, zero.policy = TRUE)

x_scaled   <- as.numeric(scale(spatial_df$pct_poverty_fb))
lag_x      <- lag.listw(lw, spatial_df$pct_poverty_fb, zero.policy = TRUE)
lag_scaled <- as.numeric(scale(lag_x))

spatial_df$local_I <- local_m[, "Ii"]
spatial_df$p_val   <- local_m[, "Pr(z != E(Ii))"]

spatial_df$cluster <- "Not significant"
spatial_df$cluster[spatial_df$p_val < 0.05 & x_scaled > 0 & lag_scaled > 0] <- "High-High (hot spot)"
spatial_df$cluster[spatial_df$p_val < 0.05 & x_scaled < 0 & lag_scaled < 0] <- "Low-Low (cold spot)"
spatial_df$cluster[spatial_df$p_val < 0.05 & x_scaled > 0 & lag_scaled < 0] <- "High-Low (outlier)"
spatial_df$cluster[spatial_df$p_val < 0.05 & x_scaled < 0 & lag_scaled > 0] <- "Low-High (outlier)"

print(table(spatial_df$cluster))


# -----------------------------------------------------------------------------
# Step 34. Map 1 — foreign-born share static choropleth (saves PNG)
# -----------------------------------------------------------------------------
spatial_df$fb_bin <- cut(
  spatial_df$pct_foreign_born,
  breaks = c(-0.1, 5, 10, 20, 35, 100),
  labels = c("Under 5%", "5-10%", "10-20%", "20-35%", "Over 35%")
)

map1_fb <- ggplot(spatial_df) +
  geom_sf(aes(fill = fb_bin), color = "white", linewidth = 0.15) +
  scale_fill_manual(
    values = c("Under 5%"   = "#fdf6ec",
               "5-10%"      = "#f4d8b6",
               "10-20%"     = "#e89c7a",
               "20-35%"     = "#bc4838",
               "Over 35%"   = "#5e1923"),
    na.value = "#dddddd",
    name = "% foreign-born"
  ) +
  labs(
    title    = "Where Philadelphia's immigrants live",
    subtitle = "Share of foreign-born population by census tract, 2020-2024",
    caption  = "Source: ACS 5-year estimates"
  ) +
  theme_void(base_size = 11) +
  theme(
    text             = element_text(family = "sans", color = gray_dark),
    plot.title       = element_text(face = "bold", size = 15, color = ink,
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 11, color = gray_mid,
                                    margin = margin(b = 12)),
    plot.caption     = element_text(size = 9, color = gray_mid, hjust = 0,
                                    margin = margin(t = 12)),
    legend.position  = "right",
    legend.title     = element_text(size = 10, color = gray_dark),
    legend.text      = element_text(size = 9, color = gray_dark),
    plot.background  = element_rect(fill = paper, color = NA),
    plot.margin      = margin(20, 24, 16, 20)
  )

print(map1_fb)
ggsave("output/map1_fb_share.png", map1_fb,
       width = 9, height = 9, dpi = 200, bg = paper)


# -----------------------------------------------------------------------------
# Step 35. Map 2 — LISA cluster static (saves PNG)
# -----------------------------------------------------------------------------
map2_lisa <- ggplot(spatial_df) +
  geom_sf(aes(fill = cluster), color = "white", linewidth = 0.15) +
  scale_fill_manual(
    values = c(
      "High-High (hot spot)"  = accent_burgundy,
      "Low-Low (cold spot)"   = accent_teal,
      "High-Low (outlier)"    = as.character(artsy["mustard"]),
      "Low-High (outlier)"    = as.character(artsy["rose"]),
      "Not significant"       = "#eeeeee"
    ),
    name = "LISA cluster"
  ) +
  labs(
    title    = "Where immigrant poverty clusters",
    subtitle = "Local Moran's I cluster classification, p < 0.05",
    caption  = paste0("Source: ACS 5-year estimates (2020-2024). ",
                      "High-High = hot spot, priority zone.")
  ) +
  theme_void(base_size = 11) +
  theme(
    text             = element_text(family = "sans", color = gray_dark),
    plot.title       = element_text(face = "bold", size = 15, color = ink,
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 11, color = gray_mid,
                                    margin = margin(b = 12)),
    plot.caption     = element_text(size = 9, color = gray_mid, hjust = 0,
                                    margin = margin(t = 12)),
    legend.position  = "right",
    legend.title     = element_text(size = 10, color = gray_dark),
    legend.text      = element_text(size = 9, color = gray_dark),
    plot.background  = element_rect(fill = paper, color = NA),
    plot.margin      = margin(20, 24, 16, 20)
  )

print(map2_lisa)
ggsave("output/map2_lisa.png", map2_lisa,
       width = 9, height = 9, dpi = 200, bg = paper)


# -----------------------------------------------------------------------------
# Step 36. Map 3 — interactive Leaflet LISA (saves standalone HTML)
# -----------------------------------------------------------------------------
pal_lisa <- colorFactor(
  palette = c(accent_burgundy, accent_teal,
              as.character(artsy["mustard"]),
              as.character(artsy["rose"]),
              "#dddddd"),
  levels  = c("High-High (hot spot)", "Low-Low (cold spot)",
              "High-Low (outlier)", "Low-High (outlier)",
              "Not significant")
)

map_leaflet <- leaflet(spatial_df,
                       options = leafletOptions(attributionControl = FALSE)) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolygons(
    fillColor   = ~pal_lisa(cluster),
    fillOpacity = 0.82,
    color       = "white", weight = 0.4,
    label = ~lapply(paste0(
      "<strong>Tract ", GEOID, "</strong><br>",
      "Cluster: <strong>", cluster, "</strong><br>",
      "FB poverty: ",
      ifelse(is.na(pct_poverty_fb), "n/a",
             paste0(round(pct_poverty_fb, 1), "%")), "<br>",
      "FB share: ", round(pct_foreign_born, 1), "%<br>",
      "Ling. isolation: ", round(pct_lang_isolated, 1), "%"
    ), htmltools::HTML),
    highlightOptions = highlightOptions(weight = 2, color = ink,
                                        bringToFront = TRUE)
  ) %>%
  addProviderTiles(providers$CartoDB.PositronOnlyLabels,
                   options = providerTileOptions(opacity = 0.55)) %>%
  addLegend(
    position = "bottomright",
    pal = pal_lisa, values = ~cluster,
    title = htmltools::HTML(paste0(
      "<strong>LISA cluster</strong><br>",
      "<span style='font-weight:400'>foreign-born poverty</span>"
    )),
    opacity = 0.9
  )

print(map_leaflet)
saveWidget(map_leaflet, "output/map3_lisa_interactive.html",
           selfcontained = TRUE)


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