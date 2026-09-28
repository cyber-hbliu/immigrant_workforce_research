
# Sections
#   1. MSA PUMA lists (2010 and 2020 vintages)
#   2. Pull PUMS for three non-overlapping windows, cached to rds
#   3. Harmonize, construct variables, PUMA context, analysis sample
#   4. Descriptives with replicate weights
#   5. OLS with occupation + PUMA fixed effects
#   6. Bleakley-Chin age-at-arrival IV (cutoff 9, sensitivity at 12)
#   7. Borjas cohort x years-since-migration
#   8. Multilevel: limited English x PUMA immigrant proportion (both recent windows)
#   9. Oaxaca-Blinder native vs foreign-born
#  10. Residential mobility: move classification, multinomial logit, wage by move
#  11. Pseudo-panel: cohort x region cells across windows
#  12. Tables and web data
#  13. Figures (framework is drawn in Python; eight paper figures + one appendix figure)
#  14. New York MSA comparison of the two-level model (Q3 only)
#  15. Cohort contrasts, duration-matched CIs, own-language density (Philadelphia and New York),
#      multinomial logit with replicate weights
#  16. Group slopes, Satterthwaite p-values, random-slope variances
#  17. Migration-area selection test
#  18. Native-born placebo and native-wage control

library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
library(fixest)
library(lme4)
library(survey)
library(srvyr)
library(nnet)
library(broom)
library(broom.mixed)
library(jsonlite)

setwd("/Users/cyberhbliu/Desktop/PERSONAL/2026portfolio/immigrant")
census_api_key("a9e713a06a0a0f8ec8531e047c9d01e7d9f507d9", install = TRUE, overwrite = TRUE)
options(tigris_use_cache = TRUE)
dir.create("results", showWarnings = FALSE)
dir.create("cache", showWarnings = FALSE)
dir.create("web/data", recursive = TRUE, showWarnings = FALSE)

states <- c("PA", "NJ", "DE", "MD")

wmean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
wmedian <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  o <- order(x[ok]); x <- x[ok][o]; w <- w[ok][o]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}

# 1. MSA PUMA lists ---------------------------------------------------------
sf_use_s2(FALSE)
msa <- core_based_statistical_areas(year = 2023, cb = TRUE) %>%
  filter(str_detect(NAME, "Philadelphia-Camden-Wilmington")) %>%
  st_transform(4326)

puma_in_msa <- function(state, year) {
  p <- pumas(state = state, year = year, cb = FALSE) %>% st_transform(4326)
  code_col <- if (year >= 2022) "PUMACE20" else "PUMACE10"
  inside <- st_within(st_point_on_surface(p), msa, sparse = FALSE)[, 1]
  p[[code_col]][inside]
}
puma_list_20 <- set_names(map(states, puma_in_msa, year = 2023), states)
puma_list_10 <- set_names(map(states, puma_in_msa, year = 2019), states)
sf_use_s2(TRUE)

# 2. Pull PUMS --------------------------------------------------------------
# If cache/pums_*.rds exist from an earlier run WITHOUT the MIG variables,
# delete them first so the pull re-runs.
base_vars <- c("AGEP", "SEX", "RAC1P", "HISP", "NATIVITY", "CIT", "SCHL", "ESR",
               "POBP", "WAOB", "POVPIP", "WAGP", "ADJINC", "ENG", "COW", "OCCP",
               "WKHP", "YOEP", "MAR", "DIS", "PUMA",
               "MIG", "MIGSP", "MIGPUMA", "GRPIP")

pull_window <- function(year, puma_list, weeks_var) {
  f <- file.path("cache", paste0("pums_", year, ".rds"))
  if (file.exists(f)) {
    out <- readRDS(f)
    if (!"MIG" %in% names(out)) stop("Old cache without MIG variables: delete ", f, " and re-run.")
    return(out)
  }
  out <- map_dfr(states, function(st) {
    get_pums(variables = c(base_vars, weeks_var), state = st, puma = puma_list[[st]],
             survey = "acs5", year = year, rep_weights = "person") %>%
      mutate(across(everything(), as.character), STATE = st)
  })
  saveRDS(out, f)
  out
}

p16 <- pull_window(2016, puma_list_10, "WKW")  %>% mutate(window = "2012-2016")
p21 <- pull_window(2021, puma_list_10, "WKW")  %>% mutate(window = "2017-2021")
p24 <- pull_window(2024, puma_list_20, "WKWN") %>% mutate(window = "2022-2024")

# 3. Harmonize and construct variables ------------------------------------
wkw_mid <- c("1" = 51, "2" = 48.5, "3" = 43.5, "4" = 33, "5" = 20, "6" = 7)

english_origin <- c("119", "138", "139", "140", "141", "142",   # Ireland, UK
                    "301", "501", "515",                       # Canada, Australia, NZ
                    "310", "323", "324", "333", "341", "368",  # Belize, Bahamas, Barbados
                    "321", "330", "338", "339", "340")         # Jamaica, T&T, Guyana, Caribbean

city_puma20 <- c("03216", "03221", "03222", "03223", "03224",
                 "03225", "03227", "03228", "03229", "03230", "03231")
city_puma10 <- sprintf("032%02d", 1:11)

harmonize <- function(d) {
  d <- d %>%
    mutate(across(everything(), as.character)) %>%
    # older vintages come back numeric and lose leading zeros; restore them
    mutate(PUMA    = str_pad(PUMA, 5, pad = "0"),
           HISP    = str_pad(HISP, 2, pad = "0"),
           OCCP    = str_pad(OCCP, 4, pad = "0"),
           POBP    = str_pad(POBP, 3, pad = "0"),
           MIGSP   = str_pad(MIGSP, 3, pad = "0")) %>%
    mutate(across(c(AGEP, WAGP, ADJINC, POVPIP, WKHP, YOEP, PWGTP,
                    starts_with("PWGTP")), as.numeric))
  if ("WKWN" %in% names(d)) {
    d <- d %>% mutate(weeks = as.numeric(WKWN))
  } else {
    d <- d %>% mutate(weeks = unname(wkw_mid[as.character(WKW)]))
  }
  if (!"MIGPUMA" %in% names(d)) {
    d <- d %>% mutate(MIGPUMA = coalesce(
      if ("MIGPUMA20" %in% names(d)) na_if(MIGPUMA20, "bbbbb") else NA_character_,
      if ("MIGPUMA10" %in% names(d)) na_if(MIGPUMA10, "bbbbb") else NA_character_))
  }
  d <- d %>% mutate(MIGPUMA = str_pad(MIGPUMA, 5, pad = "0"))
  d %>%
    mutate(
      int_year   = as.integer(substr(SERIALNO, 1, 4)),
      fb         = NATIVITY == "2",
      eng_score  = case_when(
        ENG == "1" ~ 3, ENG == "2" ~ 2, ENG == "3" ~ 1, ENG == "4" ~ 0,
        ENG %in% c("b", "0") & AGEP >= 5 ~ 3,
        TRUE ~ NA_real_),
      limited_eng = eng_score <= 1,
      female     = SEX == "2",
      hispanic   = HISP != "01",
      race       = case_when(hispanic ~ "Hispanic", RAC1P == "1" ~ "White",
                             RAC1P == "2" ~ "Black", RAC1P == "6" ~ "Asian",
                             TRUE ~ "Other"),
      schl_num   = suppressWarnings(as.numeric(SCHL)),
      edu        = case_when(schl_num <= 15 ~ "lt_hs", schl_num <= 17 ~ "hs",
                             schl_num <= 20 ~ "some_college",
                             schl_num == 21 ~ "ba", schl_num >= 22 ~ "grad",
                             TRUE ~ NA_character_),
      married    = MAR == "1",
      disabled   = DIS == "1",
      employed   = ESR %in% c("1", "2"),
      wage_real  = WAGP * ADJINC,                    # tidycensus ADJINC is a decimal
      hourly     = if_else(WKHP > 0 & weeks > 0, wage_real / (WKHP * weeks), NA_real_),
      log_hw     = if_else(hourly >= 2 & hourly <= 500, log(hourly), NA_real_),
      ysm        = if_else(fb, int_year - YOEP, 0),
      age_arrival = if_else(fb, AGEP - ysm, NA_real_),
      cohort     = case_when(!fb ~ "native",
                             YOEP < 1990 ~ "pre1990", YOEP < 2000 ~ "1990s",
                             YOEP < 2010 ~ "2000s", YOEP < 2020 ~ "2010s",
                             TRUE ~ "2020s"),
      eng_origin = fb & POBP %in% english_origin,
      region     = case_when(WAOB == "3" ~ "Latin America", WAOB == "4" ~ "Asia",
                             WAOB == "5" ~ "Europe", WAOB == "6" ~ "Africa",
                             fb ~ "Other", TRUE ~ "Native"),
      in_city    = STATE == "PA" & PUMA %in% c(city_puma20, city_puma10),
      migsp_num  = suppressWarnings(as.numeric(MIGSP)),
      grpip      = suppressWarnings(as.numeric(GRPIP)),
      puma_id    = paste(STATE, PUMA, window, sep = "_"))
}

pums <- bind_rows(harmonize(p16), harmonize(p21), harmonize(p24)) %>%
  filter(window != "2022-2024" | int_year >= 2022)

# checks
print(pums %>% count(window, in_city))          # every window needs TRUE rows
print(pums %>% count(window, fb))
print(pums %>% filter(!is.na(hourly)) %>% group_by(window) %>%
        summarise(median_hourly = median(hourly), n = n(), .groups = "drop"))
print(pums %>% filter(fb) %>%
        summarise(min_arrival = min(age_arrival, na.rm = TRUE),
                  n_negative = sum(age_arrival < 0, na.rm = TRUE)))

# PUMA context, everyone included, by window
puma_ctx <- pums %>%
  group_by(puma_id) %>%
  summarise(
    puma_fb_prop = sum(PWGTP[fb]) / sum(PWGTP) * 100,
    puma_limited_eng_prop = sum(PWGTP[fb & limited_eng %in% TRUE]) / sum(PWGTP[fb]) * 100,
    .groups = "drop")

puma_same_region <- pums %>% filter(fb) %>%
  count(puma_id, WAOB, wt = PWGTP, name = "n_region") %>%
  group_by(puma_id) %>% mutate(same_region_prop = n_region / sum(n_region) * 100) %>%
  ungroup() %>% select(puma_id, WAOB, same_region_prop)

pums <- pums %>%
  left_join(puma_ctx, by = "puma_id") %>%
  left_join(puma_same_region, by = c("puma_id", "WAOB"))

# Analysis sample: wage and salary workers 25-64 with a valid hourly wage
w <- pums %>%
  filter(AGEP >= 25, AGEP <= 64, employed, COW %in% c("1", "2", "3", "4", "5"),
         !is.na(log_hw), !is.na(eng_score), !is.na(edu), ysm >= 0) %>%
  mutate(age2 = AGEP^2, ysm2 = ysm^2)

saveRDS(pums, "cache/pums_harmonized.rds")

# 4. Descriptives with replicate weights ----------------------------------
des <- w %>% filter(window == "2022-2024") %>%
  to_survey(type = "person", design = "rep_weights")

desc_eng <- des %>%
  filter(fb) %>%
  group_by(eng_score) %>%
  summarise(median_hourly = survey_median(hourly, vartype = "ci"),
            mean_log_hw   = survey_mean(log_hw, vartype = "se"),
            n = unweighted(n()))
write_csv(desc_eng, "results/t1_hourly_wage_by_english_2022_2024.csv")

# 5. OLS ------------------------------------------------------------------
m_ols <- feols(
  log_hw ~ fb + eng_score + AGEP + age2 + female + race + edu + married + disabled
  | OCCP + puma_id + int_year,
  data = w, weights = ~PWGTP, cluster = ~puma_id)

# 6. Bleakley-Chin IV -----------------------------------------------------
iv <- w %>% filter(fb, age_arrival >= 0, age_arrival < 18) %>%
  mutate(late    = pmax(0, age_arrival - 9),
         late12  = pmax(0, age_arrival - 12),
         non_eng = !eng_origin,
         z   = late * non_eng,
         z12 = late12 * non_eng)

m_iv_first <- feols(eng_score ~ z + late + non_eng + AGEP + age2 + female + race + edu
                    + married + disabled | puma_id + int_year,
                    data = iv, weights = ~PWGTP, cluster = ~puma_id)
m_iv <- feols(log_hw ~ late + non_eng + AGEP + age2 + female + race + edu + married + disabled
              | puma_id + int_year | eng_score ~ z,
              data = iv, weights = ~PWGTP, cluster = ~puma_id)
m_iv_ols <- feols(log_hw ~ eng_score + late + non_eng + AGEP + age2 + female + race + edu
                  + married + disabled | puma_id + int_year,
                  data = iv, weights = ~PWGTP, cluster = ~puma_id)
m_iv_placebo <- feols(log_hw ~ late + AGEP + age2 + female + race + edu + married + disabled
                      | puma_id + int_year,
                      data = iv %>% filter(eng_origin), weights = ~PWGTP, cluster = ~puma_id)
# robustness: cutoff at 12
m_iv_12 <- feols(log_hw ~ late12 + non_eng + AGEP + age2 + female + race + edu + married + disabled
                 | puma_id + int_year | eng_score ~ z12,
                 data = iv, weights = ~PWGTP, cluster = ~puma_id)

# weak-instrument diagnostics. Just-identified, so the cluster-robust first-stage
# Wald F equals the Kleibergen-Paap rk Wald F. Anderson-Rubin CI by grid inversion.
iv_diag <- fitstat(m_iv, ~ ivf1 + ivwald1)
kp_f <- iv_diag$ivwald1$stat
ar_grid <- seq(-0.6, 1.6, by = 0.01)
ar_p <- map_dbl(ar_grid, function(b0) {
  m <- feols(I(log_hw - b0 * eng_score) ~ z + late + non_eng + AGEP + age2 + female + race + edu
             + married + disabled | puma_id + int_year,
             data = iv, weights = ~PWGTP, cluster = ~puma_id)
  pvalue(m)["z"]
})
ar_ci <- range(ar_grid[ar_p > 0.05])
write_csv(tibble(stat = c("first_stage_F", "kleibergen_paap_F", "AR_ci_low", "AR_ci_high"),
                 value = c(iv_diag$ivf1$stat, kp_f, ar_ci[1], ar_ci[2])),
          "results/t3_iv_diagnostics.csv")

# 7. Cohort x years since migration ---------------------------------------
m_cohort <- feols(
  log_hw ~ i(cohort, ref = "native") + ysm + ysm2 + AGEP + age2 + female + race + edu
  + married + disabled | OCCP + puma_id + int_year,
  data = w, weights = ~PWGTP, cluster = ~puma_id)
m_cohort_eng <- feols(
  log_hw ~ i(cohort, ref = "native") + ysm + ysm2 + eng_score + AGEP + age2 + female
  + race + edu + married + disabled | OCCP + puma_id + int_year,
  data = w, weights = ~PWGTP, cluster = ~puma_id)

# 8. Multilevel: density x limited English -------------------------------------------
make_ml <- function(win) {
  w %>% filter(fb, window == win) %>%
    mutate(wt = PWGTP / mean(PWGTP),
           puma_fb_c = (puma_fb_prop - mean(puma_fb_prop)) / sd(puma_fb_prop),
           same_region_c = (same_region_prop - mean(same_region_prop, na.rm = TRUE)) /
             sd(same_region_prop, na.rm = TRUE))
}
ml   <- make_ml("2022-2024")
ml21 <- make_ml("2017-2021")

m_ml_null <- lmer(log_hw ~ 1 + (1 | puma_id), data = ml, weights = wt, REML = FALSE)
icc <- as.numeric(VarCorr(m_ml_null)$puma_id[1]) /
  (as.numeric(VarCorr(m_ml_null)$puma_id[1]) + sigma(m_ml_null)^2)

ml_formula <- log_hw ~ limited_eng * puma_fb_c + AGEP + age2 + female + race + edu +
  married + disabled + ysm + ysm2 + (1 + limited_eng | puma_id)

m_ml    <- lmer(ml_formula, data = ml,   weights = wt, REML = FALSE)
m_ml_21 <- lmer(ml_formula, data = ml21, weights = wt, REML = FALSE)
m_ml_region <- lmer(log_hw ~ limited_eng * same_region_c + puma_fb_c + AGEP + age2 + female
                    + race + edu + married + disabled + ysm + ysm2 + (1 + limited_eng | puma_id),
                    data = ml, weights = wt, REML = FALSE)

# occupation FE and heterogeneity by region (regions with enough observations only)
m_ml_occ <- feols(log_hw ~ limited_eng * puma_fb_c + AGEP + age2 + female + race + edu
                  + married + disabled + ysm + ysm2 | OCCP,
                  data = ml, weights = ~PWGTP, cluster = ~puma_id)
m_ml_by_region <- feols(log_hw ~ limited_eng * puma_fb_c + AGEP + age2 + female + edu
                        + married + disabled + ysm + ysm2 | OCCP,
                        data = ml %>% filter(region %in% c("Latin America", "Asia", "Europe", "Africa")),
                        weights = ~PWGTP, cluster = ~puma_id, split = ~region)

# 9. Oaxaca-Blinder -------------------------------------------------------
ob <- w %>% filter(window == "2022-2024") %>%
  mutate(across(c(female, married, disabled), as.numeric))
X_terms <- ~ AGEP + age2 + female + race + edu + married + disabled

oaxaca_twofold <- function(d, f, group, wvar = "PWGTP") {
  X  <- model.matrix(f, d)
  y  <- d$log_hw; wt <- d[[wvar]]; g <- d[[group]]
  fit <- function(rows) lm.wfit(X[rows, , drop = FALSE], y[rows], wt[rows])$coefficients
  b_a <- fit(!g); b_b <- fit(g); b_p <- fit(rep(TRUE, nrow(d)))
  xbar <- function(rows) colSums(X[rows, ] * wt[rows]) / sum(wt[rows])
  xa <- xbar(!g); xb <- xbar(g)
  gap <- sum(xa * b_a) - sum(xb * b_b)
  explained   <- sum((xa - xb) * b_p)
  unexplained <- gap - explained
  detail <- tibble(term = names(b_p),
                   explained = (xa - xb) * b_p,
                   unexplained = xa * (b_a - b_p) + xb * (b_p - b_b))
  list(gap = gap, explained = explained, unexplained = unexplained, detail = detail)
}
ob_res     <- oaxaca_twofold(ob, X_terms, group = "fb")
ob_res_eng <- oaxaca_twofold(ob, update(X_terms, ~ . + eng_score), group = "fb")

# 10. Residential mobility ------------------------------------------------
msa_state_codes <- c(42, 34, 10, 24)

# The city's MIGPUMA, found empirically per window. Check the printout: one
# code should dominate.
city_migpuma_tab <- pums %>%
  filter(in_city, MIG == "3", migsp_num == 42, !is.na(MIGPUMA)) %>%
  count(window, MIGPUMA, wt = PWGTP) %>%
  group_by(window) %>% slice_max(n, n = 3) %>% ungroup()
print(city_migpuma_tab)
city_migpuma <- city_migpuma_tab %>%
  group_by(window) %>% slice_max(n, n = 1) %>% ungroup() %>%
  select(window, city_migpuma = MIGPUMA)

pums <- pums %>%
  left_join(city_migpuma, by = "window") %>%
  mutate(
    from_city = MIG == "3" & migsp_num == 42 & MIGPUMA == city_migpuma,
    from_msa  = MIG == "3" & migsp_num %in% msa_state_codes,
    move_type = case_when(
      MIG == "1"                        ~ "stayed",
      MIG == "2"                        ~ "from_abroad",
      in_city  & from_city              ~ "within_city",
      !in_city & from_city              ~ "city_to_suburb",
      in_city  & from_msa & !from_city  ~ "suburb_to_city",
      !in_city & from_msa & !from_city  ~ "within_suburbs",
      MIG == "3" & migsp_num <= 56      ~ "from_other_state",
      TRUE ~ NA_character_),
    move_type = factor(move_type, levels = c("stayed", "within_city", "within_suburbs",
                                             "city_to_suburb", "suburb_to_city",
                                             "from_other_state", "from_abroad")))

move_rates <- pums %>%
  filter(AGEP >= 25, AGEP <= 64, !is.na(move_type)) %>%
  count(window, fb, move_type, wt = PWGTP, name = "n") %>%
  group_by(window, fb) %>% mutate(pct = n / sum(n) * 100) %>% ungroup()
write_csv(move_rates, "results/t6_move_rates.csv")

# Multinomial logit. Sample: foreign-born 25-64 who lived in the MSA a year ago.
mn_sample <- pums %>%
  filter(fb, AGEP >= 25, AGEP <= 64,
         move_type %in% c("stayed", "within_city", "within_suburbs",
                          "city_to_suburb", "suburb_to_city"),
         !is.na(edu), !is.na(eng_score), region != "Other", ysm >= 0) %>%
  mutate(move_type = droplevels(move_type),
         age2 = AGEP^2, ysm2 = ysm^2,
         wt = PWGTP / mean(PWGTP))

m_mn_all <- multinom(move_type ~ eng_score + edu + ysm + ysm2 + cohort + region
                     + AGEP + age2 + female + married + window,
                     data = mn_sample, weights = wt, maxit = 500, trace = FALSE)

mn_workers <- mn_sample %>%
  filter(!is.na(log_hw), employed, COW %in% c("1", "2", "3", "4", "5"))
m_mn_wage <- multinom(move_type ~ log_hw + eng_score + edu + ysm + ysm2 + cohort + region
                      + AGEP + age2 + female + married + window,
                      data = mn_workers, weights = wt, maxit = 500, trace = FALSE)
m_mn_rent <- multinom(move_type ~ log_hw + grpip + eng_score + edu + ysm + ysm2 + cohort
                      + region + AGEP + age2 + female + married + window,
                      data = mn_workers %>% filter(!is.na(grpip), grpip > 0),
                      weights = wt, maxit = 500, trace = FALSE)

# robustness: binary logit with clustered SE, city residents a year ago
m_bin <- feglm(I(move_type == "city_to_suburb") ~ log_hw + eng_score + edu + ysm + ysm2
               + cohort + region + AGEP + age2 + female + married + window,
               data = mn_workers %>% filter(in_city | move_type == "city_to_suburb"),
               family = binomial(), weights = ~PWGTP, cluster = ~puma_id)

mn_tab <- bind_rows(
  tidy(m_mn_all,  conf.int = TRUE) %>% mutate(model = "all"),
  tidy(m_mn_wage, conf.int = TRUE) %>% mutate(model = "workers_wage"),
  tidy(m_mn_rent, conf.int = TRUE) %>% mutate(model = "workers_wage_rent")) %>%
  mutate(rrr = exp(estimate))
write_csv(mn_tab, "results/t7_multinomial_moves.csv")

# Wage model with move type, foreign-born workers, relative to stayers
w_move <- w %>%
  filter(fb) %>%
  left_join(pums %>% select(SERIALNO, SPORDER, window, move_type),
            by = c("SERIALNO", "SPORDER", "window")) %>%
  filter(!is.na(move_type))

m_wage_move <- feols(
  log_hw ~ i(move_type, ref = "stayed") + eng_score + AGEP + age2 + female + race + edu
  + married + disabled + ysm + ysm2 | OCCP + puma_id + int_year,
  data = w_move, weights = ~PWGTP, cluster = ~puma_id)

# 11. Pseudo-panel --------------------------------------------------------
cells <- pums %>%
  filter(fb, AGEP >= 25, AGEP <= 64, ysm >= 0,
         cohort %in% c("pre1990", "1990s", "2000s", "2010s"),
         region %in% c("Latin America", "Asia", "Europe", "Africa")) %>%
  filter(!(cohort == "2010s" & window == "2012-2016")) %>%
  group_by(cohort, region, window) %>%
  summarise(
    n_unweighted     = n(),
    pop              = sum(PWGTP),
    suburb_prop      = sum(PWGTP[!in_city]) / sum(PWGTP) * 100,
    limited_eng_prop = sum(PWGTP[limited_eng %in% TRUE]) / sum(PWGTP[!is.na(limited_eng)]) * 100,
    median_hourly    = wmedian(hourly[!is.na(log_hw)], PWGTP[!is.na(log_hw)]),
    mean_log_hw      = wmean(log_hw, PWGTP),
    rent_burden      = wmean(grpip[grpip > 0], PWGTP[grpip > 0]),
    .groups = "drop") %>%
  mutate(cell = paste(cohort, region, sep = "_"))
write_csv(cells, "results/t8_pseudo_panel_cells.csv")

# same cells in 2024 dollars for the figure. ADJINC puts each window in its final-year
# dollars; CPI-U annual averages 2016 = 240.007, 2021 = 270.970, 2024 = 313.689.
cpi24 <- c("2012-2016" = 313.689 / 240.007, "2017-2021" = 313.689 / 270.970, "2022-2024" = 1)
cells_2024 <- cells %>%
  mutate(median_hourly = median_hourly * cpi24[window],
         mean_log_hw   = mean_log_hw + log(cpi24[window]))
write_csv(cells_2024, "results/t8_pseudo_panel_cells_2024usd.csv")

# duration-matched comparison (Table 6 in the paper)
dm <- cells %>% select(cohort, region, window, suburb_prop) %>%
  filter((cohort == "2000s" & window == "2012-2016") | (cohort == "2010s" & window == "2022-2024") |
           (cohort == "1990s" & window == "2012-2016") | (cohort == "2000s" & window == "2022-2024")) %>%
  mutate(pair = if_else(window == "2012-2016", paste0(cohort, "_in_2012_2016"), paste0(cohort, "_in_2022_2024"))) %>%
  select(region, pair, suburb_prop) %>% pivot_wider(names_from = pair, values_from = suburb_prop)
write_csv(dm, "results/t11_duration_matched.csv")

m_pp      <- feols(suburb_prop ~ mean_log_hw | cell + window,
                   data = cells, weights = ~pop, cluster = ~cell)
m_pp_eng  <- feols(suburb_prop ~ mean_log_hw + limited_eng_prop | cell + window,
                   data = cells, weights = ~pop, cluster = ~cell)
m_pp_rent <- feols(suburb_prop ~ mean_log_hw + rent_burden | cell + window,
                   data = cells, weights = ~pop, cluster = ~cell)

# 12. Tables and web data ----------------------------------------------------
etable(m_ols, m_cohort, m_cohort_eng, file = "results/t2_ols_cohort.tex", replace = TRUE)
etable(m_iv_first, m_iv_ols, m_iv, m_iv_placebo, m_iv_12, file = "results/t3_iv.tex", replace = TRUE)
etable(m_ml_occ, m_ml_by_region, file = "results/t4_multilevel_fe_checks.tex", replace = TRUE)
etable(m_bin, file = "results/t7b_binary_logit_city_to_suburb.tex", replace = TRUE)
etable(m_wage_move, file = "results/t9_wage_by_move_type.tex", replace = TRUE)
etable(m_pp, m_pp_eng, m_pp_rent, file = "results/t10_pseudo_panel_fe.tex", replace = TRUE)

ml_tab <- bind_rows(
  tidy(m_ml, effects = "fixed")        %>% mutate(model = "fb_prop_2022_2024"),
  tidy(m_ml_21, effects = "fixed")     %>% mutate(model = "fb_prop_2017_2021"),
  tidy(m_ml_region, effects = "fixed") %>% mutate(model = "same_region_2022_2024"))
write_csv(ml_tab, "results/t4_multilevel.csv")
write_csv(ob_res$detail, "results/t5_oaxaca_detail.csv")
write_csv(tibble(model = c("base", "with_english"),
                 gap = c(ob_res$gap, ob_res_eng$gap),
                 explained = c(ob_res$explained, ob_res_eng$explained),
                 unexplained = c(ob_res$unexplained, ob_res_eng$unexplained)),
          "results/t5_oaxaca_summary.csv")

landing <- pums %>%
  filter(fb, move_type == "from_abroad", window == "2022-2024") %>%
  count(region, in_city, wt = PWGTP, name = "n") %>%
  group_by(region) %>% mutate(pct = n / sum(n) * 100) %>% ungroup()
flows <- pums %>%
  filter(fb, move_type %in% c("city_to_suburb", "suburb_to_city")) %>%
  count(window, move_type, STATE, PUMA, wt = PWGTP, name = "n")
write_json(list(landing = landing, flows = flows, move_rates = move_rates %>% filter(fb),
                cells = cells),
           "web/data/mobility.json", auto_unbox = TRUE, pretty = TRUE, digits = NA)

save(m_ols, m_iv_first, m_iv, m_iv_ols, m_iv_placebo, m_iv_12, m_cohort, m_cohort_eng,
     m_ml_null, m_ml, m_ml_21, m_ml_region, m_ml_occ, m_ml_by_region, icc,
     ob_res, ob_res_eng, m_mn_all, m_mn_wage, m_mn_rent, m_bin, m_wage_move,
     m_pp, m_pp_eng, m_pp_rent, cells, cells_2024, kp_f, ar_ci,
     file = "results/models_v9.RData")

# what one unit of puma_fb_c means: SD across foreign-born workers (the model's scale)
# and across PUMAs (the map's scale). Both go in the paper.
density_scale <- bind_rows(
  ml %>% summarise(level = "workers_2022_2024", mean = mean(puma_fb_prop), sd = sd(puma_fb_prop),
                   min = min(puma_fb_prop), max = max(puma_fb_prop)),
  puma_ctx %>% filter(str_detect(puma_id, "2022-2024")) %>%
    summarise(level = "pumas_2022_2024", mean = mean(puma_fb_prop), sd = sd(puma_fb_prop),
              min = min(puma_fb_prop), max = max(puma_fb_prop)))
write_csv(density_scale, "results/t4_density_scale.csv")
print(density_scale)

cat("\nICC:", round(icc, 3), "\n")
cat("Kleibergen-Paap F:", round(kp_f, 1), " AR 95% CI:", ar_ci, "\n")
cat("IV first stage F:", fitstat(m_iv, "ivf")[[1]]$stat, "\n")
# 13. Figures (submission versions) -------------------------------------------

# Seven figures, one claim each, plus one appendix figure.
# Every figure: transparent background, Inter titles, IBM Plex Mono labels,
# base size 11, title 14 bold, axis title 10.5 medium, labels 9.
# Canvas sizes: single 7.2 x 4.6 in, grid 8.4 x 6.6 in, map strip 11 x 4.8 in.
# install.packages(c("showtext", "ggrepel", "rmapshaper", "cowplot"))
library(showtext); library(ggrepel); library(rmapshaper)

font_add_google("Inter", "inter", regular.wt = 400, bold.wt = 700)
font_add_google("Inter", "inter_medium", regular.wt = 500)
font_add_google("IBM Plex Mono", "plexmono", regular.wt = 400, bold.wt = 600)
showtext_auto(); showtext_opts(dpi = 300)

ink <- "#0d0d0d"; ink_2 <- "#595959"; ink_muted <- "#7f7f7f"
grid_col <- "#ededed"; axis_col <- "#bababa"; white <- "#ffffff"
# palette: teal, orange, gold, light brown, grey (chart color sheet)
teal <- "#00b0be"; teal_dark <- "#0d7d87"; teal_light <- "#8fd7d7"
orange <- "#ea801c"; orange_light <- "#f0b077"
gold <- "#c99b38"; brown_light <- "#eddca5"; brown_dark <- "#7a5a10"
grey_3 <- "#595959"; grey_5 <- "#a1a1a1"; grey_7 <- "#d4d4d4"
teal_ramp   <- c("#dff4f4", teal_light, teal, teal_dark, "#0a4f55")
orange_ramp <- c("#fbe4cf", orange_light, orange, "#9a4d00")
gold_ramp   <- c("#f6efd9", brown_light, gold, brown_dark)
pal_english <- setNames(teal_ramp[2:5], c("Not at all", "Not well", "Well", "Very well"))
pal_cohort4 <- c("Arrived before 1990" = grey_5, "Arrived 1990s" = gold, "Arrived 2000s" = orange, "Arrived 2010s" = teal)
LAB <- 3.1   # geom_text size for all direct labels (about 9 pt)

theme_paper <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "inter") +
    theme(plot.title = element_text(family = "inter", face = "bold", size = 14, color = ink, margin = margin(b = 8)),
          plot.title.position = "plot",
          axis.title = element_text(family = "inter_medium", size = 10.5, color = ink_2),
          axis.title.x = element_text(margin = margin(t = 8)), axis.title.y = element_text(margin = margin(r = 8)),
          axis.text = element_text(family = "plexmono", size = 9, color = ink_muted),
          axis.line.x = element_line(color = axis_col, linewidth = 0.4), axis.ticks = element_blank(),
          panel.grid.major = element_line(color = grid_col, linewidth = 0.35), panel.grid.minor = element_blank(),
          panel.spacing = unit(1.4, "lines"),
          strip.text = element_text(family = "inter_medium", size = 11, color = ink, hjust = 0, margin = margin(b = 6)),
          legend.position = "top", legend.justification = "left",
          legend.title = element_text(family = "inter_medium", size = 9.5, color = ink_2),
          legend.text = element_text(family = "plexmono", size = 9, color = ink_2),
          legend.key.size = unit(0.8, "lines"), legend.margin = margin(0, 0, 4, 0),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(12, 16, 10, 12))
}
theme_blank_axes <- function() theme(axis.text = element_blank(), axis.title = element_blank(),
                                     panel.grid = element_blank(), panel.grid.major = element_blank(),
                                     axis.line.x = element_blank())
save_fig <- function(p, file, size = c("single", "grid")) {
  size <- match.arg(size)
  d <- if (size == "single") c(7.2, 4.6) else c(8.4, 6.6)
  ggsave(file, p, width = d[1], height = d[2], units = "in", dpi = 300, bg = "white")
}

# f1. Weighted hourly wage distributions by English, foreign-born 2022-2024 -----
wd <- w %>% filter(fb, window == "2022-2024", hourly <= 80) %>%
  mutate(eng = factor(eng_score, levels = 0:3, labels = names(pal_english)))
ridges <- wd %>% group_by(eng) %>%
  group_modify(~ { d <- density(.x$hourly, weights = .x$PWGTP / sum(.x$PWGTP), from = 5, to = 80, n = 300)
  tibble(x = d$x, dens = d$y) }) %>% ungroup() %>%
  mutate(y = as.numeric(eng), height = dens / max(dens) * 0.85)
# medians on the full sample, not the <= $80 plotting window, so they match t1
meds <- w %>% filter(fb, window == "2022-2024") %>%
  mutate(eng = factor(eng_score, levels = 0:3, labels = names(pal_english))) %>%
  group_by(eng) %>% summarise(med = wmedian(hourly, PWGTP), .groups = "drop") %>%
  mutate(y = as.numeric(eng)) %>%
  rowwise() %>%
  mutate(hm = approx(ridges$x[ridges$eng == eng], ridges$height[ridges$eng == eng], xout = med)$y) %>%
  ungroup()
f1 <- ggplot() +
  geom_ribbon(data = ridges, aes(x = x, ymin = y, ymax = y + height, group = eng, fill = eng),
              color = white, linewidth = 0.4) +
  geom_segment(data = meds, aes(x = med, xend = med, y = y, yend = y + hm), color = ink, linewidth = 0.7) +
  geom_text(data = meds, aes(x = med + 1, y = y + hm + 0.08, label = scales::dollar(med, accuracy = 1)),
            family = "plexmono", size = LAB, hjust = 0, color = ink) +
  scale_fill_manual(values = pal_english, guide = "none") +
  scale_x_continuous(labels = scales::dollar, breaks = seq(10, 80, 10), expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(breaks = 1:4, labels = names(pal_english), expand = expansion(add = c(0.1, 0.15))) +
  labs(x = "Hourly wage", y = NULL) +
  theme_paper() + theme(panel.grid.major.y = element_blank())
save_fig(f1, "results/f1_wage_ridges.png")

# f2. Cohort wage gap relative to natives ----------------------------------------
b <- coef(m_cohort_eng); cohorts <- c("pre1990", "1990s", "2000s", "2010s", "2020s")
ysm_max <- w %>% filter(fb, cohort %in% cohorts) %>% group_by(cohort) %>% summarise(ysm_max = max(ysm), .groups = "drop")
traj <- expand_grid(cohort = cohorts, ysm = 0:30) %>%
  left_join(ysm_max, by = "cohort") %>% filter(ysm <= pmin(30, ysm_max)) %>%
  mutate(gap = b[paste0("cohort::", cohort)] + b["ysm"] * ysm + b["ysm2"] * ysm^2,
         pct = (exp(gap) - 1) * 100, cohort = factor(cohort, levels = cohorts),
         label = dplyr::recode(cohort, pre1990 = "Before 1990"))
lab_traj <- traj %>% group_by(cohort) %>% slice_max(ysm, n = 1)
pal_cohort <- c("pre1990" = grey_7, "1990s" = grey_5, "2000s" = teal_light, "2010s" = teal, "2020s" = teal_dark)
f2 <- ggplot(traj, aes(x = ysm, y = pct, color = cohort, group = cohort)) +
  geom_hline(yintercept = 0, color = axis_col, linewidth = 0.4) +
  geom_line(linewidth = 1) +
  geom_text_repel(data = lab_traj, aes(label = label), family = "plexmono", size = LAB, direction = "y", hjust = 0,
                  nudge_x = 0.8, min.segment.length = 0, segment.color = axis_col, segment.size = 0.3, show.legend = FALSE) +
  scale_color_manual(values = pal_cohort, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.2))) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Years since migration", y = "Wage gap to natives") +
  theme_paper()
save_fig(f2, "results/f2_cohort_trajectories.png")

# f3. Enclave effect, two panels ---------------------------------------------------
make_pred <- function(coefs, label) expand_grid(puma_fb_c = seq(-1.5, 2.5, 0.1), limited = c(FALSE, TRUE)) %>%
  mutate(log_hw = coefs["puma_fb_c"] * puma_fb_c + limited * (coefs["limited_engTRUE"] + coefs["limited_engTRUE:puma_fb_c"] * puma_fb_c),
         group = if_else(limited, "Limited English", "Proficient English"), panel = label)
pred3 <- bind_rows(make_pred(fixef(m_ml), "a  No occupation controls"),
                   make_pred(coef(m_ml_occ), "b  Occupation fixed effects")) %>%
  mutate(panel = factor(panel, levels = unique(panel)))
lab3 <- pred3 %>% filter(puma_fb_c == -1.5)
gap3 <- pred3 %>% filter(puma_fb_c == 0) %>% group_by(panel) %>% summarise(gap = diff(log_hw), .groups = "drop") %>%
  mutate(lab = sprintf("differential at mean density %.0f%%", (1 - exp(gap)) * 100))
f3 <- ggplot(pred3, aes(x = puma_fb_c, y = log_hw, color = group)) +
  geom_hline(yintercept = 0, color = axis_col, linewidth = 0.4) +
  geom_line(linewidth = 1) +
  geom_text(data = lab3, aes(label = group), family = "plexmono", size = LAB, hjust = 0, vjust = -0.6, show.legend = FALSE) +
  geom_text(data = gap3, aes(x = 2.5, y = -0.42, label = lab), inherit.aes = FALSE, family = "plexmono", size = LAB, color = ink_2, hjust = 1) +
  facet_wrap(~ panel) +
  scale_color_manual(values = c("Limited English" = orange, "Proficient English" = teal)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(x = "PUMA foreign-born percentage, standardized",
       y = "Log wage, relative to proficient at mean density") +
  theme_paper() + theme(legend.position = "none")
save_fig(f3, "results/f3_enclave_two_panels.png")

# f4. Waterfall: what explains the native to foreign-born gap ---------------------
wf <- ob_res_eng$detail %>% filter(term != "(Intercept)") %>%
  mutate(group = case_when(str_detect(term, "^AGEP|^age2") ~ "Age", term == "female" ~ "Sex",
                           str_detect(term, "^race") ~ "Race and ethnicity", str_detect(term, "^edu") ~ "Education",
                           term %in% c("married", "disabled") ~ "Family and disability",
                           term == "eng_score" ~ "English proficiency")) %>%
  group_by(group) %>% summarise(v = sum(explained), .groups = "drop") %>%
  arrange(desc(abs(v))) %>% add_row(group = "Unexplained", v = ob_res_eng$unexplained) %>%
  mutate(group = factor(group, levels = rev(group)), end = cumsum(v), start = end - v, i = row_number(),
         fill = case_when(group == "Unexplained" ~ "unexplained", v >= 0 ~ "widens", TRUE ~ "narrows"),
         pct = v / ob_res_eng$gap * 100)
f4 <- ggplot(wf) +
  geom_segment(aes(x = start, xend = end, y = group, yend = group, color = fill), linewidth = 9, lineend = "butt") +
  geom_segment(data = wf %>% filter(i < n()), aes(x = end, xend = end, y = as.numeric(group) - 0.5, yend = as.numeric(group) - 1.5),
               color = axis_col, linewidth = 0.3, linetype = "22") +
  geom_text(aes(x = pmax(start, end) + 0.002, y = group, label = sprintf("%+.0f%%", pct)), family = "plexmono", size = LAB, hjust = 0, color = ink_2) +
  geom_vline(xintercept = ob_res_eng$gap, color = ink, linewidth = 0.4) +
  annotate("text", x = ob_res_eng$gap, y = 0.4, label = "total gap", family = "plexmono", size = LAB, hjust = 1.1, color = ink) +
  scale_color_manual(values = c(widens = teal, narrows = gold, unexplained = grey_7),
                     breaks = c("widens", "narrows", "unexplained"),
                     labels = c(widens = "Widens the gap", narrows = "Narrows the gap", unexplained = "Unexplained"), name = NULL) +
  guides(color = guide_legend(override.aes = list(linewidth = 4))) +
  scale_x_continuous(labels = function(x) sprintf("%.2f", x), expand = expansion(mult = c(0.02, 0.12))) +
  scale_y_discrete(expand = expansion(add = c(0.9, 0.6))) +
  labs(x = "Contribution to the log wage gap", y = NULL) +
  theme_paper() + theme(panel.grid.major.y = element_blank(), legend.position = "top")
save_fig(f4, "results/f4_gap_waterfall.png")

# f5. Maps, three panels side by side: density, limited English, wage ------------
puma_ind <- pums %>% filter(window == "2022-2024") %>% group_by(STATE, PUMA) %>%
  summarise(puma_fb_prop = sum(PWGTP[fb]) / sum(PWGTP) * 100,
            fb_limited_prop = sum(PWGTP[fb & limited_eng %in% TRUE]) / sum(PWGTP[fb & !is.na(limited_eng)]) * 100,
            fb_median_wage = wmedian(hourly[fb & !is.na(log_hw) & AGEP >= 25 & AGEP <= 64], PWGTP[fb & !is.na(log_hw) & AGEP >= 25 & AGEP <= 64]),
            in_city = first(in_city), .groups = "drop")
pumas_sf <- map_dfr(states, function(st) pumas(state = st, year = 2023) %>%
                      filter(PUMACE20 %in% puma_list_20[[st]]) %>%
                      transmute(STATE = st, PUMA = PUMACE20, name = NAMELSAD20)) %>%
  left_join(puma_ind, by = c("STATE", "PUMA")) %>% st_transform(2272) %>% ms_simplify(keep = 0.08, keep_shapes = TRUE)
city_outline <- pumas_sf %>% filter(in_city) %>% st_union()
# the densest PUMAs, for the text
pumas_sf %>% st_drop_geometry() %>% arrange(desc(puma_fb_prop)) %>%
  select(STATE, PUMA, name, puma_fb_prop, fb_limited_prop, fb_median_wage) %>% slice_head(n = 5) %>%
  write_csv("results/t12_densest_pumas.csv")

map_panel <- function(var, title, legend, low, high, labels = waiver(), breaks = waiver()) {
  ggplot() +
    geom_sf(data = pumas_sf, aes(fill = .data[[var]]), color = white, linewidth = 0.25) +
    geom_sf(data = city_outline, fill = NA, color = ink, linewidth = 0.7) +
    scale_fill_gradient(low = low, high = high, name = legend, labels = labels, breaks = breaks) +
    guides(fill = guide_colorbar(barwidth = unit(5, "lines"), barheight = unit(0.45, "lines"))) +
    labs(title = title) +
    theme_paper() + theme_blank_axes() +
    theme(plot.title = element_text(family = "inter_medium", size = 11, margin = margin(b = 2)),
          legend.position = "bottom", legend.title.position = "top")
}
library(cowplot)
strip <- plot_grid(
  map_panel("puma_fb_prop", "a  Foreign-born percentage", "percent of residents", teal_ramp[1], teal_ramp[5]),
  map_panel("fb_limited_prop", "b  Limited English among foreign-born", "percent of foreign-born", orange_ramp[1], orange_ramp[4]),
  map_panel("fb_median_wage", "c  Foreign-born median hourly wage", "dollars", gold_ramp[1], gold_ramp[4], labels = scales::dollar, breaks = c(20, 35, 50)),
  nrow = 1)
f5 <- strip + theme(plot.background = element_rect(fill = "white", color = NA))
ggsave("results/f5_puma_maps.png", f5, width = 11, height = 4.6, units = "in", dpi = 300, bg = "white")

# f6. Flow diagram: abroad, city, suburbs, with movers' median wage ---------------
mv <- pums %>% filter(fb, window == "2022-2024", AGEP >= 25, AGEP <= 64,
                      move_type %in% c("within_city", "within_suburbs", "city_to_suburb", "suburb_to_city", "from_abroad")) %>%
  mutate(flow = case_when(move_type == "from_abroad" & in_city ~ "abroad_to_city",
                          move_type == "from_abroad" ~ "abroad_to_suburb", TRUE ~ as.character(move_type))) %>%
  group_by(flow) %>%
  summarise(n = sum(PWGTP), med = wmedian(hourly[!is.na(log_hw)], PWGTP[!is.na(log_hw)]), .groups = "drop")
arrows_geom <- tribble(
  ~flow,              ~x,  ~y,   ~xend, ~yend, ~curv, ~lx,  ~ly,
  "abroad_to_city",   0.3, 1.4,  0.95,  0.28, -0.25, 0.25, 0.8,
  "abroad_to_suburb", 0.5, 1.5,  2.85,  0.28,  0.25, 2.25, 1.2,
  "city_to_suburb",   1.25, 0.12, 2.75, 0.12, -0.35, 2.0,  0.35,
  "suburb_to_city",   2.75, -0.12, 1.25, -0.12, -0.35, 2.0, -0.62)
arr <- mv %>% select(flow, n, med) %>% inner_join(arrows_geom, by = "flow") %>%
  mutate(label = sprintf("%s\n%s", scales::comma(round(n, -2)), scales::dollar(med, accuracy = 1)))
within <- mv %>% filter(flow %in% c("within_city", "within_suburbs")) %>%
  mutate(x = if_else(flow == "within_city", 1, 3), y = -0.38,
         label = sprintf("moved within\n%s, %s", scales::comma(round(n, -2)), scales::dollar(med, accuracy = 1)))
arrow_layers <- lapply(seq_len(nrow(arr)), function(i)
  geom_curve(data = arr[i, ], aes(x = x, y = y, xend = xend, yend = yend, linewidth = n, color = med),
             curvature = arr$curv[i], arrow = arrow(length = unit(0.22, "cm"), type = "closed"), lineend = "round"))
f6 <- ggplot() + arrow_layers +
  annotate("text", x = c(1, 3, 0.2), y = c(0, 0, 1.62), label = c("Philadelphia", "Suburbs", "Abroad"),
           family = "inter", fontface = "bold", size = 4.2, color = ink) +
  geom_text(data = arr, aes(x = lx, y = ly, label = label), family = "plexmono", size = LAB, color = ink_2, lineheight = 0.95) +
  geom_text(data = within, aes(x = x, y = y, label = label), family = "plexmono", size = LAB, color = ink_muted) +
  scale_linewidth(range = c(0.8, 5), guide = "none") +
  scale_color_gradient(low = teal_light, high = teal_ramp[5], name = "Movers' median hourly wage",
                       labels = scales::dollar, breaks = scales::breaks_pretty(3)) +
  guides(color = guide_colorbar(barwidth = unit(6, "lines"), barheight = unit(0.5, "lines"), title.position = "top")) +
  coord_equal(xlim = c(-0.1, 3.6), ylim = c(-0.8, 1.75), expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_paper() + theme_blank_axes()
save_fig(f6, "results/f6_move_flows.png")

# f7. Pseudo-panel trajectories, one panel per region, cohorts as a teal ramp ----
cells_f <- cells_2024 %>%
  mutate(cohort = factor(cohort, levels = c("pre1990", "1990s", "2000s", "2010s"),
                         labels = names(pal_cohort4)),
         region = factor(region, levels = c("Asia", "Latin America", "Europe", "Africa")),
         window = factor(window))
lab_cells <- cells_f %>% filter(window == "2022-2024") %>%
  mutate(lab = str_remove(as.character(cohort), "Arrived "))
f7 <- ggplot(cells_f, aes(x = median_hourly, y = suburb_prop, group = cohort, color = cohort)) +
  geom_path(linewidth = 0.9, lineend = "round", arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
  geom_point(aes(shape = window), size = 2.4, fill = white, stroke = 0.9) +
  geom_text_repel(data = lab_cells, aes(label = lab), family = "plexmono", size = LAB, show.legend = FALSE,
                  min.segment.length = 0, segment.color = axis_col, segment.size = 0.3, box.padding = 0.9, point.padding = 0.5, force = 3, force_pull = 0.5,
                  max.overlaps = Inf, nudge_x = 2, direction = "both", hjust = 0, seed = 7) +
  facet_wrap(~ region, ncol = 2) +
  scale_color_manual(values = pal_cohort4, name = NULL) +
  scale_shape_manual(values = c(21, 24, 22), labels = c("2012\u20132016", "2017\u20132021", "2022\u20132024"), name = NULL) +
  scale_x_continuous(labels = scales::dollar, expand = expansion(mult = c(0.05, 0.3))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(50, 90)) +
  guides(color = guide_legend(order = 1, nrow = 1, override.aes = list(shape = NA)),
         shape = guide_legend(order = 2, nrow = 1)) +
  labs(x = "Median hourly wage, 2024 dollars", y = "Living outside Philadelphia") +
  theme_paper() + theme(legend.box = "vertical", legend.spacing.y = unit(0, "lines"))
save_fig(f7, "results/f7_pseudo_panel_trajectories.png", "grid")

# fA1. Move rates, appendix ---------------------------------------------------------
mr <- move_rates %>% filter(window == "2022-2024", move_type != "stayed") %>%
  mutate(group = if_else(fb, "Foreign-born", "Native-born"),
         move_type = str_replace_all(as.character(move_type), "_", " ") %>% str_to_sentence(),
         move_type = fct_reorder(move_type, pct, .fun = max))
fA1 <- ggplot(mr, aes(x = pct, y = move_type, fill = group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), position = position_dodge(width = 0.7), hjust = -0.15, family = "plexmono", size = LAB, color = ink_2) +
  scale_fill_manual(values = c("Foreign-born" = teal, "Native-born" = grey_5), name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Percentage of population", y = NULL) +
  theme_paper() + theme(panel.grid.major.y = element_blank())
save_fig(fA1, "results/fA1_move_rates.png")

# 14. New York MSA comparison of the two-level model -------------------------
# Same sample rules, variables and specification as section 8, 2022-2024 window only.
# Uses its own objects (nyc_*) so nothing above is touched.
dir.create("cache_nyc", showWarnings = FALSE)
nyc_states <- c("NY", "NJ", "PA")

# 14.1 PUMAs inside the New York MSA (2020 vintage) and the five boroughs ---------
sf_use_s2(FALSE)
nyc_msa <- core_based_statistical_areas(year = 2023, cb = TRUE) %>%
  filter(str_detect(NAME, "^New York-Newark")) %>% st_transform(4326)
print(nyc_msa$NAME)

nyc_pumas_all <- map_dfr(nyc_states, function(st)
  pumas(state = st, year = 2023, cb = FALSE) %>% st_transform(4326) %>% mutate(STATE = st))
nyc_inside <- st_within(st_point_on_surface(nyc_pumas_all), nyc_msa, sparse = FALSE)[, 1]
nyc_pumas_msa <- nyc_pumas_all[nyc_inside, ]
nyc_puma_list_20 <- split(nyc_pumas_msa$PUMACE20, nyc_pumas_msa$STATE)
print(map_int(nyc_puma_list_20, length))

nyc_boroughs <- counties(state = "NY", year = 2023, cb = TRUE) %>%
  filter(NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
  st_transform(4326) %>% st_union()
nyc_city_puma20 <- nyc_pumas_msa %>% filter(STATE == "NY") %>%
  filter(st_within(st_point_on_surface(.), nyc_boroughs, sparse = FALSE)[, 1]) %>% pull(PUMACE20)
print(length(nyc_city_puma20))   # expect ~ 60 to 70
sf_use_s2(TRUE)

# 14.2 Pull 2022-2024 (person weight only; replicate weights not needed here) ------
nyc_base_vars <- c("AGEP", "SEX", "RAC1P", "HISP", "NATIVITY", "SCHL", "ESR", "POBP", "WAOB",
                   "WAGP", "ADJINC", "ENG", "COW", "OCCP", "WKHP", "WKWN", "YOEP", "MAR", "DIS", "PUMA")
nyc_f <- "cache_nyc/pums_2024.rds"
if (file.exists(nyc_f)) {
  nyc_p24 <- readRDS(nyc_f)
} else {
  nyc_p24 <- map_dfr(nyc_states, function(st) {
    if (is.null(nyc_puma_list_20[[st]])) return(NULL)
    get_pums(variables = nyc_base_vars, state = st, puma = nyc_puma_list_20[[st]],
             survey = "acs5", year = 2024) %>%
      mutate(across(everything(), as.character), STATE = st)
  })
  saveRDS(nyc_p24, nyc_f)
}
nyc_p24 <- nyc_p24 %>% mutate(window = "2022-2024")

# 14.3 Harmonize: identical to the Philadelphia script, minus the migration block ----
nyc_pums <- nyc_p24 %>%
  mutate(across(everything(), as.character)) %>%
  mutate(PUMA = str_pad(PUMA, 5, pad = "0"), HISP = str_pad(HISP, 2, pad = "0"),
         OCCP = str_pad(OCCP, 4, pad = "0"), POBP = str_pad(POBP, 3, pad = "0")) %>%
  mutate(across(c(AGEP, WAGP, ADJINC, WKHP, YOEP, PWGTP), as.numeric),
         weeks = as.numeric(WKWN)) %>%
  mutate(
    int_year   = as.integer(substr(SERIALNO, 1, 4)),
    fb         = NATIVITY == "2",
    eng_score  = case_when(ENG == "1" ~ 3, ENG == "2" ~ 2, ENG == "3" ~ 1, ENG == "4" ~ 0,
                           ENG %in% c("b", "0") & AGEP >= 5 ~ 3, TRUE ~ NA_real_),
    limited_eng = eng_score <= 1,
    female     = SEX == "2",
    hispanic   = HISP != "01",
    race       = case_when(hispanic ~ "Hispanic", RAC1P == "1" ~ "White", RAC1P == "2" ~ "Black",
                           RAC1P == "6" ~ "Asian", TRUE ~ "Other"),
    schl_num   = suppressWarnings(as.numeric(SCHL)),
    edu        = case_when(schl_num <= 15 ~ "lt_hs", schl_num <= 17 ~ "hs", schl_num <= 20 ~ "some_college",
                           schl_num == 21 ~ "ba", schl_num >= 22 ~ "grad", TRUE ~ NA_character_),
    married    = MAR == "1",
    disabled   = DIS == "1",
    employed   = ESR %in% c("1", "2"),
    wage_real  = WAGP * ADJINC,
    hourly     = if_else(WKHP > 0 & weeks > 0, wage_real / (WKHP * weeks), NA_real_),
    log_hw     = if_else(hourly >= 2 & hourly <= 500, log(hourly), NA_real_),
    ysm        = if_else(fb, int_year - YOEP, 0),
    region     = case_when(WAOB == "3" ~ "Latin America", WAOB == "4" ~ "Asia",
                           WAOB == "5" ~ "Europe", WAOB == "6" ~ "Africa", fb ~ "Other", TRUE ~ "Native"),
    in_city    = STATE == "NY" & PUMA %in% nyc_city_puma20,
    puma_id    = paste(STATE, PUMA, window, sep = "_")) %>%
  filter(int_year >= 2022)

print(nyc_pums %>% count(in_city))

nyc_puma_ctx <- nyc_pums %>% group_by(puma_id) %>%
  summarise(puma_fb_prop = sum(PWGTP[fb]) / sum(PWGTP) * 100, .groups = "drop")
nyc_puma_same_region <- nyc_pums %>% filter(fb) %>%
  count(puma_id, WAOB, wt = PWGTP, name = "n_region") %>%
  group_by(puma_id) %>% mutate(same_region_prop = n_region / sum(n_region) * 100) %>%
  ungroup() %>% select(puma_id, WAOB, same_region_prop)
nyc_pums <- nyc_pums %>% left_join(nyc_puma_ctx, by = "puma_id") %>%
  left_join(nyc_puma_same_region, by = c("puma_id", "WAOB"))

nyc_w <- nyc_pums %>%
  filter(AGEP >= 25, AGEP <= 64, employed, COW %in% c("1", "2", "3", "4", "5"),
         !is.na(log_hw), !is.na(eng_score), !is.na(edu), ysm >= 0) %>%
  mutate(age2 = AGEP^2, ysm2 = ysm^2)

# density at the PUMA level, for the text
print(nyc_puma_ctx %>% summarise(mean = mean(puma_fb_prop), sd = sd(puma_fb_prop),
                                 min = min(puma_fb_prop), max = max(puma_fb_prop)))

# 14.4 Two-level model, same code as section 8 -------------------------------------
nyc_ml <- nyc_w %>% filter(fb) %>%
  mutate(wt = PWGTP / mean(PWGTP),
         puma_fb_c = (puma_fb_prop - mean(puma_fb_prop)) / sd(puma_fb_prop),
         same_region_c = (same_region_prop - mean(same_region_prop, na.rm = TRUE)) /
           sd(same_region_prop, na.rm = TRUE))
cat("NYC worker-level SD of puma_fb_prop:", sd(nyc_ml$puma_fb_prop), "\n")
cat("n workers:", nrow(nyc_ml), " PUMAs:", n_distinct(nyc_ml$puma_id), "\n")

nyc_m_null <- lmer(log_hw ~ 1 + (1 | puma_id), data = nyc_ml, weights = wt, REML = FALSE)
icc_nyc <- as.numeric(VarCorr(nyc_m_null)$puma_id[1]) /
  (as.numeric(VarCorr(nyc_m_null)$puma_id[1]) + sigma(nyc_m_null)^2)

nyc_ml_formula <- log_hw ~ limited_eng * puma_fb_c + AGEP + age2 + female + race + edu +
  married + disabled + ysm + ysm2 + (1 + limited_eng | puma_id)
m_ml_nyc <- lmer(nyc_ml_formula, data = nyc_ml, weights = wt, REML = FALSE)
m_ml_region_nyc <- lmer(log_hw ~ limited_eng * same_region_c + puma_fb_c + AGEP + age2 + female
                        + race + edu + married + disabled + ysm + ysm2 + (1 + limited_eng | puma_id),
                        data = nyc_ml, weights = wt, REML = FALSE)
m_ml_occ_nyc <- feols(log_hw ~ limited_eng * puma_fb_c + AGEP + age2 + female + race + edu
                      + married + disabled + ysm + ysm2 | OCCP,
                      data = nyc_ml, weights = ~PWGTP, cluster = ~puma_id)

# 14.5 Output -----------------------------------------------------------------------
out <- bind_rows(
  tidy(m_ml_nyc, effects = "fixed")        %>% mutate(model = "nyc_fb_prop_2022_2024"),
  tidy(m_ml_region_nyc, effects = "fixed") %>% mutate(model = "nyc_same_region_2022_2024"),
  tidy(m_ml_occ_nyc)                       %>% mutate(model = "nyc_occ_fe_2022_2024"))
write_csv(out, "results/nyc_t4_multilevel.csv")
cat("ICC NYC:", round(icc_nyc, 3), "\n")
print(out %>% filter(str_detect(term, "limited_eng|puma_fb_c|same_region_c")) %>%
        select(model, term, estimate, std.error))

# 15. Cohort contrasts, duration-matched CIs, own-language density, replicate-weight multinomial ----
library(tidyverse); library(fixest); library(srvyr); library(lme4)
library(broom); library(broom.mixed); library(tidycensus)

# 15.1 Cohort contrasts ---------------------------------------------------------
contrast <- function(m, L) {
  b <- coef(m); V <- vcov(m)
  stopifnot(all(names(L) %in% names(b)))
  w <- setNames(rep(0, length(b)), names(b)); w[names(L)] <- L
  est <- sum(w * b); se <- sqrt(as.numeric(t(w) %*% V %*% w))
  c(estimate = est, se = se, z = est / se, p = 2 * pnorm(-abs(est / se)))
}
cohort_tests <- bind_rows(
  contrast(m_cohort_eng, c("cohort::pre1990" = 0.5, "cohort::1990s" = 0.5,
                           "cohort::2000s" = -0.5, "cohort::2010s" = -0.5)) %>%
    t() %>% as_tibble() %>% mutate(test = "pre2000 mean minus post2000 mean"),
  contrast(m_cohort_eng, c("cohort::1990s" = 1, "cohort::2000s" = -1)) %>%
    t() %>% as_tibble() %>% mutate(test = "1990s minus 2000s"))
write_csv(cohort_tests, "results/t2_cohort_contrasts.csv")
print(cohort_tests)

# 15.2 Duration-matched cells with replicate-weight CIs ---------------------------
dm_ci <- pums %>%
  filter(fb, AGEP >= 25, AGEP <= 64, ysm >= 0,
         cohort %in% c("1990s", "2000s", "2010s"),
         region %in% c("Latin America", "Asia", "Europe", "Africa")) %>%
  to_survey(type = "person", design = "rep_weights") %>%
  filter((cohort == "2000s" & window == "2012-2016") | (cohort == "2010s" & window == "2022-2024") |
           (cohort == "1990s" & window == "2012-2016") | (cohort == "2000s" & window == "2022-2024")) %>%
  group_by(region, cohort, window) %>%
  summarise(suburb = survey_mean(!in_city, vartype = "ci"), n = unweighted(n())) %>%
  mutate(across(starts_with("suburb"), ~ . * 100))
write_csv(dm_ci, "results/t11_duration_matched_ci.csv")
print(dm_ci)

# 15.3 Own-language density, Philadelphia 2022-2024 ------------------------------
# LANP is pulled separately and joined on SERIALNO + SPORDER + STATE.
# Own-language density = percentage of all PUMA residents aged 5+ who speak the
# respondent's home language at home. Computed for workers who speak a language
# other than English at home.
lanp24 <- map_dfr(states, function(st)
  get_pums(variables = "LANP", state = st, puma = puma_list_20[[st]],
           survey = "acs5", year = 2024) %>%
    mutate(across(everything(), as.character), STATE = st) %>%
    select(SERIALNO, SPORDER, STATE, LANP))

pums24 <- pums %>% filter(window == "2022-2024") %>%
  left_join(lanp24, by = c("SERIALNO", "SPORDER", "STATE")) %>%
  mutate(LANP = if_else(LANP %in% c("b", "bbbb", "0000", "N"), "english_only", LANP))

lang_ctx <- pums24 %>% filter(AGEP >= 5, !is.na(LANP)) %>%
  count(puma_id, LANP, wt = PWGTP, name = "n_lang") %>%
  group_by(puma_id) %>% mutate(lang_prop = n_lang / sum(n_lang) * 100) %>% ungroup() %>%
  select(puma_id, LANP, lang_prop)

ml_lang <- pums24 %>%
  filter(AGEP >= 25, AGEP <= 64, employed, COW %in% c("1", "2", "3", "4", "5"),
         !is.na(log_hw), !is.na(eng_score), !is.na(edu), ysm >= 0, fb,
         !is.na(LANP), LANP != "english_only") %>%
  left_join(lang_ctx, by = c("puma_id", "LANP")) %>%
  mutate(age2 = AGEP^2, ysm2 = ysm^2, wt = PWGTP / mean(PWGTP),
         puma_fb_c = (puma_fb_prop - mean(puma_fb_prop)) / sd(puma_fb_prop),
         lang_c = (lang_prop - mean(lang_prop)) / sd(lang_prop))

cat("Own-language sample: N =", nrow(ml_lang), " PUMAs =", n_distinct(ml_lang$puma_id),
    " SD of own-language density (pp) =", round(sd(ml_lang$lang_prop), 2), "\n")

# 3a. own-language density with generic density as a control
m_ml_lang <- lmer(log_hw ~ limited_eng * lang_c + puma_fb_c + AGEP + age2 + female + race + edu
                  + married + disabled + ysm + ysm2 + (1 + limited_eng | puma_id),
                  data = ml_lang, weights = wt, REML = FALSE)
# 3b. same with occupation fixed effects
m_ml_lang_occ <- feols(log_hw ~ limited_eng * lang_c + puma_fb_c + AGEP + age2 + female + race + edu
                       + married + disabled + ysm + ysm2 | OCCP,
                       data = ml_lang, weights = ~PWGTP, cluster = ~puma_id)
# 3c. generic density only, on the SAME subsample (so col. 1 and col. 4 are comparable)
m_ml_sub <- lmer(log_hw ~ limited_eng * puma_fb_c + AGEP + age2 + female + race + edu
                 + married + disabled + ysm + ysm2 + (1 + limited_eng | puma_id),
                 data = ml_lang, weights = wt, REML = FALSE)

lang_out <- bind_rows(
  tidy(m_ml_lang, effects = "fixed")     %>% mutate(model = "lang_density_2022_2024"),
  tidy(m_ml_lang_occ)                    %>% mutate(model = "lang_density_occ_fe_2022_2024"),
  tidy(m_ml_sub, effects = "fixed")      %>% mutate(model = "generic_density_lang_subsample")) %>%
  mutate(n = nrow(ml_lang))
write_csv(lang_out, "results/t4_language_density.csv")
print(lang_out %>% filter(str_detect(term, "limited_eng|lang_c|puma_fb_c")) %>%
        select(model, term, estimate, std.error))

# slopes of own-language density for each English group, from the same model
b_l <- fixef(m_ml_lang); V_l <- as.matrix(vcov(m_ml_lang))
slope_prof <- b_l["lang_c"]
slope_lim  <- b_l["lang_c"] + b_l["limited_engTRUE:lang_c"]
se_lim <- sqrt(V_l["lang_c", "lang_c"] + V_l["limited_engTRUE:lang_c", "limited_engTRUE:lang_c"]
               + 2 * V_l["lang_c", "limited_engTRUE:lang_c"])
lang_slopes <- tibble(group = c("proficient", "limited"),
                      slope = c(slope_prof, slope_lim),
                      se = c(sqrt(V_l["lang_c", "lang_c"]), se_lim))
write_csv(lang_slopes, "results/t4_language_slopes.csv")
print(lang_slopes)

# 15.4 Multinomial logit with replicate-weight standard errors -------------------
# install.packages("svyVGAM") once.
library(svyVGAM)
des_mn <- mn_workers %>% to_survey(type = "person", design = "rep_weights")
m_mn_svy <- svy_vglm(move_type ~ log_hw + eng_score + edu + ysm + ysm2 + cohort + region + AGEP + age2
                     + female + married + window,
                     family = multinomial(refLevel = "stayed"), design = des_mn)
# outcome index: 1 within_city, 2 within_suburbs, 3 city_to_suburb, 4 suburb_to_city
mn_svy_out <- summary(m_mn_svy)$coeftable %>% as.data.frame() %>% rownames_to_column("term")
write_csv(mn_svy_out, "results/t7_multinomial_moves_repweights.csv")
print(mn_svy_out %>% filter(str_detect(term, "log_hw")))

# 15.5 Own-language density, New York 2022-2024 ----------------------------------
# Same construction as section 3 on the v9 section-14 objects.
nyc_states_present <- names(nyc_puma_list_20)
lanp_nyc <- map_dfr(nyc_states_present, function(st)
  get_pums(variables = "LANP", state = st, puma = nyc_puma_list_20[[st]],
           survey = "acs5", year = 2024) %>%
    mutate(across(everything(), as.character), STATE = st) %>%
    select(SERIALNO, SPORDER, STATE, LANP))

nyc24 <- nyc_pums %>%
  left_join(lanp_nyc, by = c("SERIALNO", "SPORDER", "STATE")) %>%
  mutate(LANP = if_else(LANP %in% c("b", "bbbb", "0000", "N"), "english_only", LANP))

nyc_lang_ctx <- nyc24 %>% filter(AGEP >= 5, !is.na(LANP)) %>%
  count(puma_id, LANP, wt = PWGTP, name = "n_lang") %>%
  group_by(puma_id) %>% mutate(lang_prop = n_lang / sum(n_lang) * 100) %>% ungroup() %>%
  select(puma_id, LANP, lang_prop)

nyc_ml_lang <- nyc24 %>%
  filter(AGEP >= 25, AGEP <= 64, employed, COW %in% c("1", "2", "3", "4", "5"),
         !is.na(log_hw), !is.na(eng_score), !is.na(edu), ysm >= 0, fb,
         !is.na(LANP), LANP != "english_only") %>%
  left_join(nyc_lang_ctx, by = c("puma_id", "LANP")) %>%
  mutate(age2 = AGEP^2, ysm2 = ysm^2, wt = PWGTP / mean(PWGTP),
         puma_fb_c = (puma_fb_prop - mean(puma_fb_prop)) / sd(puma_fb_prop),
         lang_c = (lang_prop - mean(lang_prop)) / sd(lang_prop))

cat("NYC own-language sample: N =", nrow(nyc_ml_lang), " PUMAs =", n_distinct(nyc_ml_lang$puma_id),
    " SD of own-language density (pp) =", round(sd(nyc_ml_lang$lang_prop), 2), "\n")

nyc_m_lang <- lmer(log_hw ~ limited_eng * lang_c + puma_fb_c + AGEP + age2 + female + race + edu
                   + married + disabled + ysm + ysm2 + (1 + limited_eng | puma_id),
                   data = nyc_ml_lang, weights = wt, REML = FALSE)
nyc_m_lang_occ <- feols(log_hw ~ limited_eng * lang_c + puma_fb_c + AGEP + age2 + female + race + edu
                        + married + disabled + ysm + ysm2 | OCCP,
                        data = nyc_ml_lang, weights = ~PWGTP, cluster = ~puma_id)
nyc_m_sub <- lmer(log_hw ~ limited_eng * puma_fb_c + AGEP + age2 + female + race + edu
                  + married + disabled + ysm + ysm2 + (1 + limited_eng | puma_id),
                  data = nyc_ml_lang, weights = wt, REML = FALSE)

nyc_lang_out <- bind_rows(
  tidy(nyc_m_lang, effects = "fixed") %>% mutate(model = "nyc_lang_density_2022_2024"),
  tidy(nyc_m_lang_occ)                %>% mutate(model = "nyc_lang_density_occ_fe_2022_2024"),
  tidy(nyc_m_sub, effects = "fixed")  %>% mutate(model = "nyc_generic_density_lang_subsample")) %>%
  mutate(n = nrow(nyc_ml_lang))
write_csv(nyc_lang_out, "results/nyc_t4_language_density.csv")
print(nyc_lang_out %>% filter(str_detect(term, "limited_eng|lang_c|puma_fb_c")) %>%
        select(model, term, estimate, std.error))

save(cohort_tests, dm_ci, ml_lang, m_ml_lang, m_ml_lang_occ, m_ml_sub, lang_slopes,
     m_mn_svy, nyc_ml_lang, nyc_m_lang, nyc_m_lang_occ, nyc_m_sub,
     file = "results/models_revision3.RData")

# 16. Group slopes, Satterthwaite p-values, random-slope variances -------------------
library(tidyverse); library(lme4); library(lmerTest)

models <- list(
  phl_generic_2022      = list(m = m_ml,      d = "puma_fb_c"),
  phl_generic_2017      = list(m = m_ml_21,   d = "puma_fb_c"),
  phl_generic_subsample = list(m = m_ml_sub,  d = "puma_fb_c"),
  phl_language_2022     = list(m = m_ml_lang, d = "lang_c"),
  nyc_generic_2022      = list(m = m_ml_nyc,  d = "puma_fb_c"),
  nyc_generic_subsample = list(m = nyc_m_sub, d = "puma_fb_c"),
  nyc_language_2022     = list(m = nyc_m_lang, d = "lang_c"))

# 16.1 density slope for proficient (L = 0) and limited-English (L = 1) workers, with SE
group_slopes <- imap_dfr(models, function(x, name) {
  b <- fixef(x$m); V <- as.matrix(vcov(x$m))
  d <- x$d; i <- paste0("limited_engTRUE:", d)
  tibble(model = name,
         group = c("proficient", "limited"),
         slope = c(b[d], b[d] + b[i]),
         se = c(sqrt(V[d, d]), sqrt(V[d, d] + V[i, i] + 2 * V[d, i]))) %>%
    mutate(z = slope / se, p = 2 * pnorm(-abs(z)))
})
write_csv(group_slopes, "results/t4_group_slopes.csv")
print(group_slopes)

# 16.2 Satterthwaite degrees of freedom and p-values for the fixed effects of interest
satt <- imap_dfr(models, function(x, name) {
  mt <- as_lmerModLmerTest(x$m)
  co <- summary(mt)$coefficients
  as.data.frame(co) %>% rownames_to_column("term") %>%
    filter(str_detect(term, "limited_eng|puma_fb_c|lang_c")) %>%
    mutate(model = name)
})
names(satt) <- c("term", "estimate", "se", "df", "t", "p", "model")
write_csv(satt, "results/t4_satterthwaite.csv")
print(satt %>% select(model, term, estimate, se, df, p))

# 16.3 random-effect variances and singular-fit check
rand <- imap_dfr(models, function(x, name) {
  vc <- as.data.frame(VarCorr(x$m))
  tibble(model = name,
         var_intercept = vc$vcov[vc$grp == "puma_id" & vc$var1 == "(Intercept)" & is.na(vc$var2)],
         var_slope     = vc$vcov[vc$grp == "puma_id" & vc$var1 == "limited_engTRUE" & is.na(vc$var2)],
         corr          = vc$sdcor[vc$grp == "puma_id" & !is.na(vc$var2)],
         var_residual  = vc$vcov[vc$grp == "Residual"],
         singular      = isSingular(x$m),
         n_groups      = ngrps(x$m)[["puma_id"]])
})
write_csv(rand, "results/t4_random_slopes.csv")
print(rand)

# 17. Migration-area selection test ---------------------------------------------------
library(tidyverse); library(fixest)

state_fips <- c(PA = 42, NJ = 34, DE = 10, MD = 24)

# 17.1 PUMA -> MIGPUMA map, modal MIGPUMA among in-state movers now living in each PUMA
puma_mig <- pums24 %>%
  filter(MIG == "3", migsp_num == state_fips[STATE], !is.na(MIGPUMA), MIGPUMA != "bbbbb") %>%
  count(STATE, PUMA, MIGPUMA, wt = PWGTP, name = "n") %>%
  group_by(STATE, PUMA) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
  transmute(STATE, PUMA, mig_id = paste(state_fips[STATE], MIGPUMA))
print(nrow(puma_mig))                       # should equal the number of PUMAs, 48
print(n_distinct(puma_mig$mig_id))          # number of migration areas

# 17.2 own-language density by migration area (residents aged 5+)
mig_lang <- pums24 %>%
  filter(AGEP >= 5, !is.na(LANP)) %>%
  left_join(puma_mig, by = c("STATE", "PUMA")) %>%
  count(mig_id, LANP, wt = PWGTP, name = "n_lang") %>%
  group_by(mig_id) %>% mutate(origin_lang_prop = n_lang / sum(n_lang) * 100) %>% ungroup() %>%
  select(mig_id, LANP, origin_lang_prop)

# 17.3 sample: foreign-born workers with a non-English home language who were in the MSA a year ago
sel <- pums24 %>%
  filter(AGEP >= 25, AGEP <= 64, employed, COW %in% c("1", "2", "3", "4", "5"),
         !is.na(log_hw), !is.na(eng_score), !is.na(edu), ysm >= 0, fb,
         !is.na(LANP), LANP != "english_only",
         MIG == "1" | (MIG == "3" & migsp_num %in% msa_state_codes & !is.na(MIGPUMA) & MIGPUMA != "bbbbb")) %>%
  left_join(puma_mig, by = c("STATE", "PUMA")) %>%
  mutate(origin_id = if_else(MIG == "3", paste(migsp_num, MIGPUMA), mig_id),
         left_origin = MIG == "3" & origin_id != mig_id) %>%
  left_join(mig_lang, by = c("origin_id" = "mig_id", "LANP")) %>%
  filter(!is.na(origin_lang_prop)) %>%
  mutate(origin_lang_c = (origin_lang_prop - mean(origin_lang_prop)) / sd(origin_lang_prop),
         age2 = AGEP^2, ysm2 = ysm^2)
print(sel %>% count(limited_eng, left_origin))
cat("SD of origin own-language density (pp):", sd(sel$origin_lang_prop), "\n")

# 17.4 wage of leavers vs stayers by origin density, separately by English group
m_sel_prof <- feols(log_hw ~ left_origin * origin_lang_c + AGEP + age2 + female + race + edu
                    + married + disabled + ysm + ysm2 | int_year,
                    data = sel %>% filter(!limited_eng), weights = ~PWGTP, cluster = ~origin_id)
m_sel_lim  <- feols(log_hw ~ left_origin * origin_lang_c + AGEP + age2 + female + race + edu
                    + married + disabled + ysm + ysm2 | int_year,
                    data = sel %>% filter(limited_eng), weights = ~PWGTP, cluster = ~origin_id)
# same, with occupation fixed effects
m_sel_prof_occ <- feols(log_hw ~ left_origin * origin_lang_c + AGEP + age2 + female + race + edu
                        + married + disabled + ysm + ysm2 | int_year + OCCP,
                        data = sel %>% filter(!limited_eng), weights = ~PWGTP, cluster = ~origin_id)
m_sel_lim_occ  <- feols(log_hw ~ left_origin * origin_lang_c + AGEP + age2 + female + race + edu
                        + married + disabled + ysm + ysm2 | int_year + OCCP,
                        data = sel %>% filter(limited_eng), weights = ~PWGTP, cluster = ~origin_id)

sel_out <- bind_rows(
  tidy(m_sel_prof)     %>% mutate(model = "proficient"),
  tidy(m_sel_lim)      %>% mutate(model = "limited"),
  tidy(m_sel_prof_occ) %>% mutate(model = "proficient_occ_fe"),
  tidy(m_sel_lim_occ)  %>% mutate(model = "limited_occ_fe")) %>%
  filter(str_detect(term, "left_origin|origin_lang_c")) %>%
  mutate(n_prof = sum(!sel$limited_eng), n_lim = sum(sel$limited_eng),
         n_left_prof = sum(sel$left_origin & !sel$limited_eng),
         n_left_lim = sum(sel$left_origin & sel$limited_eng))
write_csv(sel_out, "results/t13_selection_test.csv")
print(sel_out %>% select(model, term, estimate, std.error, p.value))

# 18. Native-born placebo and native-wage control -------------------------------------
library(tidyverse); library(lme4); library(fixest); library(broom); library(broom.mixed)

# A. natives ------------------------------------------------------------------
nat <- w %>% filter(!fb, window == "2022-2024") %>%
  mutate(wt = PWGTP / mean(PWGTP),
         puma_fb_c = (puma_fb_prop - mean(ml$puma_fb_prop)) / sd(ml$puma_fb_prop))  # same scale as ml
cat("natives:", nrow(nat), " PUMAs:", n_distinct(nat$puma_id), "\n")

m_nat <- lmer(log_hw ~ puma_fb_c + AGEP + age2 + female + race + edu + married + disabled
              + (1 | puma_id), data = nat, weights = wt, REML = FALSE)
m_nat_occ <- feols(log_hw ~ puma_fb_c + AGEP + age2 + female + race + edu + married + disabled | OCCP,
                   data = nat, weights = ~PWGTP, cluster = ~puma_id)

# B. foreign-born model with native median log wage of the PUMA as a control -----
nat_wage <- nat %>% group_by(puma_id) %>%
  summarise(native_med_lw = log(wmedian(hourly, PWGTP)), .groups = "drop")
ml_nw <- ml %>% left_join(nat_wage, by = "puma_id") %>%
  mutate(native_lw_c = (native_med_lw - mean(native_med_lw)) / sd(native_med_lw))
cat("SD of PUMA native median log wage:", sd(nat_wage$native_med_lw), "\n")

m_ml_nw <- lmer(log_hw ~ limited_eng * puma_fb_c + native_lw_c + AGEP + age2 + female + race + edu
                + married + disabled + ysm + ysm2 + (1 + limited_eng | puma_id),
                data = ml_nw, weights = wt, REML = FALSE)

out <- bind_rows(
  tidy(m_nat, effects = "fixed") %>% mutate(model = "natives_two_level"),
  tidy(m_nat_occ)                %>% mutate(model = "natives_occ_fe"),
  tidy(m_ml_nw, effects = "fixed") %>% mutate(model = "foreign_born_with_native_wage")) %>%
  filter(str_detect(term, "puma_fb_c|limited_eng|native_lw_c")) %>%
  mutate(n_natives = nrow(nat), singular_nat = isSingular(m_nat), singular_fb = isSingular(m_ml_nw))
write_csv(out, "results/t14_native_placebo.csv")
print(out %>% select(model, term, estimate, std.error))