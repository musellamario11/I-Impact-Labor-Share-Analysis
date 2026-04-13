03_econometric_models.R
# Empirical Verification
# 0. SETUP
rm(list = ls())
# Load packages
library(tidyverse)
library(data.table)
library(fixest)
library(broom)
library(ggplot2)
library(stringr)
library(purrr)
# 1. LOAD DATA
panel_raw <- read_csv("dataset_tesi_finale_completo.csv")
glimpse(panel_raw)
# 2. RENAME & CONSTRUCT KEY VARIABLES
panel <- panel_raw %>% rename(year = YEAR, sector_id = IND1990, w_h = wage_H, w_l = wage_L, ai_felten = exposure_ai, fo_risk = risk_auto, rti = risk_routine) %>% mutate(wage_gap = w_h - w_l, post_year_threshold = 2020, post_period = if_else(year >= post_year_threshold, 1, 0), high_ai = if_else(ai_felten >= quantile(ai_felten, 0.75, na.rm = TRUE), 1, 0))
# Sort for lags
panel <- panel %>% arrange(sector_id, year) %>% group_by(sector_id) %>% mutate(ai_felten_lag = lag(ai_felten, 1), fo_risk_lag = lag(fo_risk, 1), delta_ai = ai_felten - ai_felten_lag) %>% ungroup()
# Remove missing key vars
panel <- panel %>% filter(!is.na(wage_gap), !is.na(ai_felten), !is.na(fo_risk))
# Quick sanity check
summary(panel$year)
length(unique(panel$sector_id))
# 3. DESCRIPTIVE STATISTICS & CORRELATIONS
# Summary stats for main vars
summary_vars <- panel %>% select(wage_gap, skill_premium, ai_felten, fo_risk, rti, share_high_skill, total_emp) %>% summary()
print(summary_vars)
# Correlation matrix
cor_mat <- panel %>% select(wage_gap, skill_premium, ai_felten, fo_risk, rti) %>% cor(use = "pairwise.complete.obs")
print(cor_mat)
# Correlation tests (bivariate)
cor_test_felten_gap <- cor.test(panel$wage_gap, panel$ai_felten)
cor_test_fo_gap <- cor.test(panel$wage_gap, panel$fo_risk)
cor_test_rti_gap <- cor.test(panel$wage_gap, panel$rti)
print(cor_test_felten_gap)
print(cor_test_fo_gap)
print(cor_test_rti_gap)
# 4. BIVARIATE OLS (RECAP PRELIMINARY ANALYSIS)
# 4.1 Wage gap ~ AI Exposure
ols_gap_felten <- lm(wage_gap ~ ai_felten, data = panel)
summary(ols_gap_felten)
# 4.2 Wage gap ~ Automation Risk
ols_gap_fo <- lm(wage_gap ~ fo_risk, data = panel)
summary(ols_gap_fo)
# 4.3 Wage gap ~ AI Exposure + Automation Risk
ols_gap_biv <- lm(wage_gap ~ ai_felten + fo_risk, data = panel)
summary(ols_gap_biv)
# (Optional) using skill_premium instead of wage_gap
ols_sp_felten <- lm(skill_premium ~ ai_felten, data = panel)
summary(ols_sp_felten)
# Scatterplots with fitted line
ggplot(panel, aes(x = ai_felten, y = wage_gap)) + geom_point(alpha = 0.4) + geom_smooth(method = "lm", se = TRUE) + labs(title = "Wage gap (H - L) vs AI Exposure", x = "AI exposure (Felten-type index)", y = "Wage gap (w_h - w_l)")
ggplot(panel, aes(x = fo_risk, y = wage_gap)) + geom_point(alpha = 0.4) + geom_smooth(method = "lm", se = TRUE) + labs(title = "Wage gap (H - L) vs Automation Risk", x = "Automation risk (Frey-Osborne-type index)", y = "Wage gap (w_h - w_l)")
# 5. PANEL FE REGRESSIONS (BASELINE)
# Two-way FE: sector + year, clustered by sector
fe_base_felten <- feols(wage_gap ~ ai_felten | sector_id + year, data = panel, cluster = ~ sector_id)

fe_base_fo <- feols(wage_gap ~ fo_risk | sector_id + year, data = panel, cluster = ~ sector_id)
etable(fe_base_felten, fe_base_fo, dict = c(ai_felten = "AI Exposure", fo_risk = "Automation Risk"), title = "Baseline FE regressions (wage gap)")
# 6. DYNAMIC SPECIFICATION (LAGGED AI EXPOSURE)
fe_lag_felten <- feols(wage_gap ~ ai_felten_lag | sector_id + year, data = panel, cluster = ~ sector_id)
fe_lag_fo <- feols(wage_gap ~ fo_risk_lag | sector_id + year, data = panel, cluster = ~ sector_id)
etable(fe_lag_felten, fe_lag_fo, dict = c(ai_felten_lag = "Lagged AI Exposure", fo_risk_lag = "Lagged Automation Risk"), title = "Dynamic FE (lagged AI / automation risk)")
# 7. INTERACTION WITH RTI (ROUTINE TASK INTENSITY)
fe_int_felten_rti <- feols(wage_gap ~ ai_felten * rti | sector_id + year, data = panel, cluster = ~ sector_id)
etable(fe_int_felten_rti, dict = c(ai_felten = "AI Exposure", rti = "Routine Task Intensity", "ai_felten:rti" = "AI × RTI interaction"), title = "Interaction: AI exposure × RTI")
# 8. DID: HIGH-AI SECTORS × POST
did_highAI <- feols(wage_gap ~ high_ai * post_period | sector_id + year, data = panel, cluster = ~ sector_id)
etable(did_highAI, dict = c(high_ai = "High AI sector (top 25%)", post_period = "Post-period (post threshold)", "high_ai:post_period" = "High AI × Post"), title = "DiD: High-AI sectors vs others, pre/post")
# 9. FIRST DIFFERENCE SPECIFICATION (ΔAI)
panel_fd <- panel %>% filter(!is.na(delta_ai))
fe_fd <- feols(wage_gap ~ delta_ai | sector_id + year, data = panel_fd, cluster = ~ sector_id)
etable(fe_fd, dict = c(delta_ai = "Δ AI exposure"), title = "First-difference specification (ΔAI)")
# 10. COLLECT MAIN RESULTS INTO A TIDY TABLE
models_list <- list("OLS: wage_gap ~ ai_felten" = ols_gap_felten, "OLS: wage_gap ~ ai_felten + fo_risk" = ols_gap_biv, "FE: ai_felten (2-way FE)" = fe_base_felten, "FE-lag: ai_felten_lag (2-way FE)" = fe_lag_felten, "FE: ai_felten × rti" = fe_int_felten_rti, "DiD: high_ai × post_period" = did_highAI, "FD: ΔAI" = fe_fd)
extract_main_coef <- function(model_obj, coef_name_pattern = "ai_felten|high_ai:post_period|delta_ai") {
  broom::tidy(model_obj) %>% filter(str_detect(term, coef_name_pattern)) %>% select(term, estimate, std.error, statistic, p.value)}
results_table <- map_df(models_list, ~ extract_main_coef(.x), .id = "model")
print(results_table)
if (!dir.exists("output")) dir.create("output")
write_csv(results_table, "output/cap4_main_results.csv")
# 12. FE combined: AI exposure + Automation Risk
fe_combined <- feols(wage_gap ~ ai_felten + fo_risk | sector_id + year, data = panel, cluster = ~ sector_id)
etable(fe_combined, dict = c(ai_felten = "AI Exposure", fo_risk = "Automation Risk"), title = "FE combined: AI exposure and automation risk")
# 13. Log-type model: skill_premium as dependent variable
fe_sp_felten <- feols(skill_premium ~ ai_felten | sector_id + year, data = panel, cluster = ~ sector_id)
fe_sp_combined <- feols(skill_premium ~ ai_felten + fo_risk | sector_id + year, data = panel, cluster = ~ sector_id)
etable(fe_sp_felten, fe_sp_combined, dict = c(ai_felten = "AI Exposure", fo_risk = "Automation Risk"), title = "FE regressions on skill premium")
# 14. FE with controls: skill share and employment size
panel <- panel %>% mutate(log_total_emp = log(total_emp))
fe_controls <- feols(wage_gap ~ ai_felten + share_high_skill + log_total_emp | sector_id + year, data = panel, cluster = ~ sector_id)
etable(fe_controls, dict = c(ai_felten = "AI Exposure", share_high_skill = "Share of high-skill workers", log_total_emp = "Log total employment"), title = "FE with additional controls")
models_list <- list("OLS: wage_gap ~ ai_felten" = ols_gap_felten, "OLS: wage_gap ~ ai_felten + fo_risk" = ols_gap_biv, "FE: ai_felten (2-way FE)" = fe_base_felten, "FE-lag: ai_felten_lag (2-way FE)" = fe_lag_felten, "FE: ai_felten × rti" = fe_int_felten_rti, "DiD: high_ai × post_period" = did_highAI, "FD: ΔAI" = fe_fd, "FE combined: ai_felten + fo_risk" = fe_combined, "FE skill prem: ai_felten" = fe_sp_felten, "FE skill prem: ai_felten + fo_risk" = fe_sp_combined, "FE controls: ai_felten + controls" = fe_controls)
extract_main_coef <- function(model_obj, coef_name_pattern = "ai_felten|high_ai:post_period|delta_ai") {
  broom::tidy(model_obj) %>% filter(str_detect(term, coef_name_pattern)) %>% select(term, estimate, std.error, statistic, p.value)}
results_table <- map_df(models_list, ~ extract_main_coef(.x), .id = "model")
print(results_table)
write_csv(results_table, "output/cap4_main_results_extended.csv")
fe_contemp <- feols(wage_gap ~ ai_felten | sector_id + year, data = panel, cluster = ~ sector_id)
fe_lag_only <- feols(wage_gap ~ ai_felten_lag | sector_id + year, data = panel, cluster = ~ sector_id)
fe_both <- feols(wage_gap ~ ai_felten + ai_felten_lag | sector_id + year, data = panel, cluster = ~ sector_id)
etable(fe_contemp, fe_lag_only, fe_both, dict = c(ai_felten = "AI Exposure (t)", ai_felten_lag = "AI Exposure (t−1)"), title = "Table C.2 ,  Contemporaneous vs lagged AI exposure (TWFE)")
########### PREFERRED SPECIFICATION
library(tidyverse)
library(fixest)
# 1. Load data 
df <- read_csv("dataset_tesi_finale_completo.csv")
# 2. regression dataset
df_reg <- df %>% mutate(wage_gap_dollars = wage_H - wage_L, log_total_emp = log(total_emp)) %>% filter(YEAR >= 2016, total_emp > 1000)
# 3. Change in AI exposure (2016–2025)
ai_trend <- df_reg %>% group_by(YEAR) %>% summarise(avg_ai = weighted.mean(exposure_ai, total_emp, na.rm = TRUE), .groups = "drop")
delta_ai <- ai_trend$avg_ai[ai_trend$YEAR == 2025] - ai_trend$avg_ai[ai_trend$YEAR == 2016]
cat(">>> Change in average AI exposure (2016–2025):", round(delta_ai, 4), "\n")
# 4. Actual change in the wage gap (2016–2025) 
gap_trend <- df_reg %>% group_by(YEAR) %>% summarise(avg_gap = weighted.mean(wage_gap_dollars, total_emp, na.rm = TRUE), .groups = "drop")
actual_change_dollars <- gap_trend$avg_gap[gap_trend$YEAR == 2025] - gap_trend$avg_gap[gap_trend$YEAR == 2016]
cat(">>> Actual change in wage gap (2016–2025):", round(actual_change_dollars, 2), "dollars/hour\n")
# 5. Preferred FE model with controls 
model_best_scenario <- feols(wage_gap_dollars ~ exposure_ai + share_high_skill + log_total_emp | YEAR + IND1990, data = df_reg, weights = ~ total_emp)
beta_ai_best <- coef(model_best_scenario)["exposure_ai"]
cat(">>> 'Best-scenario' AI coefficient:", round(beta_ai_best, 3), " (dollars/hour per unit of AI exposure)\n")
# 6. Counterfactual impact of AI --------------------------------------------
impact_best <- beta_ai_best * delta_ai
share_best <- abs(impact_best / actual_change_dollars) * 100
cat("\n>>> COUNTERFACTUAL RESULT – PREFERRED SPECIFICATION <<<\n")
cat("1. Estimated AI impact on the wage gap:", round(impact_best, 2), "dollars/hour\n")
cat("2. Share of the observed change explained by AI:", round(share_best, 2), "%\n")
# Sector-level decomposition: TOP 10 & BOTTOM 10
library(tidyverse)
# 1. Load data
df <- read_csv("dataset_tesi_finale_completo.csv")
df_reg <- df %>% mutate(wage_gap_dollars = wage_H - wage_L) %>% filter(YEAR >= 2016, total_emp > 1000)
# 2. Preferred AI coefficient
beta_ai <- 1.62 
# 3. Build sector trends
sector_trends <- df_reg %>% group_by(IND1990, YEAR) %>% summarise(avg_ai = weighted.mean(exposure_ai, total_emp, na.rm = TRUE), avg_gap = weighted.mean(wage_gap_dollars, total_emp, na.rm = TRUE), .groups = "drop")
# Wide format: 2016 vs 2025
sector_1625 <- sector_trends %>% filter(YEAR %in% c(2016, 2025)) %>% pivot_wider(id_cols = IND1990, names_from = YEAR, values_from = c(avg_ai, avg_gap), names_sep = "_")
# 4. Compute sector-level predicted effects
sector_ai_effects <- sector_1625 %>% mutate(delta_ai = avg_ai_2025 - avg_ai_2016, observed_gap_chg = avg_gap_2025 - avg_gap_2016, predicted_ai_eff = beta_ai * delta_ai, share_explained = if_else(!is.na(observed_gap_chg) & observed_gap_chg != 0, (predicted_ai_eff / observed_gap_chg) * 100, NA_real_))
# 5. Top 10 most affected sectors
top10_affected <- sector_ai_effects %>% arrange(desc(predicted_ai_eff)) %>% slice(1:10)
# 6. Bottom 10 least affected sectors
bottom10_affected <- sector_ai_effects %>% arrange(predicted_ai_eff) %>% slice(1:10)
# 7. Export for LaTeX tables
if (!dir.exists("output")) dir.create("output")
write_csv(top10_affected, "output/top10_ai_effect_sectors.csv")
write_csv(bottom10_affected, "output/bottom10_ai_effect_sectors.csv")
write_csv(sector_ai_effects, "output/all_sectors_ai_impact.csv")
# Show previews
cat("\n>>> TOP 10 SECTORS MOST AFFECTED BY AI <<<\n"); print(top10_affected)
cat("\n>>> BOTTOM 10 SECTORS LEAST AFFECTED BY AI <<<\n"); print(bottom10_affected)
# 1. Lookup table
sector_names <- tribble(~IND1990, ~sector_name, 40, "Mining", 41, "Oil & Gas Extraction", 101, "Agriculture & Forestry", 132, "Textile Mills", 141, "Apparel Manufacturing", 161, "Wood Products", 162, "Paper Manufacturing", 182, "Printing & Publishing", 220, "Chemicals", 221, "Pharmaceuticals", 231, "Plastics & Rubber", 252, "Primary Metals", 281, "Machinery", 290, "Computer Equipment", 292, "Electronics", 360, "Transportation Equipment", 361, "Motor Vehicles", 400, "Wholesale Trade", 422, "Retail Trade", 542, "Finance & Insurance", 771, "Professional Services")
# 2. Add names to top and bottom sectors
top10_named <- top10_affected %>% left_join(sector_names, by = "IND1990") %>% select(sector_name, delta_ai, predicted_ai_eff, observed_gap_chg, share_explained)
bottom10_named <- bottom10_affected %>% left_join(sector_names, by = "IND1990") %>% select(sector_name, delta_ai, predicted_ai_eff, observed_gap_chg, share_explained)
# 3. Preview
cat("\n>>> TOP 10 SECTORS MOST AFFECTED BY AI (NAMED) <<<\n"); print(top10_named)
cat("\n>>> BOTTOM 10 SECTORS LEAST AFFECTED BY AI (NAMED) <<<\n"); print(bottom10_named)
# 4. Export
write_csv(top10_named, "output/top10_ai_effect_sectors_named.csv")
write_csv(bottom10_named, "output/bottom10_ai_effect_sectors_named.csv")
