01_data_construction.R
library(tidyverse)
library(ipumsr)
# 1. LOAD RISK DATASETS (Frey, Felten, RTI)
# A. Frey & Osborne (Automation Risk - Beta)
fo <- read_csv("frey_osborne.csv", show_col_types = FALSE) %>% mutate(soc_clean = str_remove_all(soc_code, "-")) %>% select(soc_clean, auto_prob)
# B. Felten et al. (AI Exposure - Gamma)
ai_raw <- read_csv("felten_ai_exposure.csv", show_col_types = FALSE)
if(ncol(ai_raw) < 2) { ai_raw <- read_delim("felten_ai_exposure.csv", delim = ";", show_col_types = FALSE) }
ai <- ai_raw %>% select(1, ncol(ai_raw)) %>% set_names(c("soc_code", "ai_score")) %>% mutate(soc_clean = str_remove_all(soc_code, "-"), ai_score_str = str_replace(as.character(ai_score), ",", "."), ai_score = as.numeric(ai_score_str)) %>% filter(!is.na(ai_score)) %>% select(soc_clean, ai_score)
cat(">>> Felten AI Rows Loaded:", nrow(ai), "\n")
# C. RTI (Routine Task Intensity - Robustness)
rti <- read_csv("rti.csv", show_col_types = FALSE) %>% rename(occ1990dd = 1, rti_index = 2) 
# 2. LOAD CROSSWALKS
cat(">>> 2. Loading Crosswalks...\n")
# 1: CPS (OCC2010) -> Frey/Felten (SOC)
cw_soc <- read_csv("occ2010_soc2010.csv", show_col_types = FALSE) %>% mutate(occ2010 = as.numeric(occ2010), soc_clean = str_remove_all(soc2010, "-")) %>% distinct(occ2010, .keep_all = TRUE) %>% select(occ2010, soc_clean)
# 2: CPS (OCC2010) -> RTI (OCC1990dd)
cw_rti <- read_csv("occ2010_occ1990dd.csv", show_col_types = FALSE) %>% mutate(occ2010 = as.numeric(occ2010), occ1990dd = as.numeric(occ1990dd)) %>% distinct(occ2010, .keep_all = TRUE) %>% select(occ2010, occ1990dd)
# 3. CREATE RISK TABLE ---
cat(">>> 3. Creating Risk Table...\n")
occupation_risks <- cw_soc %>% left_join(fo, by = "soc_clean") %>% left_join(ai, by = "soc_clean") %>% left_join(cw_rti, by = "occ2010") %>% left_join(rti, by = "occ1990dd") %>% select(occ2010, auto_prob, ai_score, rti_index)
cat(">>> Risk Table Ready. Rows:", nrow(occupation_risks), "\n")
# 4. LOAD CPS MICRODATA
cat(">>> 4. Loading and Cleaning CPS Data (Please wait...)\n")
ddi_file <- "cps_00001.xml"
data_file <- "cps_00001.dat.gz"
cps_ddi <- read_ipums_ddi(ddi_file)
cps_raw <- read_ipums_micro(cps_ddi, data_file = data_file)

cps_clean <- cps_raw %>% filter(EMPSTAT %in% c(10, 12)) %>% filter(INCWAGE > 0, UHRSWORKLY > 0, WKSWORK1 > 0) %>% mutate(annual_hours = WKSWORK1 * UHRSWORKLY, hourly_wage = INCWAGE / annual_hours, is_high_skill = if_else(EDUC >= 111, 1, 0), occ2010 = as.numeric(OCC2010)) %>% filter(hourly_wage > 2 & hourly_wage < 1000)
# 5. MERGE RISKS TO WORKERS
cat(">>> 5. Merging risks to individual workers...\n")
cps_w_risks <- cps_clean %>% left_join(occupation_risks, by = "occ2010")
# 6. AGGREGATE TO INDUSTRY (FINAL DATASET)
cat(">>> 6. Creating Final Industry-Year Panel...\n")
final_dataset <- cps_w_risks %>% group_by(YEAR, IND1990) %>% summarise(wage_H = weighted.mean(hourly_wage[is_high_skill == 1], ASECWT[is_high_skill == 1], na.rm = TRUE), wage_L = weighted.mean(hourly_wage[is_high_skill == 0], ASECWT[is_high_skill == 0], na.rm = TRUE), skill_premium = log(wage_H) - log(wage_L), exposure_ai = weighted.mean(ai_score, ASECWT, na.rm = TRUE), risk_auto = weighted.mean(auto_prob, ASECWT, na.rm = TRUE), risk_routine = weighted.mean(rti_index, ASECWT, na.rm = TRUE), share_high_skill = weighted.mean(is_high_skill, ASECWT, na.rm = TRUE), total_emp = sum(ASECWT, na.rm = TRUE), .groups = "drop") %>% filter(!is.na(skill_premium), !is.na(exposure_ai))
# --- 7. SAVE ---
write_csv(final_dataset, "dataset_tesi_finale_completo.csv")
print(head(final_dataset))


