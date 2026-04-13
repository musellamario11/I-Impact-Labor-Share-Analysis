02_descriptive_analysis.R
library(tidyverse)
library(ggplot2)
library(scales)
library(patchwork)
library(kableExtra)
 final_dataset <- read_csv("dataset_tesi_finale_completo.csv", show_col_types = FALSE)
 print(head(final_dataset))
econ_cols <- list(blue   = "#1f77b4", red    = "#d62728", green  = "#2ca02c", orange = "#ff7f0e", gray   = "#7f7f7f")
theme_econ <- theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold", size = 15, hjust = 0),plot.subtitle = element_text(size = 11, hjust = 0), axis.title    = element_text(face = "bold"), panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_text(face = "bold")  )
# 2. Descriptive table
vars_to_describe <- c( "wage_H", "wage_L", "skill_premium","exposure_ai", "risk_auto", "risk_routine", "share_high_skill", "total_emp")
desc_table <- final_dataset %>%
  select(all_of(vars_to_describe)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise (N  = sum(!is.na(value)), Mean   = mean(value, na.rm = TRUE), SD     = sd(value, na.rm = TRUE),  P25    = quantile(value, 0.25, na.rm = TRUE), Median = quantile(value, 0.50, na.rm = TRUE), P75    = quantile(value, 0.75, na.rm = TRUE), Min    = min(value, na.rm = TRUE), Max    = max(value, na.rm = TRUE), .groups = "drop") %>%
 arrange(match(variable, vars_to_describe))
print(desc_table)
 # 3. Time series plots (industry-weighted averages by year)
 ts_year <- final_dataset %>%
   group_by(YEAR) %>%
   summarise(
  skill_premium = weighted.mean(skill_premium, w = total_emp, na.rm = TRUE), exposure_ai   = weighted.mean(exposure_ai, w= total_emp, na.rm = TRUE), risk_auto     = weighted.mean(risk_auto, w = total_emp, na.rm = TRUE), risk_routine  = weighted.mean(risk_routine,  w = total_emp, na.rm = TRUE), .groups = "drop" )
 theme_econ_small <- theme_econ + theme(plot.title = element_text(size = 12), axis.title = element_text(size = 10), axis.text  = element_text(size = 9), plot.margin = margin(3, 3, 3, 3))
 # 3.1 Skill premium over time
 p_ts_skill <- ggplot(ts_year, aes(x = YEAR, y = skill_premium)) + geom_line(color = econ_cols$blue, linewidth = 0.9) + geom_point(color = econ_cols$blue, size = 1.8) + labs( title = "Skill premium", x = "Year", y = "Log wage gap (H − L)) + theme_econ_small
 # 3.2 AI exposure over time
 p_ts_ai <- ggplot(ts_year, aes(x = YEAR, y = exposure_ai)) + geom_line(color = econ_cols$green, linewidth = 0.9) +  geom_point(color = econ_cols$green, size = 1.8) +
   labs( title = "AI exposure", x = "Year", y = "AI exposure (Felten)") +  theme_econ_small
 # 3.3 Automation risk over time
 p_ts_auto <- ggplot(ts_year, aes(x = YEAR, y = risk_auto)) + geom_line(color = econ_cols$red, linewidth = 0.9) +  geom_point(color = econ_cols$red, size = 1.8) + labs( title = "Automation risk", x = "Year",  y = "Automation risk (Frey–Osborne)" ) + theme_econ_small
 # 3.4 RTI over time
 p_ts_rti <- ggplot(ts_year, aes(x = YEAR, y = risk_routine)) +geom_line(color = econ_cols$orange, linewidth = 0.9) +  geom_point(color = econ_cols$orange, size = 1.8) +
  labs( title = "Routine task intensity", x = "Year", y = "RTI (Autor–Dorn)") + theme_econ_small 
 # 3.5 Combined grid (2x2)
 g_ts <- (p_ts_skill + p_ts_ai) / (p_ts_auto + p_ts_rti)
 print(g_ts)
 ggsave("fig_timeseries_skill_ai_auto_rti.png",  g_ts, width = 10, height = 8, dpi = 300)
# 4. Scatter plots: tech variables vs skill premium (clean)

scatter_data <- final_dataset %>%
  filter(!is.na(skill_premium), !is.na(exposure_ai), !is.na(risk_auto), !is.na(risk_routine), !is.na(total_emp) )
# 4.1 Skill premium vs AI exposure
p_sc_ai <- ggplot(scatter_data, aes(x = exposure_ai, y = skill_premium)) +  geom_point(aes(size = total_emp), alpha = 0.4, color = econ_cols$green) + geom_smooth(method = "lm", se = TRUE, linewidth = 1, color = econ_cols$blue) + scale_size_continuous(name = "Total employment", labels = scales::label_number(scale_cut = scales::cut_si(" "))) + labs(title = "Skill premium vs AI exposure", x = "AI exposure (Felten)", y = "Skill premium (log wage gap)" ) +  theme_econ
# 4.2 Skill premium vs Automation risk
p_sc_auto <- ggplot(scatter_data, aes(x = risk_auto, y = skill_premium)) + geom_point(aes(size = total_emp), alpha = 0.4, color = econ_cols$red) +  geom_smooth(method = "lm", se = TRUE, linewidth = 1, color = econ_cols$blue) + scale_size_continuous( name = "Total employment", labels = scales::label_number(scale_cut = scales::cut_si(" "))) + labs( title = "Skill premium vs automation risk", x = "Automation risk (Frey–Osborne)",  y = "Skill premium (log wage gap)") + theme_econ
# 4.3 Skill premium vs RTI
p_sc_rti <- ggplot(scatter_data, aes(x = risk_routine, y = skill_premium)) + geom_point(aes(size = total_emp), alpha = 0.4, color = econ_cols$orange) + geom_smooth(method = "lm", se = TRUE, linewidth = 1, color = econ_cols$blue) + scale_size_continuous(name = "Total employment", labels = scales::label_number(scale_cut = scales::cut_si(" "))) + labs(title = "Skill premium vs routine task intensity", x = "RTI (Autor–Dorn)", y = "Skill premium (log wage gap)") + theme_econ
# 4.4 Combined scatter grid
g_scatter <- (p_sc_ai + p_sc_auto + p_sc_rti) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
print(g_scatter)
ggsave("fig_scatter_skill_vs_tech.png", g_scatter, width = 12, height = 6, dpi = 300)
## CORRELATION ANALYSIS
library(ggcorrplot)
df_ana <- df %>%
filter(!is.na(skill_premium),!is.na(exposure_ai),!is.na(risk_auto),!is.na(risk_routine),!is.na(share_high_skill))
# Select key variables for the correlation matrix
vars_corr <- df_ana %>%
 select(skill_premium, exposure_ai, risk_auto, risk_routine, share_high_skill) %>%
  rename("Wage Gap"        = skill_premium, "AI Exposure"     = exposure_ai, "Automation Risk" = risk_auto,"Routine (RTI)"   = risk_routine, "High Skill Share" = share_high_skill)
# Compute correlation matrix 
cor_matrix <- cor(vars_corr, use = "complete.obs")
cat(">>> CORRELATION MATRIX:\n")
print(round(cor_matrix, 2))
ggplot(df_ana, aes(exposure_ai, risk_auto)) + geom_point(alpha=.3) + geom_smooth(method="lm")
boot_corr <- numeric(nrow(df_ana))
for(i in seq_len(nrow(df_ana))) {boot_corr[i] <- cor(df_ana$exposure_ai[-i], df_ana$risk_auto[-i], use = "complete.obs" )}
summary(boot_corr)
library(wCorr)
wc_ai_auto <- weightedCorr(
  x= df_ana$exposure_ai, y = df_ana$risk_auto, weights = df_ana$total_emp, method  = "Pearson" )
wc_ai_auto
# Visualisation 
ggcorrplot(cor_matrix, method = "square", type   = "lower",  # show only the lower triangle lab    = TRUE,     # print correlation coefficients colors = c("blue", "white", "red"), title  = "Correlation Matrix: Technology and Inequality", ggtheme = theme_minimal())
