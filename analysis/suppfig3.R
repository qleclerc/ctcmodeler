
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(cowplot)
library(reshape2)
library(RColorBrewer)
library(ggh4x)
library(ggtext)
library(openxlsx)

col_pal = c("white", brewer.pal(7, "Dark2"), "grey30")

adm_data = read.csv(here::here("data", "toy_admission.csv"), sep=";") %>%
  select(id, hospitalization, cat)
adm_data$cat[adm_data$cat == ""] = adm_data$hospitalization[adm_data$cat == ""]
adm_data = adm_data[,c(1,3)] %>%
  distinct()
eq_table = openxlsx::read.xlsx(here::here("data", "cat_groupings.xlsx")) %>%
  select(cat, cat_ag)
adm_data = adm_data %>%
  mutate(group = grepl("PA-", id)) %>%
  mutate(group = replace(group, group==T, "Patients")) %>%
  mutate(group = replace(group, group==F, "Staff")) %>%
  left_join(eq_table, by="cat")

data_none = read.csv2(here::here("contact", "matContactBuiltSimulatedInterventionbyCatCohorting.csv"))
data_nurses = read.csv2(here::here("contact", "matContactBuiltSimulatedInterventionbyCatCohorting1_iter_1_scenario_1.csv"))
data_care = read.csv2(here::here("contact", "matContactBuiltSimulatedInterventionbyCatCohorting2_iter_1_scenario_2.csv"))
data_reeducation = read.csv2(here::here("contact", "matContactBuiltSimulatedInterventionbyCatCohorting3_iter_1_scenario_3.csv"))
data_doctors = read.csv2(here::here("contact", "matContactBuiltSimulatedInterventionbyCatCohorting4_iter_1_scenario_4.csv"))
data_porters = read.csv2(here::here("contact", "matContactBuiltSimulatedInterventionbyCatCohorting5_iter_1_scenario_5.csv"))
data_other = read.csv2(here::here("contact", "matContactBuiltSimulatedInterventionbyCatCohorting6_iter_1_scenario_6.csv"))
data_all = read.csv2(here::here("contact", "matContactBuiltSimulatedInterventionbyCatCohorting63_iter_1_scenario_63.csv"))
data_random = read.csv2(here::here("contact", "matContactBuiltRandomGraphbyCatCohorting.csv"))

# Cohorting baseline ##

data_none = data_none %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PE-PA" | type == "PA-PE")
unique_none = c()

for(id in unique(c(data_none$from, data_none$to))){
  data_id = data_none %>%
    filter(from == id | to == id)
  data_id = c(data_id$from, data_id$to) %>%
    unique() %>%
    length()
  unique_none = c(unique_none, data_id)
}


# Cohorting nurses ##

data_nurses = data_nurses %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PE-PA" | type == "PA-PE")
unique_nurses = c()

for(id in unique(c(data_nurses$from, data_nurses$to))){
  data_id = data_nurses %>%
    filter(from == id | to == id)
  data_id = c(data_id$from, data_id$to) %>%
    unique() %>%
    length()
  unique_nurses = c(unique_nurses, data_id)
}
  

# Cohorting care ##

data_care = data_care %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PE-PA" | type == "PA-PE")
unique_care = c()

for(id in unique(c(data_care$from, data_care$to))){
  data_id = data_care %>%
    filter(from == id | to == id)
  data_id = c(data_id$from, data_id$to) %>%
    unique() %>%
    length()
  unique_care = c(unique_care, data_id)
}


# Cohorting reeducation ##

data_reeducation = data_reeducation %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PE-PA" | type == "PA-PE")
unique_reeducation = c()

for(id in unique(c(data_reeducation$from, data_reeducation$to))){
  data_id = data_reeducation %>%
    filter(from == id | to == id)
  data_id = c(data_id$from, data_id$to) %>%
    unique() %>%
    length()
  unique_reeducation = c(unique_reeducation, data_id)
}


# Cohorting doctors ##

data_doctors = data_doctors %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PE-PA" | type == "PA-PE")
unique_doctors = c()

for(id in unique(c(data_doctors$from, data_doctors$to))){
  data_id = data_doctors %>%
    filter(from == id | to == id)
  data_id = c(data_id$from, data_id$to) %>%
    unique() %>%
    length()
  unique_doctors = c(unique_doctors, data_id)
}


# Cohorting porters ##

data_porters = data_porters %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PE-PA" | type == "PA-PE")
unique_porters = c()

for(id in unique(c(data_porters$from, data_porters$to))){
  data_id = data_porters %>%
    filter(from == id | to == id)
  data_id = c(data_id$from, data_id$to) %>%
    unique() %>%
    length()
  unique_porters = c(unique_porters, data_id)
}


# Cohorting other ##

data_other = data_other %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PE-PA" | type == "PA-PE")
unique_other = c()

for(id in unique(c(data_other$from, data_other$to))){
  data_id = data_other %>%
    filter(from == id | to == id)
  data_id = c(data_id$from, data_id$to) %>%
    unique() %>%
    length()
  unique_other = c(unique_other, data_id)
}


# Cohorting all ##

data_all = data_all %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PE-PA" | type == "PA-PE")
unique_all = c()

for(id in unique(c(data_all$from, data_all$to))){
  data_id = data_all %>%
    filter(from == id | to == id)
  data_id = c(data_id$from, data_id$to) %>%
    unique() %>%
    length()
  unique_all = c(unique_all, data_id)
}

# Cohorting random ##

data_random = data_random %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PE-PA" | type == "PA-PE")
unique_random = c()

for(id in unique(c(data_random$from, data_random$to))){
  data_id = data_random %>%
    filter(from == id | to == id)
  data_id = c(data_id$from, data_id$to) %>%
    unique() %>%
    length()
  unique_random = c(unique_random, data_id)
}

# Plot together ##

summary_data = data.frame(Baseline = unique_none, `Healthcare assistants` = unique_care, Nurses = unique_nurses,
                          Other = unique_other, Rehabilitation = unique_reeducation,
                          Porters = unique_porters, Physicians = unique_doctors,
                          All = unique_all, Random = unique_random) %>%
  melt()

ggplot(summary_data) +
  geom_boxplot(aes(variable, value, fill = variable)) +
  scale_fill_manual(values = col_pal[c(1,2,5,6,4,7,8,3,9)]) +
  theme_bw() +
  guides(fill = "none") +
  labs(x = "Staff reallocation scenario", y = "Number of unique patients per staff") +
  theme(text = element_text(size=12))

ggsave(here::here("figures", "suppfig3.png"), width = 10)
