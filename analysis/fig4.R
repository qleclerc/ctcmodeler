
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

col_pal = c("grey40", brewer.pal(3, "Set1")[-3])
  
adm_data = read.csv(here::here("data", "toy_admission.csv"), sep=";") %>%
  select(id, hospitalization, cat)
adm_data$cat[adm_data$cat == ""] = adm_data$hospitalization[adm_data$cat == ""]
adm_data = adm_data[,c(1,3)] %>%
  distinct()
eq_table = openxlsx::read.xlsx(here::here("data", "cat_groupings.xlsx")) %>%
  select(cat, cat_ag) %>%
  mutate(cat_ag = replace(cat_ag, cat_ag == "Doctors", "Physicians")) %>%
  mutate(cat_ag = replace(cat_ag, cat_ag == "Reeducation", "Rehabilitation")) %>%
  mutate(cat_ag = replace(cat_ag, cat_ag == "Care assistants", "Healthcare asst."))
sc_files = list.files(here::here("interventions"))

sc_data = data.frame()

for(f in sc_files){
  dat = read.csv(here::here("interventions", f), sep=";")[,c(2:3)] %>%
    rename(id = name)
  if(grepl("tc_", f)) dat = cbind(dat, type = "Frequency SC")
  else dat = cbind(dat, type = "Duration SC")
  
  if(grepl("SCPA", f)) dat = cbind(dat, group = "Patients")
  else dat = cbind(dat, group = "Staff")
  
  sc_data = rbind(sc_data, dat)
}

# which people are both freq and dur supercontactors?
sc_data %>%
  count(id) %>%
  arrange(n) %>%
  filter(n == 2) %>%
  left_join(adm_data, "id") %>%
  left_join(eq_table, "cat")

sc_data = left_join(sc_data, adm_data, "id") %>%
  group_by(type, group, cat) %>%
  summarise(n = n()) %>%
  mutate(cat = as.factor(cat)) %>%
  ungroup %>%
  complete(type, nesting(group, cat), fill = list(n=0)) %>%
  left_join(eq_table, "cat")

adm_data = adm_data %>%
  mutate(group = grepl("PA-", id)) %>%
  mutate(group = replace(group, group==T, "Patients")) %>%
  mutate(group = replace(group, group==F, "Staff")) %>%
  left_join(eq_table, by="cat") %>%
  count(group, cat_ag) %>%
  group_by(group) %>%
  mutate(rel_n = n/sum(n)) %>%
  filter(!is.na(cat_ag)) %>%
  mutate(type = "All")

sc_data %>%
  group_by(type, group, cat_ag) %>%
  summarise(n = sum(n)) %>%
  group_by(type, group) %>%
  mutate(rel_n = n/sum(n)) %>%
  rbind(adm_data) %>%
  ggplot() +
  geom_col(aes(cat_ag, rel_n, group = type, fill = type), position = "dodge", width = 0.7) +
  scale_fill_discrete(type = col_pal) +
  facet_grid(cols = vars(group), scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text = element_text(size=12),
        legend.position = "bottom") +
  labs(x = "", y = "Proportion", fill = "Population:")

ggsave(here::here("figures", "fig4.png"), width = 10, height = 6)


chisq_data_dur_pat = adm_data %>%
  filter(group =="Patients") %>%
  left_join(sc_data %>%
              filter(type == "Duration SC" & group == "Patients") %>%
              group_by(cat_ag) %>%
              summarise(n_sc = sum(n)), by = "cat_ag") %>%
  filter(cat_ag != "<NA>") %>%
  mutate(n = n-n_sc)

chisq_data_dur_pat = matrix(c(chisq_data_dur_pat$n, chisq_data_dur_pat$n_sc), nrow = 2, byrow = T,
                            dimnames = list(c("non-SC", "SC"),
                                            chisq_data_dur_pat$cat_ag))
DescTools::GTest(chisq_data_dur_pat)

chisq_data_freq_pat = adm_data %>%
  filter(group =="Patients") %>%
  left_join(sc_data %>%
              filter(type == "Frequency SC" & group == "Patients") %>%
              group_by(cat_ag) %>%
              summarise(n_sc = sum(n)), by = "cat_ag") %>%
  filter(cat_ag != "<NA>") %>%
  mutate(n = n-n_sc)

chisq_data_freq_pat = matrix(c(chisq_data_freq_pat$n, chisq_data_freq_pat$n_sc), nrow = 2, byrow = T,
                            dimnames = list(c("non-SC", "SC"),
                                            chisq_data_freq_pat$cat_ag))
DescTools::GTest(chisq_data_freq_pat)


chisq_data_dur_sta = adm_data %>%
  filter(group =="Staff") %>%
  left_join(sc_data %>%
              filter(type == "Duration SC" & group == "Staff") %>%
              group_by(cat_ag) %>%
              summarise(n_sc = sum(n)), by = "cat_ag") %>%
  filter(cat_ag != "<NA>") %>%
  mutate(n = n-n_sc)

chisq_data_dur_sta = matrix(c(chisq_data_dur_sta$n, chisq_data_dur_sta$n_sc), nrow = 2, byrow = T,
                            dimnames = list(c("non-SC", "SC"),
                                            chisq_data_dur_sta$cat_ag))
DescTools::GTest(chisq_data_dur_sta)


chisq_data_freq_sta = adm_data %>%
  filter(group =="Staff") %>%
  left_join(sc_data %>%
              filter(type == "Frequency SC" & group == "Staff") %>%
              group_by(cat_ag) %>%
              summarise(n_sc = sum(n)), by = "cat_ag") %>%
  filter(cat_ag != "<NA>") %>%
  mutate(n = n-n_sc)

chisq_data_freq_sta = matrix(c(chisq_data_freq_sta$n, chisq_data_freq_sta$n_sc), nrow = 2, byrow = T,
                             dimnames = list(c("non-SC", "SC"),
                                             chisq_data_freq_sta$cat_ag))

DescTools::GTest(chisq_data_freq_sta)

