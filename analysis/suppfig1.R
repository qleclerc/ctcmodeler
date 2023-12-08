
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

col_pal = brewer.pal(7, "Dark2")

adm_data = read.csv(here::here("data", "toy_admission.csv"), sep=";") %>%
  select(id, hospitalization, cat)
adm_data$cat[adm_data$cat == ""] = adm_data$hospitalization[adm_data$cat == ""]
adm_data = adm_data[,c(1,3)] %>%
  distinct()
eq_table = openxlsx::read.xlsx(here::here("data", "cat_groupings.xlsx")) %>%
  select(cat, cat_ag)

adm_data = adm_data %>%
  left_join(eq_table, by = "cat") %>%
  count(cat_ag) %>%
  rename(cat = cat_ag)

cohorting_data = read.csv(here::here("results", "cohorting_results.csv"))
cohorting_matrix = read.csv(here::here("results", "cohorting_matrix.csv"))

cohorting_numbers = cohorting_matrix %>%
  left_join(adm_data, by = "cat") %>%
  mutate(n = n*value) %>%
  group_by(variable) %>%
  summarise(n=sum(n)) %>%
  rename(scenario = variable) %>%
  mutate(n = replace(n, n==0, max(n)))

cohorting_data = cohorting_data %>%
  left_join(cohorting_numbers, by = "scenario") %>%
  mutate(rel_val = val/n,
         rel_lower = lower/n,
         rel_upper = upper/n)


cohorting_matrix$value[which(cohorting_matrix$value!=0 & cohorting_matrix$cat=="Nurses")] = 1
cohorting_matrix$value[which(cohorting_matrix$value!=0 & cohorting_matrix$cat=="Care assistants")] = 2
cohorting_matrix$value[which(cohorting_matrix$value!=0 & cohorting_matrix$cat=="Reeducation")] = 3
cohorting_matrix$value[which(cohorting_matrix$value!=0 & cohorting_matrix$cat=="Doctors")] = 4
cohorting_matrix$value[which(cohorting_matrix$value!=0 & cohorting_matrix$cat=="Porters")] = 5
cohorting_matrix$value[which(cohorting_matrix$value!=0 & cohorting_matrix$cat=="Other")] = 6

cohorting_matrix[cohorting_matrix=="Doctors"] = "Physicians"
cohorting_data[cohorting_data=="Doctors"] = "Physicians"
cohorting_matrix[cohorting_matrix=="Reeducation"] = "Rehabilitation"
cohorting_data[cohorting_data=="Reeducation"] = "Rehabilitation"
cohorting_matrix[cohorting_matrix=="Care assistants"] = "Healthcare asst."
cohorting_data[cohorting_data=="Care assistants"] = "Healthcare asst."


# BY VALUE ##########
cohorting_datav = cohorting_data %>%
  arrange(desc(val)) %>%
  mutate(scenario = factor(scenario, levels = unique(scenario)))

cohorting_matrixv = cohorting_matrix %>%
  filter(variable %in% unique(cohorting_datav$scenario)) %>%
  mutate(variable = factor(variable, levels = levels(cohorting_datav$scenario))) %>%
  mutate(cat = factor(cat, levels = rev(unique(cat)))) %>%
  mutate(value = as.factor(value))

pa = ggplot(cohorting_datav) +
  geom_col(aes(scenario, val)) +
  geom_errorbar(aes(scenario, val, ymin = lower, ymax = upper)) +
  scale_y_continuous(breaks = seq(-0.4,0.4,0.1)) +
  theme_bw() +
  labs(x = "" , y = "Relative reduction in\ncumulative incidence") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(t=5,l=43,r=5,b=0))


pb = ggplot(cohorting_matrixv) +
  geom_point(aes(variable, cat, fill = value),
             pch = 22, size = 3) +
  scale_fill_manual(values = c("white", col_pal[c(7,2,3,4,6,5)])) +
  theme_bw() +
  labs(x = "Scenario", y = "Categories reallocated") +
  guides(fill = "none") +
  theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1))


# BY VALUE ##########
cohorting_datav = cohorting_data %>%
  arrange(desc(rel_val)) %>%
  mutate(scenario = factor(scenario, levels = unique(scenario)))

cohorting_matrixv = cohorting_matrix %>%
  filter(variable %in% unique(cohorting_datav$scenario)) %>%
  mutate(variable = factor(variable, levels = levels(cohorting_datav$scenario))) %>%
  mutate(cat = factor(cat, levels = rev(unique(cat)))) %>%
  mutate(value = as.factor(value))

pc = ggplot(cohorting_datav) +
  geom_col(aes(scenario, rel_val)) +
  geom_errorbar(aes(scenario, rel_val, ymin = rel_lower, ymax = rel_upper)) +
  scale_y_continuous(breaks = seq(-0.003,0.003,0.001)) +
  theme_bw() +
  labs(x = "" , y = "Relative reduction in\ncumulative incidence") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(t=5,l=32,r=5,b=0))


pd = ggplot(cohorting_matrixv) +
  geom_point(aes(variable, cat, fill = value),
             pch = 22, size = 3) +
  scale_fill_manual(values = c("white", col_pal[c(7,2,3,4,6,5)])) +
  theme_bw() +
  labs(x = "Scenario", y = "Categories reallocated") +
  guides(fill = "none") +
  theme(axis.text.x = element_text(size=8, angle = 45, hjust = 1))

plot_grid(pa, pb, pc, pd, ncol = 1, rel_heights = c(1,0.8,1,0.8), labels = c("a)", "", "b)", ""))

ggsave(here::here("figures", "suppfig1.png"), width = 10, height = 10)
