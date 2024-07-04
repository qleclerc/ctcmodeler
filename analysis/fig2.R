
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

cohorting_data = read.csv(here::here("results", "cohorting_results.csv"))
cohorting_matrix = read.csv(here::here("results", "cohorting_matrix.csv"))


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
  filter(scenario %in% c(1:21,63,64)) %>%
  arrange(desc(val)) %>%
  mutate(scenario = factor(scenario, levels = unique(scenario)))

cohorting_matrixv = cohorting_matrix %>%
  filter(variable %in% unique(cohorting_datav$scenario)) %>%
  mutate(variable = factor(variable, levels = levels(cohorting_datav$scenario))) %>%
  mutate(cat = factor(cat, levels = rev(unique(cat)))) %>%
  mutate(value = as.factor(value))

pa = ggplot(cohorting_datav) +
  geom_col(aes(scenario, val), fill="grey60") +
  geom_errorbar(aes(scenario, val, ymin = lower, ymax = upper), width=0.7) +
  geom_text(aes(scenario, 0.05, label = ifelse(pval>0.05, "n.s.", ""))) +
  scale_y_continuous(breaks = seq(-0.4,0.7,0.2)) +
  theme_bw() +
  labs(x = "" , y = "Relative reduction in\ncumulative incidence") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(t=5,l=42.5,r=5,b=0))


pb = ggplot(cohorting_matrixv) +
  geom_point(aes(variable, cat, fill = value),
             pch = 22, size = 8) +
  scale_fill_manual(values = c("white", col_pal[c(4,1,3,7,6,5)])) +
  theme_bw() +
  labs(y = "Categories reallocated") +
  guides(fill = "none") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



# BY VALUE ##########
cohorting_datav = cohorting_data %>%
  filter(scenario %in% c(1:21,63)) %>%
  arrange(desc(rel_val)) %>%
  mutate(scenario = factor(scenario, levels = unique(scenario)))

cohorting_matrixv = cohorting_matrix %>%
  filter(variable %in% unique(cohorting_datav$scenario)) %>%
  mutate(variable = factor(variable, levels = levels(cohorting_datav$scenario))) %>%
  mutate(cat = factor(cat, levels = rev(unique(cat)))) %>%
  mutate(value = as.factor(value))

pc = ggplot(cohorting_datav) +
  geom_col(aes(scenario, rel_val), fill="grey60") +
  geom_errorbar(aes(scenario, rel_val, ymin = rel_lower, ymax = rel_upper), width = 0.7) +
  geom_text(aes(scenario, 0.002, label = ifelse(pval>0.05, "n.s.", ""))) +
  scale_y_continuous(breaks = seq(-0.008,0.011,0.002)) +
  theme_bw() +
  labs(x = "" , y = "Relative reduction in cumulative\nincidence per person reallocated") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(t=5,l=32.5,r=5,b=0))


pd = ggplot(cohorting_matrixv) +
  geom_point(aes(variable, cat, fill = value),
             pch = 22, size = 8) +
  scale_fill_manual(values = c("white", col_pal[c(4,1,3,7,6,5)])) +
  theme_bw() +
  labs(y = "Categories reallocated") +
  guides(fill = "none") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

plot_grid(pa, pb, pc, pd, ncol = 1, rel_heights = c(1,0.8,1,0.8), labels = c("a)", "", "b)", ""))

ggsave(here::here("figures", "fig2.png"), width = 8, height = 10)
