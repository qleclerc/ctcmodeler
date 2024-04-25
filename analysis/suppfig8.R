
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(cowplot)
library(reshape2)
library(RColorBrewer)
library(ggh4x)
library(ggtext)

col_pal = c(brewer.pal(4, "Set1")[-3], "grey30")

CP_results = read.csv(here::here("results", "CPSC_60_results.csv")) %>%
  select(val, group, inter, scenario)
VC_results = read.csv(here::here("results", "VCSC_60_results.csv")) %>%
  select(val, group, inter, scenario)
CP_pat_results = read.csv(here::here("results", "CPRandPat_60_results.csv")) %>%
  select(val, group, inter, scenario)
CP_all_results = read.csv(here::here("results", "CPRandAll_60_results.csv")) %>%
  select(val, group, inter, scenario)
VC_pat_results = read.csv(here::here("results", "VCRandPat_60_results.csv")) %>%
  select(val, group, inter, scenario)
VC_all_results = read.csv(here::here("results", "VCRandAll_60_results.csv")) %>%
  select(val, group, inter, scenario)


compare2_results = rbind(CP_pat_results, CP_all_results,
                         VC_pat_results, VC_all_results,
                         CP_results, VC_results) %>%
  mutate(SC = sapply(group, FUN = function(x) unlist(strsplit(x, "-"))[2])) %>%
  mutate(SC = replace(SC, is.na(SC), "Random")) %>%
  mutate(SC = replace(SC, SC == "Both", "Mixed SC")) %>%
  mutate(SC = replace(SC, SC == "Contact", "Frequency SC")) %>%
  mutate(SC = replace(SC, SC == "Duration", "Duration SC")) %>%
  mutate(group = replace(group, grepl("atient", group), "Patients")) %>%
  mutate(group = replace(group, grepl("aff", group), "Staff")) %>%
  mutate(group = replace(group, grepl("oth", group), "Mixed"))


all_res_pat = expand.grid(eff_vac=c(2,4,6,8,10), eff_cp=c(2,4,6,8,10)) %>%
  mutate(group = "Patients", inter="", SC="", val=0)

for(i in 1:nrow(all_res_pat)){
  
  res_i = rbind(compare2_results %>%
                  filter(group == "Patients") %>%
                  filter(inter == "Vaccination") %>%
                  filter(scenario == all_res_pat$eff_vac[i]),
                compare2_results %>%
                  filter(group == "Patients") %>%
                  filter(inter == "Contact precaution") %>%
                  filter(scenario == all_res_pat$eff_cp[i])) %>%
    filter(val == max(val))
  
  all_res_pat$inter[i] = res_i$inter
  all_res_pat$SC[i] = res_i$SC
  all_res_pat$val[i] = res_i$val
  
}

all_res_staff = expand.grid(eff_vac=c(2,4,6,8,10), eff_cp=c(2,4,6,8,10)) %>%
  mutate(group = "Staff", inter="", SC="", val=0)

for(i in 1:nrow(all_res_staff)){
  
  res_i = rbind(compare2_results %>%
                  filter(group == "Staff") %>%
                  filter(inter == "Vaccination") %>%
                  filter(scenario == all_res_staff$eff_vac[i]),
                compare2_results %>%
                  filter(group == "Staff") %>%
                  filter(inter == "Contact precaution") %>%
                  filter(scenario == all_res_staff$eff_cp[i])) %>%
    filter(val == max(val))
  
  all_res_staff$inter[i] = res_i$inter
  all_res_staff$SC[i] = res_i$SC
  all_res_staff$val[i] = res_i$val
  
}

rbind(all_res_pat, all_res_staff) %>%
  mutate(inter = replace(inter, inter=="Vaccination", "V")) %>%
  mutate(inter = replace(inter, inter=="Contact precaution", "CP")) %>%
  ggplot() +
  geom_tile(aes(x = eff_vac, y = eff_cp, fill = SC), colour = "grey", linewidth=0.8) +
  geom_text(aes(x = eff_vac, y = eff_cp, label = inter), fontface = "bold", colour = "white", size=7) +
  geom_text(aes(x = 13.5, y = 5, label = "CP: contact\nprecautions"), data = data.frame(group = "Staff"), size = 4) +
  geom_text(aes(x = 13.8, y = 4, label = "V: vaccination"), data = data.frame(group = "Staff"), size = 4) +
  facet_grid(cols=vars(group)) +
  scale_fill_manual(values = col_pal) +
  coord_cartesian(xlim = c(1,11), ylim=c(1,11), clip="off") +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        strip.text = element_text(size=11),
        legend.text = element_text(size=11),
        legend.title = element_text(size=12),
        legend.margin = margin(-100,0,0,0)) +
  labs(x = "Vaccination fold-reduction", y = "Contact precaution fold-reduction",
       fill = "Supercontactors\nto target:", colour = "Intervention\nto use:") +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  scale_y_continuous(breaks = c(2,4,6,8,10))

ggsave(here::here("figures", "suppfig8.png"), height=5,width=8)



