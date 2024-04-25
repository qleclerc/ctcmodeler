
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

CP_results = read.csv(here::here("results", "CPSC_PVS_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_results = read.csv(here::here("results", "VCSC_PVS_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
CP_pat_results = read.csv(here::here("results", "CPRandPat_PVS_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
CP_all_results = read.csv(here::here("results", "CPRandAll_PVS_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_pat_results = read.csv(here::here("results", "VCRandPat_PVS_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_all_results = read.csv(here::here("results", "VCRandAll_PVS_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)


compare2_results = rbind(CP_pat_results, CP_all_results,
                         VC_pat_results, VC_all_results,
                         CP_results, VC_results) %>%
  filter(scenario == 6) %>%
  mutate(SC = sapply(group, FUN = function(x) unlist(strsplit(x, "-"))[2])) %>%
  mutate(SC = replace(SC, is.na(SC), "Random")) %>%
  mutate(SC = replace(SC, SC == "Both", "Mixed SC")) %>%
  mutate(SC = replace(SC, SC == "Contact", "Frequency SC")) %>%
  mutate(SC = replace(SC, SC == "Duration", "Duration SC")) %>%
  mutate(SC = factor(SC, levels = c("Duration SC", "Frequency SC", "Mixed SC", "Random"))) %>%
  mutate(group = replace(group, grepl("atient", group), "Patients")) %>%
  mutate(group = replace(group, grepl("aff", group), "Staff")) %>%
  mutate(group = replace(group, grepl("oth", group), "Mixed")) %>%
  mutate(group = factor(group, levels = c("Staff", "Patients", "Mixed")))

ggplot(compare2_results,
          aes(SC, val, fill = SC)) +
  facet_nested(~inter+group, scales = "free_x", space="free_x") +
  geom_col() +
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_errorbar(aes(ymax = upper, ymin = lower), linewidth = 0.8, width = 0.5) +
  scale_fill_manual(values = col_pal) +
  scale_y_continuous(breaks = seq(-0.05,0.25,0.05)) +
  coord_cartesian(ylim = c(-0.01, 0.25), clip = "off") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  labs(y = "Relative reduction in cumulative incidence", fill="Targeting strategy:", x = "")


ggsave(here::here("figures", "suppfig7.png"), width = 13, height = 7)
