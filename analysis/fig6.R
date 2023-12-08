
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

CP_results = read.csv(here::here("results", "CPSC_half_transmission_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_results = read.csv(here::here("results", "VCSC_half_transmission_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
CP_pat_results = read.csv(here::here("results", "CPRandPat_half_transmission_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
CP_all_results = read.csv(here::here("results", "CPRandAll_half_transmission_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_pat_results = read.csv(here::here("results", "VCRandPat_half_transmission_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_all_results = read.csv(here::here("results", "VCRandAll_half_transmission_results.csv")) %>%
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
  mutate(group = factor(group, levels = c("Staff", "Patients", "Mixed"))) %>%
  filter(group != "Mixed")

pp=ggplot(compare2_results,
          aes(SC, val, fill = SC)) +
  facet_nested(~inter+group, scales = "free_x", space="free_x") +
  geom_col() +
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_errorbar(aes(ymax = upper, ymin = lower), linewidth = 0.8, width = 0.5) +
  scale_fill_manual(values = col_pal) +
  scale_y_continuous(breaks = seq(-0.05,0.25,0.05)) +
  coord_cartesian(ylim = c(-0.02, 0.25), clip = "off") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  labs(y = "Relative reduction in cumulative incidence", fill="Targeting strategy:", x = "",
       title = "Halved transmission")


CP_results = read.csv(here::here("results", "CPSC_double_transmission_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_results = read.csv(here::here("results", "VCSC_double_transmission_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
CP_pat_results = read.csv(here::here("results", "CPRandPat_double_transmission_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
CP_all_results = read.csv(here::here("results", "CPRandAll_double_transmission_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_pat_results = read.csv(here::here("results", "VCRandPat_double_transmission_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_all_results = read.csv(here::here("results", "VCRandAll_double_transmission_results.csv")) %>%
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
  mutate(group = factor(group, levels = c("Staff", "Patients", "Mixed"))) %>%
  filter(group != "Mixed")

pp2 = ggplot(compare2_results,
             aes(SC, val, fill = SC)) +
  facet_nested(~inter+group, scales = "free_x", space="free_x") +
  geom_col() +
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_errorbar(aes(ymax = upper, ymin = lower), linewidth = 0.8, width = 0.5) +
  scale_fill_manual(values = col_pal) +
  scale_y_continuous(breaks = seq(0,0.35,0.05)) +
  coord_cartesian(ylim = c(-0.02, 0.25), clip = "off") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  labs(y = "Relative reduction in cumulative incidence", fill="Targeting strategy:", x = "",
       title = "Doubled transmission")


CP_results = read.csv(here::here("results", "CPSC_half_duration_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_results = read.csv(here::here("results", "VCSC_half_duration_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
CP_pat_results = read.csv(here::here("results", "CPRandPat_half_duration_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
CP_all_results = read.csv(here::here("results", "CPRandAll_half_duration_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_pat_results = read.csv(here::here("results", "VCRandPat_half_duration_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_all_results = read.csv(here::here("results", "VCRandAll_half_duration_results.csv")) %>%
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
  mutate(group = factor(group, levels = c("Staff", "Patients", "Mixed"))) %>%
  filter(group != "Mixed")

pp3=ggplot(compare2_results,
           aes(SC, val, fill = SC)) +
  facet_nested(~inter+group, scales = "free_x", space="free_x") +
  geom_col() +
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_errorbar(aes(ymax = upper, ymin = lower), linewidth = 0.8, width = 0.5) +
  scale_fill_manual(values = col_pal) +
  scale_y_continuous(breaks = seq(-0.05,0.25,0.05)) +
  coord_cartesian(ylim = c(-0.02, 0.25), clip = "off") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  labs(y = "Relative reduction in cumulative incidence", fill="Targeting strategy:", x = "",
       title = "Halved carriage duration")


CP_results = read.csv(here::here("results", "CPSC_double_duration_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_results = read.csv(here::here("results", "VCSC_double_duration_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
CP_pat_results = read.csv(here::here("results", "CPRandPat_double_duration_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
CP_all_results = read.csv(here::here("results", "CPRandAll_double_duration_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_pat_results = read.csv(here::here("results", "VCRandPat_double_duration_results.csv")) %>%
  select(val, lower, upper, group, inter, scenario)
VC_all_results = read.csv(here::here("results", "VCRandAll_double_duration_results.csv")) %>%
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
  mutate(group = factor(group, levels = c("Staff", "Patients", "Mixed"))) %>%
  filter(group != "Mixed")

pp4 = ggplot(compare2_results,
             aes(SC, val, fill = SC)) +
  facet_nested(~inter+group, scales = "free_x", space="free_x") +
  geom_col() +
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_errorbar(aes(ymax = upper, ymin = lower), linewidth = 0.8, width = 0.5) +
  scale_fill_manual(values = col_pal) +
  scale_y_continuous(breaks = seq(0,0.35,0.05)) +
  coord_cartesian(ylim = c(-0.02, 0.25), clip = "off") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12)) +
  labs(y = "Relative reduction in cumulative incidence", fill="Targeting strategy:", x = "",
       title = "Doubled carriage duration")


data_baseline = read.csv(here::here("results", "baseline_results.csv"))
data_half_tr = read.csv(here::here("results", "baseline_half_transmission_results.csv"))
data_double_tr = read.csv(here::here("results", "baseline_double_transmission_results.csv"))
data_half_dur = read.csv(here::here("results", "baseline_half_duration_results.csv"))
data_double_dur = read.csv(here::here("results", "baseline_double_duration_results.csv"))


pp5 = data.frame(scenario = c("Estimated values", "Halved transmission", "Doubled transmission",
                              "Halved duration", "Doubled duration"),
                 mean = c(mean(data_baseline$acq_tot),
                          mean(data_half_tr$acq_tot),
                          mean(data_double_tr$acq_tot),
                          mean(data_half_dur$acq_tot),
                          mean(data_double_dur$acq_tot)),
                 sd = c(sd(data_baseline$acq_tot),
                        sd(data_half_tr$acq_tot),
                        sd(data_double_tr$acq_tot),
                        sd(data_half_dur$acq_tot),
                        sd(data_double_dur$acq_tot))) %>%
  ggplot() +
  geom_pointrange(aes(x = factor(scenario, levels = unique(scenario)), y = mean, ymin = mean-sd, ymax = mean+sd)) +
  theme_bw() +
  labs(x = "", y = "Cumulative incidence") +
  theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=12))

plot_grid(plot_grid(pp + theme(legend.position = "none"),
                    pp2 + theme(legend.position = "none"),
                    pp3 + theme(legend.position = "none"),
                    pp4 + theme(legend.position = "none"),
                    ncol = 2,
                    labels = c("a)", "b)", "c)", "d)"), hjust = 0),
          plot_grid(pp5,
                    get_legend(pp),
                    ncol = 1, rel_heights = c(1, 0.58), labels = c("e)", ""), hjust = 0),
          ncol = 2, rel_widths = c(1,0.4))

ggsave(here::here("figures", "fig6.png"), height = 9, width = 13)
