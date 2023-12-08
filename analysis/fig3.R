
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(cowplot)
library(reshape2)
library(RColorBrewer)

col_pal = brewer.pal(7, "Dark2")

cohorting_data = read.csv(here::here("results", "cohorting_results.csv"))
cohorting_data = cohorting_data %>%
  filter(scenario %in% c(1:6, 63)) %>%
  mutate(group = c("Nurses", "Healthcare asst.", "Rehabilitation", "Physicians",
                   "Porters", "Other", "All")) %>%
  mutate(group = factor(group, levels = c("Physicians", "Porters", "Nurses", 
                                          "Rehabilitation", "Other", "Healthcare asst.",
                                          "All"))) %>%
  mutate(inter = "Staff reallocation")

compare_data = read.csv(here::here("results", "compare_results.csv")) %>%
  mutate(group = replace(group, group %in% c(1:5), -1),
         group = replace(group, group %in% c(6:10), -2),
         group = replace(group, group %in% c(11:15), -3),
         group = replace(group, group %in% c(16:20), -4),
         group = replace(group, group %in% c(21:25), -5),
         group = replace(group, group %in% c(26:30), -6)) %>%
  mutate(group = as.character(group)) %>%
  mutate(group = replace(group, group == "-1", "Nurses"),
         group = replace(group, group == "-2", "Rehabilitation"),
         group = replace(group, group == "-3", "Physicians"),
         group = replace(group, group == "-4", "Porters"),
         group = replace(group, group == "-5", "Other"),
         group = replace(group, group == "-6", "Healthcare asst."))
  
  

pb = ggplot(compare_data, aes(scenario, val, colour = group)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_hline(yintercept = cohorting_data$val[2], lty = "dashed", linewidth = 1, colour = col_pal[2]) +
  geom_hline(yintercept = cohorting_data$val[7], lty = "dashed", linewidth = 1, colour = col_pal[1]) +
  geom_hline(yintercept = cohorting_data$val[1], lty = "dashed", linewidth = 1, colour = col_pal[7]) +
  geom_errorbar(aes(ymax = upper, ymin = lower), linewidth = 0.8, width = 0.5) +
  facet_wrap(~inter) +
  scale_x_continuous(breaks = seq(2,10,2)) +
  coord_cartesian(ylim = c(-0.03,0.4)) +
  scale_y_continuous(breaks = seq(-0.1,0.4,0.1)) +
  scale_colour_manual(values = col_pal[c(2,7,5,4,6,3)]) +
  theme_bw() +
  theme(strip.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))+
  labs(x = "Assumed fold reduction in transmission probability",
       y = "Relative reduction in cumulative incidence", colour = "Staff category:")

pa = ggplot(cohorting_data,
            aes(group, val, fill = group)) +
  facet_grid(~inter) +
  geom_col() +
  geom_errorbar(aes(ymax = upper+0.01, ymin = lower-0.01), linewidth = 0.8, width = 0.5) +
  geom_hline(yintercept = 0, lty = "dashed") +
  coord_cartesian(ylim = c(-0.1,0.4)) +
  scale_y_continuous(breaks = seq(-0.1,0.4,0.1)) +
  scale_fill_manual(values = col_pal[c(4,6,7,3,5,2,1)]) +
  theme_bw() + 
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 30, hjust = 1)) +
  labs(x = "Staff category reallocated", y = "Relative reduction in cumulative incidence")


plot_grid(pb+theme(legend.position = "none"),
          get_legend(pa+labs(fill="Staff category:")),
          nrow = 1, rel_widths = c(1,0.25))

ggsave(here::here("figures", "fig3.png"), width = 8, height = 4)
