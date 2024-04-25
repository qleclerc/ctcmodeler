
library(dplyr)
library(reshape2)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(cowplot)

pal = c("darkolivegreen","darkgreen")

distrib_all = read.csv(here::here("results", "validation_contacts.csv"))
distrib_all$date_posix = sapply(distrib_all$date_posix, function(x) if(nchar(x) < 5) paste0("0",x) else x)
all_incidence_m = read.csv(here::here("results", "incidence_validation.csv")) %>%
  mutate(date = as_date(date))
all_incidence_traj = read.csv(here::here("results", "incidence_traj.csv")) %>%
  mutate(date = as_date(date))

pa = ggplot(distrib_all) +
  geom_ribbon(aes(date_posix, ymin = q25, ymax = q75, fill = network, group = interaction(type, network)),
              alpha = 0.3) +
  geom_point(aes(date_posix, med, colour = network)) +
  geom_line(aes(date_posix, med, colour = network, group = interaction(type, network))) +
  scale_colour_discrete(type = pal) +
  scale_fill_discrete(type = pal) +
  scale_y_continuous(breaks = seq(0,170,20)) +
  scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00",
                              "16:00","20:00")) +
  facet_wrap(~type) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))  +
  labs(x = "Hours", y = "Median number of unique contacts", colour = "Network:", fill = "Network:")


# Incidence ###########

pb = ggplot(all_incidence_m) +
  facet_grid(cols = vars(variable)) +
  geom_line(data = all_incidence_m %>% filter(type=="Simulated"),
            aes(date, mean, colour = "Simulated"), linewidth = 1) +
  geom_ribbon(data = all_incidence_m %>% filter(type=="Simulated"),
              aes(date, ymin = pmax(lower,0), ymax = upper, fill = "Simulated"), alpha = 0.3) +
  geom_pointrange(data = all_incidence_m %>% filter(type=="Real"),
                  aes(date, y = mean, ymin = pmax(lower,0), ymax = upper, colour = "Real"), size = 0.5) +
  scale_colour_discrete(type = pal) +
  scale_fill_discrete(type = pal[2]) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.position = "none") +
  labs(x = "Weeks", y = "Weekly MRSA colonisation incidence rate", colour = "Source:", fill = "Source:")

#calc diff between mid
all_incidence_m %>%
  filter(variable == "All") %>%
  select(date, mean, type) %>%
  dcast(date~type, value.var = "mean") %>%
  mutate(diff = Real-Simulated) %>%
  select(diff) %>%
  pull %>%
  mean

plot_grid(pa, pb, ncol=1,
          labels = c("a)", "b)"), hjust = 0)

ggsave(here::here("figures", "fig1.png"), height = 8, width = 8)

