
library(dplyr)
library(ggplot2)
library(cowplot)
library(lubridate)

data_prel = read.csv2(here::here("data", "toy_prlvt_all.csv"))

data_prel_m = data_prel %>%
  filter(origin == "Nasal") %>%
  mutate(type = substr(calc_ident, 1, 2)) %>%
  group_by(date_prl, type) %>%
  summarise(tot=n(),
            pos=sum(species=="aureus")) %>%
  ungroup %>%
  mutate(week_day = as.character(wday(as_date(date_prl)))) %>%
  mutate(week_day = replace(week_day, week_day=="2", "Monday"),
         week_day = replace(week_day, week_day=="3", "Tuesday"),
         week_day = replace(week_day, week_day=="4", "Wednesday"),
         week_day = replace(week_day, week_day=="5", "Thursday")) %>%
  mutate(type = replace(type, type=="PA", "Patients"),
         type = replace(type, type=="PE", "Staff"))

data_prel_summary = data_prel_m %>%
  group_by(week_day, type) %>%
  summarise(mean = mean(tot), sd = sd(tot))

pb=ggplot(data_prel_m) +
  geom_density(aes(tot, fill = "Observed"), alpha=0.5) +
  geom_density(aes(rnorm(1000, data_prel_summary$mean[1], data_prel_summary$sd[1]),
                   fill="Simulated"),
                 data = data.frame(type=rep(data_prel_summary$type[1],1000),
                                   week_day=rep(data_prel_summary$week_day[1],1000)), alpha=0.5) +
  geom_density(aes(rnorm(1000, data_prel_summary$mean[2], data_prel_summary$sd[2]),
                   fill="Simulated"),
               data = data.frame(type=rep(data_prel_summary$type[2],1000),
                                 week_day=rep(data_prel_summary$week_day[2],1000)), alpha=0.5) +
  geom_density(aes(rnorm(1000, data_prel_summary$mean[3], data_prel_summary$sd[3]),
                   fill="Simulated"),
               data = data.frame(type=rep(data_prel_summary$type[3],1000),
                                 week_day=rep(data_prel_summary$week_day[3],1000)), alpha=0.5) +
  geom_density(aes(rnorm(1000, data_prel_summary$mean[4], data_prel_summary$sd[4]),
                   fill="Simulated"),
               data = data.frame(type=rep(data_prel_summary$type[4],1000),
                                 week_day=rep(data_prel_summary$week_day[4],1000)), alpha=0.5) +
  geom_density(aes(rnorm(1000, data_prel_summary$mean[5], data_prel_summary$sd[5]),
                   fill="Simulated"),
               data = data.frame(type=rep(data_prel_summary$type[5],1000),
                                 week_day=rep(data_prel_summary$week_day[5],1000)), alpha=0.5) +
  geom_density(aes(rnorm(1000, data_prel_summary$mean[6], data_prel_summary$sd[6]),
                   fill="Simulated"),
               data = data.frame(type=rep(data_prel_summary$type[6],1000),
                                 week_day=rep(data_prel_summary$week_day[6],1000)), alpha=0.5) +
  geom_density(aes(rnorm(1000, data_prel_summary$mean[7], data_prel_summary$sd[7]),
                   fill="Simulated"),
               data = data.frame(type=rep(data_prel_summary$type[7],1000),
                                 week_day=rep(data_prel_summary$week_day[7],1000)), alpha=0.5) +
  geom_density(aes(rnorm(1000, data_prel_summary$mean[8], data_prel_summary$sd[8]),
                   fill="Simulated"),
               data = data.frame(type=rep(data_prel_summary$type[8],1000),
                                 week_day=rep(data_prel_summary$week_day[8],1000)), alpha=0.5) +
  coord_cartesian(xlim=c(0,100)) +
  scale_y_continuous(breaks=seq(0,0.08,0.02)) +
  facet_grid(rows=vars(week_day), cols=vars(type)) +
  theme_bw() +
  labs(x = "Number of swabs", y = "Density", fill = "Distribution:")

# ggplot(data_prel_m) +
#   geom_line(aes(date_prl, pos/tot)) +
#   facet_wrap(~type) +
#   theme_bw()



data_dur = read.csv2(here::here("data", "resultParameters.csv"),header = F)
dur_PA = as.numeric(c(unlist(data_dur[153,4:5]), unlist(data_dur[154:175,1:5])))
dur_PE = as.numeric(c(unlist(data_dur[176,4:5]), unlist(data_dur[177:196,1:5])))
dur_PA = dur_PA[!is.na(dur_PA)]
dur_PE = dur_PE[!is.na(dur_PE)]

dur_PA_log = rlnorm(5000, log(28.004464), log(sqrt(755.011241)))
dur_PA_log = dur_PA_log[dur_PA_log <= 84]
dur_PE_log = rlnorm(5000, log(17.702970), log(sqrt(270.585891)))
dur_PE_log = dur_PE_log[dur_PE_log <= 84]
dur_PA_n = rnorm(5000, 28.004464, sqrt(755.011241))
dur_PA_n = dur_PA_n[dur_PA_n > 0]
dur_PE_n = rnorm(5000, 17.702970, sqrt(270.585891))
dur_PE_n = dur_PE_n[dur_PE_n > 0]


pa=ggplot() +
  geom_density(aes(dur_PA, fill = "Observed"), data = data.frame(type=rep("Patients",length(dur_PA))), alpha=0.5) +
  geom_density(aes(dur_PA_log, fill="Simulated"),
               data = data.frame(type=rep("Patients", length(dur_PA_log))), alpha=0.5) +
  geom_density(aes(dur_PE, fill = "Observed"), data = data.frame(type=rep("Staff",length(dur_PE))), alpha=0.5) +
  geom_density(aes(dur_PE_log, fill="Simulated"),
               data = data.frame(type=rep("Staff",length(dur_PE_log))), alpha=0.5) +
  # geom_density(aes(dur_PA_n, fill="Simulated N"),
  #              data = data.frame(type=rep("PA",length(dur_PA_n))), alpha=0.5) +
  # geom_density(aes(dur_PE_n, fill="Simulated N"),
  #              data = data.frame(type=rep("PE",length(dur_PE_n))), alpha=0.5) +
  coord_cartesian(xlim=c(0,81)) +
  facet_wrap(~type) +
  theme_bw() +
  labs(x="Colonisation duration", y="Density", fill="Distribution:")

plot_grid(pa,pb+guides(fill="none"),ncol=1,labels=c("a)", "b)"),hjust=0,rel_heights = c(1,1.2))

ggsave(here::here("figures", "suppfig9.png"), width = 13, height = 9)
