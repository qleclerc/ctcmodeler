
library(dplyr)
library(reshape2)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(cowplot)

pal = c("darkolivegreen","darkgreen")

adm_data = read.csv(here::here("data", "toy_admission.csv"), sep=";") %>%
  select(id, hospitalization, cat)
adm_data$cat[adm_data$cat == ""] = adm_data$hospitalization[adm_data$cat == ""]
adm_data = adm_data[,c(1,3)] %>%
  distinct()
eq_table = openxlsx::read.xlsx(here::here("data", "cat_groupings.xlsx")) %>%
  select(cat, cat_ag)
adm_data = adm_data %>%
  left_join(eq_table, by = "cat") %>%
  mutate(staff = grepl("PE-", id))


# Contacts ###########

contact_files = list.files(here::here("contact", "validation"))
PAPA_contacts = data.frame()
PEPE_contacts = data.frame()
PAPE_contacts = data.frame()

for(f in contact_files){
  
  data = read.csv2(here::here("contact", "validation", f))
  
  distrib_PAPA = data %>%
    mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
    filter(type == "PA-PA") %>%
    mutate(date_posix = as_datetime(date_posix)) %>%
    filter(date_posix >= as_datetime("2009-07-27") & date_posix <= as_datetime("2009-08-24")) %>%
    mutate(date_posix = floor_date(date_posix, "hour")) %>%
    select(-length) %>%
    distinct() %>%
    count(date_posix) %>%
    ungroup() %>%
    mutate(date_posix = hour(date_posix)) %>%
    mutate(type = "Patient-Patient")
  
  PAPA_contacts = rbind(PAPA_contacts, distrib_PAPA)
  
  distrib_PEPE = data %>%
    mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
    filter(type == "PE-PE") %>%
    mutate(date_posix = as_datetime(date_posix)) %>%
    filter(date_posix >= as_datetime("2009-07-27") & date_posix <= as_datetime("2009-08-24")) %>%
    mutate(date_posix = floor_date(date_posix, "hour")) %>%
    select(-length) %>%
    distinct() %>%
    count(date_posix) %>%
    ungroup() %>%
    mutate(date_posix = hour(date_posix)) %>%
    mutate(type = "Staff-Staff")
  
  PEPE_contacts = rbind(PEPE_contacts, distrib_PEPE)
  
  distrib_PAPE = data %>%
    mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
    filter(type == "PA-PE" | type == "PE-PA") %>%
    mutate(date_posix = as_datetime(date_posix)) %>%
    filter(date_posix >= as_datetime("2009-07-27") & date_posix <= as_datetime("2009-08-24")) %>%
    mutate(date_posix = floor_date(date_posix, "hour")) %>%
    select(-length) %>%
    distinct() %>%
    count(date_posix) %>%
    ungroup() %>%
    mutate(date_posix = hour(date_posix)) %>%
    mutate(type = "Staff-Patient")
  
  PAPE_contacts = rbind(PAPE_contacts, distrib_PAPE)
  
}

distrib_simu_bias = rbind(distrib_PAPA, distrib_PEPE, distrib_PAPE) %>%
  group_by(date_posix, type) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(date_posix = paste0(date_posix, ":00")) %>%
  mutate(date_posix = factor(date_posix, levels = unique(date_posix))) %>%
  mutate(network = "Simulated")

data = read.csv2(here::here("contact", "toy_mat_ctc.csv"))

distrib_PAPA = data %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PA-PA") %>%
  mutate(date_posix = as_datetime(date_posix)) %>%
  filter(date_posix >= as_datetime("2009-07-27") & date_posix <= as_datetime("2009-08-24")) %>%
  mutate(date_posix = floor_date(date_posix, "hour")) %>%
  select(-length) %>%
  distinct() %>%
  count(date_posix) %>%
  ungroup() %>%
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Patient-Patient")

distrib_PEPE = data %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PE-PE") %>%
  mutate(date_posix = as_datetime(date_posix)) %>%
  filter(date_posix >= as_datetime("2009-07-27") & date_posix <= as_datetime("2009-08-24")) %>%
  mutate(date_posix = floor_date(date_posix, "hour")) %>%
  select(-length) %>%
  distinct() %>%
  count(date_posix) %>%
  ungroup() %>%
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-Staff")

distrib_PAPE = data %>%
  mutate(type = paste0(substr(from, 1, 2), "-", substr(to, 1, 2))) %>%
  filter(type == "PA-PE" | type == "PE-PA") %>%
  mutate(date_posix = as_datetime(date_posix)) %>%
  filter(date_posix >= as_datetime("2009-07-27") & date_posix <= as_datetime("2009-08-24")) %>%
  mutate(date_posix = floor_date(date_posix, "hour")) %>%
  select(-length) %>%
  distinct() %>%
  count(date_posix) %>%
  ungroup() %>%
  mutate(date_posix = hour(date_posix)) %>%
  group_by(date_posix) %>%
  summarise(med = median(n),
            q25 = quantile(n, 0.25),
            q75 = quantile(n, 0.75)) %>%
  mutate(type = "Staff-Patient")

distrib_real = rbind(distrib_PAPA, distrib_PEPE, distrib_PAPE) %>%
  mutate(date_posix = paste0(date_posix, ":00")) %>%
  mutate(date_posix = factor(date_posix, levels = unique(date_posix))) %>%
  mutate(network = "Real")

distrib_all = rbind(distrib_real, distrib_simu_bias)

pa = ggplot(distrib_all) +
  geom_ribbon(aes(date_posix, ymin = q25, ymax = q75, fill = network, group = interaction(type, network)),
              alpha = 0.3) +
  geom_point(aes(date_posix, med, colour = network)) +
  geom_line(aes(date_posix, med, colour = network, group = interaction(type, network))) +
  scale_colour_discrete(type = pal) +
  scale_fill_discrete(type = pal) +
  scale_y_continuous(breaks = seq(0,170,20)) +
  scale_x_discrete(breaks = c("0:00", "4:00", "8:00", "12:00",
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

incidence_files = list.files(here::here("acquisition"))

real_incidence = read.csv2(here::here("acquisition", incidence_files[grepl("real", incidence_files)])) %>%
  select(Date,
         Bacteria_Staphylococcaceae_Staphylococcus_aureus__SARM_.nbPA,
         Bacteria_Staphylococcaceae_Staphylococcus_aureus__SARM_.nbPE,
         Bacteria_Staphylococcaceae_Staphylococcus_aureus__SARM_.prelPA,
         Bacteria_Staphylococcaceae_Staphylococcus_aureus__SARM_.prelPE)

colnames(real_incidence) = c("date", "nbPA", "nbPE", "prelPA", "prelPE")

real_incidence = real_incidence %>%
  mutate(date = as_date(date)) %>%
  mutate(across(nbPA:prelPE, as.numeric)) %>%
  mutate(nb = nbPA+nbPE, prel = prelPA+prelPE)

real_incidence = rbind(real_incidence %>%
                         select(date, nbPA, prelPA) %>%
                         rename(nb=nbPA, prel=prelPA) %>%
                         mutate(variable = "Patients"),
                       real_incidence %>%
                         select(date, nbPE, prelPE) %>%
                         rename(nb=nbPE, prel=prelPE) %>%
                         mutate(variable = "Staff"),
                       real_incidence %>%
                         select(date, nb, prel) %>%
                         mutate(variable = "All")) %>%
  mutate(mean = nb/prel) %>%
  mutate(mean = replace(mean, is.nan(mean), 0)) %>%
  mutate(upper = mean+qnorm(0.95)*sqrt(nb/(prel^2)),
         lower = mean-qnorm(0.95)*sqrt(nb/(prel^2))) %>%
  mutate(lower = replace(lower, is.nan(lower), 0),
         upper = replace(upper, is.nan(upper), 0)) %>%
  select(date, variable, mean, upper, lower)


all_incidence = data.frame()

for(d in incidence_files[-grepl("real", incidence_files)]){
  
  data = read.csv2(here::here("acquisition", d)) %>%
    select(Date,
           Bacteria_Staphylococcaceae_Staphylococcus_aureus__SARM_.ALL,
           Bacteria_Staphylococcaceae_Staphylococcus_aureus__SARM_.PA,
           Bacteria_Staphylococcaceae_Staphylococcus_aureus__SARM_.PE)
  all_incidence = rbind(all_incidence, data)
  
}

colnames(all_incidence) = c("date", "All", "Patients", "Staff")

all_incidence = all_incidence %>%
  mutate(date = as_date(date)) %>%
  melt(id.vars = "date") %>%
  mutate(value = as.numeric(value)) %>%
  group_by(date, variable) %>%
  summarise(mean = median(value),
            upper = quantile(value, 0.025),
            lower = quantile(value, 0.975)) %>%
  mutate(type = "Simulated") %>%
  rbind(real_incidence %>% mutate(type = "Real"))

pb = ggplot(all_incidence) +
  facet_grid(cols = vars(variable)) +
  geom_line(data = all_incidence %>% filter(type=="Simulated"),
            aes(date, mean, colour = "Simulated"), linewidth = 1) +
  geom_ribbon(data = all_incidence %>% filter(type=="Simulated"),
              aes(date, ymin = pmax(lower,0), ymax = upper, fill = "Simulated"), alpha = 0.3) +
  geom_pointrange(data = all_incidence %>% filter(type=="Real"),
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

plot_grid(pa, pb, ncol=1,
          labels = c("a)", "b)"), hjust = 0)

ggsave(here::here("figures", "fig1.png"), height = 8, width = 8)

