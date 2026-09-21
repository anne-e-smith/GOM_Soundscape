##############################
#
# Script to summarize diel and weekly vessel presence in the GOM
#
##############################


library(tidyverse)
library(ggpubr)

#### LOAD DATA AND COMBINE NEFSC/AEON ####

NEFSC_hp_all = read.csv("data/Vessels/All_GOM_Vessel_Hourly_Presence_effortCorrected.csv", header = TRUE)
AEON_hp_all = read.csv("data/Vessels/All_AEON_vessel_hourly_presence.csv", header = TRUE)

### convert to local time
#NEFSC
NEFSC_hp_all$datetime_UTC = paste0(NEFSC_hp_all$start_date_ISO," ", NEFSC_hp_all$Begin_Hour, ":00")
NEFSC_hp_all$datetime_UTC = as.POSIXct(NEFSC_hp_all$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
NEFSC_hp_all$datetime_EST = with_tz(NEFSC_hp_all$datetime_UTC, "EST")
#AEON
AEON_hp_all$datetime_UTC = paste0(AEON_hp_all$Date," ", AEON_hp_all$Hour, ":00")
AEON_hp_all$datetime_UTC = as.POSIXct(AEON_hp_all$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
AEON_hp_all$datetime_EST = with_tz(AEON_hp_all$datetime_UTC, "EST")

## simplify for join
NEFSC = NEFSC_hp_all%>%
  select(SITE, datetime_EST, Ves_pres)
colnames(NEFSC) = c("site", "datetime_EST", "ves_pres")

AEON = AEON_hp_all %>%
  select(Site, datetime_EST, ves_pres)
colnames(AEON) = c("site", "datetime_EST", "ves_pres")

GOM_hp = bind_rows(NEFSC, AEON)

## change USTR to GEB name
GOM_hp$site2 = case_when(GOM_hp$site == "LUBEC" ~ "LUBEC", 
                         GOM_hp$site == "MDR" ~ "MDR",
                         GOM_hp$site == "MONHEGAN" ~ "MONHEGAN",
                         GOM_hp$site == "PORTLAND" ~ "PORTLAND",
                         GOM_hp$site == "YORK" ~ "YORK",
                         GOM_hp$site == "SB03" ~ "SB03",
                         GOM_hp$site == "USTR01" ~ "GEB01",
                         GOM_hp$site == "USTR03" ~ "GEB03",
                         GOM_hp$site == "USTR11" ~ "GEB11",
                         GOM_hp$site == "AEON1_NEC" ~ "AEON1_NEC",
                         GOM_hp$site == "AEON2_ECS" ~ "AEON2_ECS",
                         GOM_hp$site == "AEON3_GEB" ~ "AEON3_GEB",
                         GOM_hp$site == "AEON4_JOB" ~ "AEON4_JOB",
                         GOM_hp$site == "AEON5_WIB" ~ "AEON5_WIB")


## add subregion
GOM_hp$region = case_when(GOM_hp$site == "LUBEC" ~ "1. Grand Manan", 
                          GOM_hp$site == "MDR" ~ "3. North Coastal",
                          GOM_hp$site == "MONHEGAN" ~ "3. North Coastal",
                          GOM_hp$site == "PORTLAND" ~ "3. North Coastal",
                          GOM_hp$site == "YORK" ~ "5. South Coastal",
                          GOM_hp$site == "SB03" ~ "5. South Coastal",
                          GOM_hp$site == "USTR01" ~ "4. Offshore",
                          GOM_hp$site == "USTR03" ~ "4. Offshore",
                          GOM_hp$site == "USTR11" ~ "4. Offshore",
                          GOM_hp$site == "AEON1_NEC" ~ "4. Offshore",
                          GOM_hp$site == "AEON2_ECS" ~ "4. Offshore",
                          GOM_hp$site == "AEON3_GEB" ~ "4. Offshore",
                          GOM_hp$site == "AEON4_JOB" ~ "2. Central",
                          GOM_hp$site == "AEON5_WIB" ~ "5. South Coastal")

# add variables for hour, day, month
GOM_hp  = GOM_hp %>%
  mutate(hour_EST = hour(datetime_EST), day_EST = day(datetime_EST), month_EST = month(datetime_EST))

### Summarize within sub-region

# Calculate month summary: N hours present/N deployment hours
month_pres = GOM_hp %>%
  group_by(region, month_EST) %>%
  summarize(nHP = sum(ves_pres, na.rm = TRUE), nDep = n(), nHP_effort = nHP/nDep, percentHP = nHP_effort*100)

# Calculate diel summary: N hours present/N deployment hours
diel_pres = GOM_hp %>%
  group_by(region, hour_EST) %>%
  summarize(nHP = sum(ves_pres, na.rm = TRUE), nDep = n(), nHP_effort = nHP/nDep, percentHP = nHP_effort*100)

###### sorting out color scales
ggplot(aes(x = month_EST, y = percentHP, fill = percentHP), data = month_pres)+ theme_bw()+
  geom_bar(stat = "identity")+
  coord_polar(theta = "x", start = 0)+
  scale_fill_brewer(palette = "YlOrRd")+
  scale_x_continuous(breaks=seq(0,12,1), 
                     minor_breaks = seq(0:12),
                     expand = c(0,0))+
  facet_wrap(vars(region), nrow = 3)+
  xlab("Month")+
  ylab("N hours present/Total deployment hours (%)")+ 
  ylim(c(0,100))+
  theme(text=element_text(size=20),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        strip.text.y = element_text(colour = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  #R turns the proportion values into bins in the legend when I change the title, so I'm removing it instead
  theme(legend.title=element_blank())

#######

### Plot region summaries
# Monthly
vesselsXMonth = ggplot(aes(x = month_EST, y = nHP_effort, fill = nHP_effort), data = month_pres)+ theme_bw()+
  geom_bar(stat = "identity")+
  coord_polar(theta = "x", start = 0)+
  scale_fill_viridis_c(begin = 0, end = 1, limits = c(0,1), 
                       breaks = c(0,0.25,0.50,0.75,1), option = "B")+
  scale_x_continuous(breaks=seq(0,12,1), 
                     minor_breaks = seq(0:12),
                     expand = c(0,0))+
  facet_wrap(vars(region), nrow = 3)+
  xlab("Month")+
  ylab("N hours present / Total deployment hours")+ 
  ylim(c(0,1))+
  theme(text=element_text(size=20),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        strip.text.y = element_text(colour = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  #R turns the proportion values into bins in the legend when I change the title, so I'm removing it instead
  theme(legend.title=element_blank())


# Diel
vesselsXhour = ggplot(aes(x = hour_EST, y = nHP_effort, fill = nHP_effort), data = diel_pres)+ theme_bw()+
  geom_bar(stat = "identity")+
  coord_polar(theta = "x", start = 0)+
  scale_fill_viridis_c(begin = 0, end = 1, limits = c(0,1), 
                       breaks = c(0,0.25,0.5,0.75,1), 
                       option = "B")+
  scale_x_continuous(breaks=seq(0,23,4), 
                     minor_breaks = seq(0:23),
                     expand = c(0,0))+
  scale_y_continuous(limits=c(0,1),
                     breaks=seq(0,1,0.25),
                     expand = c(0, 0))+
  facet_wrap(vars(region), nrow = 3)+
  xlab("Hour (EST)")+
  ylab("N hours present / Total deployment hours")+
  theme(text=element_text(size=20),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        strip.text.y = element_text(colour = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  #R turns the proportion values into bins in the legend when I change the title, so I'm removing it instead
  theme(legend.title=element_blank())


comboPlot <- ggarrange(vesselsXhour, vesselsXMonth, ncol = 2, labels = c("A", "B"))
ggsave(filename= "vesselPlot.png", plot= comboPlot, )

### Summarize within site

# Calculate month summary: N hours present/N deployment hours
month_pres_site = GOM_hp %>%
  group_by(region, site, month_EST) %>%
  summarize(nHP = sum(ves_pres, na.rm = TRUE), nDep = n(), nHP_effort = nHP/nDep, percentHP = nHP_effort*100)


# Calculate diel summary: N hours present/N deployment hours
diel_pres_site = GOM_hp %>%
  group_by(region,site, hour_EST) %>%
  summarize(nHP = sum(ves_pres, na.rm = TRUE), nDep = n(), nHP_effort = nHP/nDep, percentHP = nHP_effort*100)


### Plot by site
ggplot(aes(x = month_EST, y = nHP_effort, fill = nHP_effort), data = month_pres_site)+ theme_bw()+
  geom_bar(stat = "identity")+
  coord_polar(theta = "x", start = 0)+
  scale_fill_viridis_c(begin = 0, end = 1, limits = c(0,1), 
                       breaks = c(0,0.25,0.5,0.75,1), option = "B")+
  scale_x_continuous(breaks=seq(0,12,1), 
                     minor_breaks = seq(0:12),
                     expand = c(0,0))+
  facet_wrap(vars(site), nrow = 4)+
  xlab("Month")+
  ylab("N hours present / Total deployment hours")+ 
  ylim(c(0,1))+
  theme(text=element_text(size=20),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        strip.text.y = element_text(colour = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  #R turns the proportion values into bins in the legend when I change the title, so I'm removing it instead
  theme(legend.title=element_blank())


# Diel
ggplot(aes(x = hour_EST, y = nHP_effort, fill = nHP_effort), data = diel_pres_site)+ theme_bw()+
  geom_bar(stat = "identity")+
  coord_polar(theta = "x", start = 0)+
  scale_fill_viridis_c(begin = 0, end = 1, limits = c(0,1), 
                       breaks = c(0,0.25,0.5,0.75,1), 
                       option = "B")+
  scale_x_continuous(breaks=seq(0,23,4), 
                     minor_breaks = seq(0:23),
                     expand = c(0,0))+
  scale_y_continuous(limits=c(0,1),
                     breaks=seq(0,1,0.25),
                     expand = c(0, 0))+
  facet_wrap(vars(site), nrow = 3)+
  xlab("Hour (EST)")+
  ylab("N hours present / Total deployment hours")+
  theme(text=element_text(size=20),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        strip.text.y = element_text(colour = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  #R turns the proportion values into bins in the legend when I change the title, so I'm removing it instead
  theme(legend.title=element_blank())








