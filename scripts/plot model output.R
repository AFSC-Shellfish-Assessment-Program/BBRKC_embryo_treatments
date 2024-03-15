# plot hindcasts and projections of pH and temp from Darren P.
# "temp" throughout refers to bottom temp!

library(tidyverse)

# set palette
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# set theme
theme_set(theme_bw())

## projections -------------

# load files

file_names <- list.files("./output/cmip6_projections")

# set up vectors of variables, SSPs, and time domains
variables <- c(rep("bottom_pH", 4), rep("bottom_temp", 4), rep("sst", 4))
SSPs <- rep(c("ssp126", "ssp126", "ssp585", "ssp585"), 3)
time <- c(rep(c("2050-2059", "2090-2099"), 6))


# vector of columnames (day and model identity)
col_names <- c("year", "month", "day", "CESM", "GFDL", "MIROC")

# set up combined df to plot

cmip6_plot <- data.frame()

# loop through files and join!

for(i in 1:length(file_names)){

  path <- paste("./output/cmip6_projections/", file_names[i], sep = "")
  temp <- read.csv(path, header = F)
  
  names(temp) <- col_names
  
  temp$variable <- variables[i]
  temp$SSP <- SSPs[i]
  temp$time <- time[i]
  
  # temp <- temp %>%
  #   pivot_longer(cols = c(-`day of year`, -variable, -SSP, -time))
  
  cmip6_plot <- rbind(cmip6_plot, temp)  

}

cmip6_plot <- cmip6_plot %>%
  pivot_longer(cols = c(-year, -month, -day, -variable, -SSP, -time), names_to = "model")


## hindcasts ----------------

# load temp

bottom_temp_hind <- read.csv("./output/hindcasts/temp_bottom5m_week_hind_2013-2022_interp_BB.csv", header = F)

names(bottom_temp_hind) <- c("year", "month", "day", "bottom_temp")

# load pH
bottom_pH_hind <- read.csv("./output/hindcasts/pH_bottom5m_week_hind_2013-2022_interp_BB.csv", header = F)

names(bottom_pH_hind) <- c("year", "month", "day", "bottom_pH")

# load sst
sst_hind <- read.csv("./output/hindcasts/temp_surface5m_week_hind_2013-2022_interp_BB.csv", header = F)

names(sst_hind) <- c("year", "month", "day", "sst")


## compare monthly pH distributions for hindcast and projections averaged by SSP -----

pH_project <- cmip6_plot %>%
  filter(variable == "bottom_pH") %>%
  mutate(month = month(month, label = T)) %>%
  group_by(time, year, month, day, model) %>% # for a given day/month/year/time period/model, average across SSPs
  select(-variable, -year, -day, -SSP)

pH_project <- pH_project[,3:6]  # remove grouping variables by hand

         
bottom_pH_hind <- bottom_pH_hind %>%
  mutate(model = "_hindcast",
         time = "2050-2059",
         month = month(month, label = T)) %>%
  rename(value = bottom_pH) %>%
  select(month, time, model, value)

# and double to include 2090-2099
bottom_pH_hind2 <- bottom_pH_hind %>%
  mutate(time = "2090-2099")

bottom_pH_hind <- rbind(bottom_pH_hind, bottom_pH_hind2)
  
# combine and plot
pH_pdfs <- rbind(pH_project, bottom_pH_hind)

ggplot(filter(pH_pdfs, time == "2050-2059"), aes(value, fill = model)) +
  geom_density(alpha = 0.3, lty = 0) +
  facet_wrap(~month, scales = "free_y") +
  scale_fill_manual(values = cb[c(1,2,4,6)]) +
  xlim(7.5, 8.15) +
  xlab("bottom pH") +
  ggtitle("2050-59 projections; CMIP6 values = SSP126, SSP585 means") 


ggsave("./figs/pH_hindcast_projection_monthly_pdfs_by_model.png", width = 10, height = 6, units = 'in')

# plot the ensemble projection against hindcast

pH_project2 <- cmip6_plot %>%
  filter(variable == "bottom_pH") %>%
  mutate(month = month(month, label = T),
         model = "CMIP6 ensemble") %>%
  group_by(time, year, month, day) %>% # for a given day/month/year/time period, average across SSPs & models
  select(-variable, -year, -day, -SSP)

pH_project2 <- pH_project2[,3:6]  # remove grouping variables by hand

# combine and plot
pH_pdfs <- rbind(pH_project2, bottom_pH_hind)


ggplot(filter(pH_pdfs, time == "2050-2059"), aes(value, fill = model)) +
  geom_density(alpha = 0.3, lty = 0) +
  facet_wrap(~month, scales = "free_y") +
  scale_fill_manual(values = cb[c(1,2)]) +
  xlim(7.5, 8.15) +
  xlab("bottom pH") +
  ggtitle("2050-59 projections; CMIP6 values = SSP126, SSP585 ensemble means") 


ggsave("./figs/pH_hindcast_projection_monthly_pdfs_ensemble.png", width = 10, height = 6, units = 'in')


## compare monthly pH distributions for hindcast and projections averaged by SSP -----

bottom_temp_project <- cmip6_plot %>%
  filter(variable == "bottom_temp") %>%
  mutate(month = month(month, label = T)) %>%
  group_by(time, year, month, day, model) %>% # for a given day/month/year/time period/model, average across SSPs
  select(-variable, -year, -day, -SSP)

bottom_temp_project <- bottom_temp_project[,3:6]  # remove grouping variables by hand


bottom_temp_hind <- bottom_temp_hind %>%
  mutate(model = "_hindcast",
         time = "2050-2059",
         month = month(month, label = T)) %>%
  rename(value = bottom_temp) %>%
  select(month, time, model, value)

# and double to include 2090-2099
bottom_temp_hind2 <- bottom_temp_hind %>%
  mutate(time = "2090-2099")

bottom_temp_hind <- rbind(bottom_temp_hind, bottom_temp_hind2)

# combine and plot
bottom_temp_pdfs <- rbind(bottom_temp_project, bottom_temp_hind)

ggplot(filter(bottom_temp_pdfs, time == "2050-2059"), aes(value, fill = model)) +
  geom_density(alpha = 0.3, lty = 0) +
  facet_wrap(~month, scales = "free_y") +
  scale_fill_manual(values = cb[c(1,2,4,6)]) +
  xlim(-3.5, 11) +
  xlab("bottom temp") +
  geom_vline(xintercept = -1.8, lty = 2) +
  ggtitle("2050-59 projections; CMIP6 values = SSP126, SSP585 means; dashed line = -1.8°") 


ggsave("./figs/bottom_temp_hindcast_projection_monthly_pdfs_by_model.png", width = 10, height = 6, units = 'in')

# plot the ensemble projection against hindcast

bottom_temp_project2 <- cmip6_plot %>%
  filter(variable == "bottom_temp") %>%
  mutate(month = month(month, label = T),
         model = "CMIP6 ensemble") %>%
  group_by(time, year, month, day) %>% # for a given day/month/year/time period, average across SSPs & models
  select(-variable, -year, -day, -SSP)

bottom_temp_project2 <- bottom_temp_project2[,3:6]  # remove grouping variables by hand

# combine and plot
bottom_temp_pdfs <- rbind(bottom_temp_project2, bottom_temp_hind)


ggplot(filter(bottom_temp_pdfs, time == "2050-2059"), aes(value, fill = model)) +
  geom_density(alpha = 0.3, lty = 0) +
  facet_wrap(~month, scales = "free_y") +
  scale_fill_manual(values = cb[c(1,2)]) +
  xlim(-3.5, 11) +
  xlab("bottom temp") +
  geom_vline(xintercept = -1.8, lty = 2) +
  ggtitle("2050-59 projections; CMIP6 values = SSP126, SSP585 ensemble means; dashed line = -1.8°") 


ggsave("./figs/bottom_temp_hindcast_projection_monthly_pdfs_ensemble.png", width = 10, height = 6, units = 'in')

## compare monthly SST distributions for hindcast and projections averaged by SSP -----

sst_project <- cmip6_plot %>%
  filter(variable == "sst") %>%
  mutate(month = month(month, label = T)) %>%
  group_by(time, year, month, day, model) %>% # for a given day/month/year/time period/model, average across SSPs
  select(-variable, -year, -day, -SSP)

sst_project <- sst_project[,3:6]  # remove grouping variables by hand


sst_hind <- sst_hind %>%
  mutate(model = "_hindcast",
         time = "2050-2059",
         month = month(month, label = T)) %>%
  rename(value = sst) %>%
  select(month, time, model, value)

# and double to include 2090-2099
sst_hind2 <- sst_hind %>%
  mutate(time = "2090-2099")

sst_hind <- rbind(sst_hind, sst_hind2)

# combine and plot
sst_pdfs <- rbind(sst_project, sst_hind)

ggplot(filter(sst_pdfs, time == "2050-2059"), aes(value, fill = model)) +
  geom_density(alpha = 0.3, lty = 0) +
  facet_wrap(~month, scales = "free_y") +
  scale_fill_manual(values = cb[c(1,2,4,6)]) +
  xlim(-5, 20) +
  xlab("sst") +
  ggtitle("2050-59 projections; CMIP6 values = SSP126, SSP585 means; dashed line = -1.8°") +
  geom_vline(xintercept = -1.8, lty = 2)


ggsave("./figs/sst_hindcast_projection_monthly_pdfs_by_model.png", width = 10, height = 6, units = 'in')

# plot the ensemble projection against hindcast

sst_project2 <- cmip6_plot %>%
  filter(variable == "sst") %>%
  mutate(month = month(month, label = T),
         model = "CMIP6 ensemble") %>%
  group_by(time, year, month, day) %>% # for a given day/month/year/time period, average across SSPs & models
  select(-variable, -year, -day, -SSP)

sst_project2 <- sst_project2[,3:6]  # remove grouping variables by hand

# combine and plot
temp_pdfs <- rbind(sst_project2, sst_hind)


ggplot(filter(temp_pdfs, time == "2050-2059"), aes(value, fill = model)) +
  geom_density(alpha = 0.3, lty = 0) +
  facet_wrap(~month, scales = "free_y") +
  scale_fill_manual(values = cb[c(1,2)]) +
  xlim(-5, 20) +
  xlab("sst") +
  ggtitle("2050-59 projections; CMIP6 values = SSP126, SSP585 means; dashed line = -1.8°") +
  geom_vline(xintercept = -1.8, lty = 2)


ggsave("./figs/sst_hindcast_projection_monthly_pdfs_ensemble.png", width = 10, height = 6, units = 'in')
