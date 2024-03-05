# plot hindcasts and projections of pH and temp from Darren P.

library(tidyverse)

# set palette
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# set theme
theme_set(theme_bw())

## projections -------------

# load files

file_names <- list.files("./output/cmip6_projections")

# set up vectors of variables, SSPs, and time domains

variables <- c(rep("pH", 4), rep("temp", 4))
SSPs <- rep(c("ssp126", "ssp126", "ssp585", "ssp585"), 2)
time <- c(rep(c("2050-2059", "2090-2099"), 4))


# vector of columnames (day and model identity)
col_names <- c("day of year", "CESM", "GFDL", "MIROC")

# set up combined df to plot

cmip6_plot <- output.frame()

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
  pivot_longer(cols = c(-`day of year`, -variable, -SSP, -time), names_to = "model")

# pH plot
ggplot(filter(cmip6_plot, variable == "pH"), aes(`day of year`, value, color = model)) +
  geom_line() +
  facet_grid(time ~ SSP) +
  scale_color_manual(values = cb[c(2,4,6)]) +
  ylab("pH")

ggsave("./figs/ph_projections.png", width = 6, height = 4, units = 'in')


# temp plot
ggplot(filter(cmip6_plot, variable == "temp"), aes(`day of year`, value, color = model)) +
  geom_line() +
  facet_grid(time ~ SSP) +
  scale_color_manual(values = cb[c(2,4,6)]) +
  ylab("temp")

ggsave("./figs/temp_projections.png", width = 6, height = 4, units = 'in')


# compare SSPs for model/time period comparisons
cmip6_temp <- cmip6_plot %>%
  filter(variable == "temp") %>%
  select(-variable) %>%
  pivot_wider(names_from = SSP, values_from = value) 

# plot
ggplot(cmip6_temp, aes(ssp126, ssp585, color = model)) +
  geom_point() +
  facet_wrap(~time) +
  scale_color_manual(values = cb[c(2,4,6)]) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Black line is 1:1")

ggsave("./figs/temp_ssp126_vs_ssp585.png", width = 8, height = 4, units = 'in')

## hindcasts ----------------

# load and plot temp

temp_hind <- read.csv("./output/hindcasts/temp_bottom5m_mon_hind_2013-2022_BB.csv", header = F)

names(temp_hind) <- c("year", "month", "temp")

ggplot(temp_hind, aes(month, temp)) +
  geom_line() +
  geom_point() +
  facet_wrap(~year) +
  scale_x_continuous(breaks = 1:12)

ggsave("./figs/temp_hindcasts.png", width = 7.5, height = 6, units = 'in')

# load and plot pH
pH_hind <- read.csv("./output/hindcasts/pH_bottom5m_mon_hind_2013-2022_BB.csv", header = F)

names(pH_hind) <- c("year", "month", "pH")

ggplot(pH_hind, aes(month, pH)) +
  geom_line() +
  geom_point() +
  facet_wrap(~year) +
  scale_x_continuous(breaks = 1:12)

ggsave("./figs/pH_hindcasts.png", width = 7.5, height = 6, units = 'in')
