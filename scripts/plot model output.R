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


# compare time periods for model/SSP comparisons
cmip6_temp <- cmip6_plot %>%
  filter(variable == "temp") %>%
  select(-variable) %>%
  pivot_wider(names_from = time, values_from = value) 

# plot
ggplot(cmip6_temp, aes(`2050-2059`, `2090-2099`, color = model)) +
  geom_point() +
  facet_wrap(~SSP) +
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

## compare monthly pH distributions for hindcast and projections -----

pH_project <- cmip6_plot %>%
  filter(variable == "pH",
         time == "2050-2059") %>%
  mutate(doy = round(`day of year`)) %>% 
  group_by(model, doy) %>%
  summarise(pH = mean(value)) %>%
  mutate(doy = ymd("2050-01-01") + days(doy - 1),
         month = month(doy, label = T),
         group = "CMIP6 2050-59") %>%
  select(pH, month, group)

# drop 'model' manually
pH_project <- pH_project[,2:4]


pH_hind <- pH_hind %>%
  mutate(group = "hindcast",
         month = month(month, label = T)) %>%
  select(pH, month, group)
  
# combine and plot
pH_pdfs <- rbind(pH_project, pH_hind)

# and get median values by group, month to plot
pH_medians <- pH_pdfs %>%
  group_by(group, month) %>%
  summarise(median = median(pH))

# save monthly hindcast medians for experimental setup
pH_hindcast_medians <- pH_medians %>%
  filter(group == "hindcast") 

write.csv(pH_hindcast_medians[,2:3], "./summaries/pH_hindcast_monthly_medians.csv", row.names = F)


pH_pdfs <- left_join(pH_pdfs, pH_medians)

ggplot(pH_pdfs, aes(pH, fill = group)) +
  geom_density(alpha = 0.3, lty = 0) +
  facet_wrap(~month, scales = "free_y") +
  scale_fill_manual(values = cb[c(2,6)]) +
  xlim(7.65, 8.07) +
  ggtitle("CMIP6 values = SSP126, SSP585 means; dashed lines = medians") +
  geom_vline(aes(xintercept = median, color = group), lty = 2) +
  scale_color_manual(values = cb[c(2,6)])


ggsave("./figs/pH_hindcast_projection_monthly_pdfs.png", width = 10, height = 6, units = 'in')

## now, advance the hindcast pH climatology by the difference in median between hindcast and projections

# first, get difference in medians  
pH_median_diff <- pH_medians %>%
  pivot_wider(names_from = group, values_from = median) %>%
  mutate(median_diff = `CMIP6 2050-59` - hindcast)

# now, project entire hindcast climatology by this amount
pH_projected_10_percentile <- pH_pdfs %>%
  filter(group == "hindcast") %>%
  left_join(., pH_median_diff) %>%
  mutate(pH_projected = pH + median_diff) %>%
  group_by(month) %>%
  summarise(tenth_percentile = quantile(pH_projected, 0.1))

# save for setting up experimental conditions  
write.csv(pH_projected_10_percentile, "./summaries/pH_projected_10th_percentile_2050-59_SSP126_SSP585_mean.csv", row.names = F)

## compare monthly temp distributions for hindcast and projections -----

temp_project <- cmip6_plot %>%
  filter(variable == "temp",
         time == "2050-2059") %>%
  mutate(doy = round(`day of year`)) %>% 
  group_by(model, doy) %>%
  summarise(temp = mean(value)) %>%
  mutate(doy = ymd("2050-01-01") + days(doy - 1),
         month = month(doy, label = T),
         group = "CMIP6 2050-59") %>%
  select(temp, month, group)

# drop 'model' manually
temp_project <- temp_project[,2:4]

temp_hind <- temp_hind %>%
  mutate(group = "hindcast",
         month = month(month, label = T)) %>%
  select(temp, month, group)

# combine and plot
temp_pdfs <- rbind(temp_project, temp_hind)

# and get median values by group, month to plot
temp_medians <- temp_pdfs %>%
  group_by(group, month) %>%
  summarise(median = median(temp))

# save monthly hindcast medians for experimental setup
temp_hindcast_medians <- temp_medians %>%
  filter(group == "hindcast") 

write.csv(temp_hindcast_medians[,2:3], "./summaries/temp_hindcast_monthly_medians.csv", row.names = F)

temp_pdfs <- left_join(temp_pdfs, temp_medians)

ggplot(temp_pdfs, aes(temp, fill = group)) +
  geom_density(alpha = 0.3, lty = 0) +
  facet_wrap(~month, scales = "free_y") +
  scale_fill_manual(values = cb[c(2,6)]) +
  xlim(-1, 8) +
  ggtitle("CMIP6 values = SSP126, SSP585 means; dashed lines = medians") +
  geom_vline(aes(xintercept = median, color = group), lty = 2) +
  scale_color_manual(values = cb[c(2,6)])


ggsave("./figs/temp_hindcast_projection_monthly_pdfs.png", width = 10, height = 6, units = 'in')

## now, advance the hindcast temp climatology by the difference in median between hindcast and projections

# first, get difference in medians  
temp_median_diff <- temp_medians %>%
  pivot_wider(names_from = group, values_from = median) %>%
  mutate(median_diff = `CMIP6 2050-59` - hindcast)

# now, project entire hindcast climatology by this amount
temp_projected_90_percentile <- temp_pdfs %>%
  filter(group == "hindcast") %>%
  left_join(., temp_median_diff) %>%
  mutate(temp_projected = temp + median_diff) %>%
  group_by(month) %>%
  summarise(ninetieth_percentile = quantile(temp_projected, 0.9))

# save for setting up experimental conditions  
write.csv(temp_projected_90_percentile, "./summaries/temp_projected_90th_percentile_2050-59_SSP126_SSP585_mean.csv", row.names = F)
