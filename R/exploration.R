library(nanoparquet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(patchwork)

########################################
### Load inflow_outflow_dataset data ###
########################################

inflow_outflow_dataset <- read_parquet("inflow_outflow_dataset.parquet")

########################
### Data Exploration ###
########################

lower_reservoir_sim_facet_grid <- inflow_outflow_dataset %>%
  mutate(sum_lower_reservoir_inflow = sideflow_cfs + dis_tot_upper_cfs) %>%
  pivot_longer(cols = c(sum_lower_reservoir_inflow, dis_tot_lower_cfs), names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable, sum_lower_reservoir_inflow = "Inflow", dis_tot_lower_cfs = "Outflow")) %>%
  ggplot(., aes(x = dt, y = value, color = sim_id)) +
  geom_line() +
  facet_wrap(~ c(variable)) +
  labs(title = "Lower Reservoir Inflow and Outflow Over Time by Simulation",
       x = "Date",
       y = "cfs") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

lower_reservoir_sim_facet_grid

upper_reservoir_sim_facet_grid <- inflow_outflow_dataset %>%
  pivot_longer(cols = c(inflow_cfs, dis_tot_upper_cfs), names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable, inflow_cfs = "Inflow", dis_tot_upper_cfs = "Outflow")) %>%
  ggplot(., aes(x = dt, y = value, color = sim_id)) +
  geom_line() +
  facet_wrap(~ c(variable)) +
  labs(title = "Upper Reservoir Inflow and Outflow Over Time by Simulation",
       x = "Date",
       y = "cfs") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

upper_reservoir_sim_facet_grid

lower_reservoir_scatter_plot <- inflow_outflow_dataset %>%
  mutate(sum_lower_reservoir_inflow = sideflow_cfs + dis_tot_upper_cfs) %>%
  ggplot(., aes(x = sum_lower_reservoir_inflow, y = dis_tot_lower_cfs)) +
  geom_point(alpha = 0.5) +
  labs(title = "Inflow vs Outflow Lower Reservoir",
       x = "Inflow (cfs)",
       y = "Lower Reservoir Outflow (cfs)") +
  theme_minimal()

lower_reservoir_scatter_plot

upper_reservoir_scatter_plot <- inflow_outflow_dataset %>%
  ggplot(., aes(x = inflow_cfs, y = dis_tot_upper_cfs)) +
  geom_point(alpha = 0.5) +
  labs(title = "Inflow vs Outflow Upper Reservoir",
       x = "Inflow (cfs)",
       y = "Upper Reservoir Outflow (cfs)") +
  theme_minimal()

upper_reservoir_scatter_plot

upper_reservoir_mean <- inflow_outflow_dataset %>%
  pivot_longer(cols = c(inflow_cfs, dis_tot_upper_cfs), names_to = "variable", values_to = "value") %>%
  group_by(dt, variable) %>%
  summarise(
    mean_flow = mean(value),
    sd_flow = sd(value),
    n = sum(value)) %>%
  mutate(
    lower_sd = mean_flow - sd_flow,
    upper_sd = mean_flow + sd_flow,
    variable = recode(variable, "inflow_cfs" = "Inflow", "dis_tot_upper_cfs" = "Outflow")) %>%
  ggplot(., aes(x = dt, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = lower_sd, ymax = upper_sd), alpha = 0.2,color = NA) +
  geom_line(aes(y = mean_flow), linewidth = 0.9) +
  labs(title = "Upper Reservoir Inflow vs. Outflow", subtitle = "Mean flow ± SD", x = "Date", y = "cfs")

upper_reservoir_mean

lower_reservoir_mean <- inflow_outflow_dataset %>%
  mutate(sum_lower_reservoir_inflow = sideflow_cfs + dis_tot_upper_cfs) %>%
  pivot_longer(cols = c(sum_lower_reservoir_inflow, dis_tot_lower_cfs), names_to = "variable", values_to = "value") %>%
  group_by(dt, variable) %>%
  summarise(
    mean_flow = mean(value),
    sd_flow = sd(value),
    n = sum(value)) %>%
  mutate(
    lower_sd = mean_flow - sd_flow,
    upper_sd = mean_flow + sd_flow,
    variable = recode(variable, "sum_lower_reservoir_inflow" = "Inflow", "dis_tot_lower_cfs" = "Outflow")) %>%
  ggplot(., aes(x = dt, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = lower_sd, ymax = upper_sd), alpha = 0.2,color = NA) +
  geom_line(aes(y = mean_flow), linewidth = 0.75) +
  labs(title = "Lower Reservoir Inflow vs. Outflow", subtitle = "Mean flow ± SD", x = "Date", y = "cfs") +
  facet_wrap(~ c(variable))

lower_reservoir_mean

####################################
## Upper Reservoir Ensemble Plots ##
####################################

upper_reservoir_ensemble_plot <- inflow_outflow_dataset %>%
  pivot_longer(cols = c(inflow_cfs, dis_tot_upper_cfs), names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable, inflow_cfs = "Inflow", dis_tot_upper_cfs = "Outflow")) %>%
  group_by(dt, variable) %>%
  summarise(
    median_flow = median(value),
    p10 = quantile(value, 0.10),
    p25 = quantile(value, 0.25),
    p75 = quantile(value, 0.75),
    p90 = quantile(value, 0.90)) %>%
  ggplot(., aes(x = dt, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.2,color = NA) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.4,color = NA) +
  geom_line(aes(y = median_flow), linewidth = 0.75) +
  labs(title = "Ensemble Plot", y = "cfs") +
  xlab(element_blank()) +
  facet_wrap(~ c(variable)) + 
  theme(legend.position = "none")

upper_reservoir_ensemble_plot

upper_reservoir_balance <- inflow_outflow_dataset %>%
  mutate(net_upper_reservoir = inflow_cfs - dis_tot_upper_cfs) %>%
  group_by(dt) %>%
  summarise(
    median_balance = median(net_upper_reservoir),
    p10 = quantile(net_upper_reservoir, 0.10),
    p25 = quantile(net_upper_reservoir, 0.25),
    p75 = quantile(net_upper_reservoir, 0.75),
    p90 = quantile(net_upper_reservoir, 0.90)) %>%
  ggplot(., aes(x = dt)) +
  geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.2,fill = "gray70") +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.4,fill = "gray50") +
  geom_ribbon(aes(ymin = pmin(median_balance, 0), ymax = pmax(median_balance, 0), fill = median_balance >= 0), alpha = 0.5) +
  geom_line(aes(y = median_balance), linewidth = 0.75) +
  scale_fill_manual(
    values = c("TRUE" = "#2f8a13", "FALSE" = "#f32408")) +
  labs(title = "Reservoir Balance", y = "cfs") +
  xlab(element_blank()) + 
  theme(legend.position = "none")

upper_reservoir_balance

combined_plot <- upper_reservoir_ensemble_plot / upper_reservoir_balance +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(title = "Upper Reservoir Inflow vs. Outflow and Net Flow")

combined_plot

####################################
## Lower Reservoir Ensemble Plots ##
####################################

lower_reservoir_ensemble_plot <- inflow_outflow_dataset %>%
  mutate(sum_lower_reservoir_inflow = sideflow_cfs + dis_tot_upper_cfs) %>%
  pivot_longer(cols = c(sum_lower_reservoir_inflow, dis_tot_lower_cfs), names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable, sum_lower_reservoir_inflow = "Inflow", dis_tot_lower_cfs = "Outflow")) %>%
  group_by(dt, variable) %>%
  summarise(
    median_flow = median(value),
    p10 = quantile(value, 0.10),
    p25 = quantile(value, 0.25),
    p75 = quantile(value, 0.75),
    p90 = quantile(value, 0.90)) %>%
  ggplot(., aes(x = dt, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.2,color = NA) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.4,color = NA) +
  geom_line(aes(y = median_flow), linewidth = 0.75) +
  labs(title = "Ensemble Plot", y = "cfs") +
  xlab(element_blank()) +
  facet_wrap(~ c(variable)) + 
  theme(legend.position = "none")

lower_reservoir_ensemble_plot

lower_reservoir_balance <- inflow_outflow_dataset %>%
  mutate(sum_lower_reservoir_inflow = sideflow_cfs + dis_tot_upper_cfs) %>%
  mutate(net_lower_reservoir = sum_lower_reservoir_inflow - dis_tot_lower_cfs) %>%
  group_by(dt) %>%
  summarise(
    median_balance = median(net_lower_reservoir),
    p10 = quantile(net_lower_reservoir, 0.10),
    p25 = quantile(net_lower_reservoir, 0.25),
    p75 = quantile(net_lower_reservoir, 0.75),
    p90 = quantile(net_lower_reservoir, 0.90)) %>%
  ggplot(., aes(x = dt)) +
  geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.2,fill = "gray70") +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.4,fill = "gray50") +
  geom_ribbon(aes(ymin = pmin(median_balance, 0), ymax = pmax(median_balance, 0), fill = median_balance >= 0), alpha = 0.5) +
  geom_line(aes(y = median_balance), linewidth = 0.75) +
  scale_fill_manual(
    values = c("TRUE" = "#2f8a13","FALSE" = "#f32408")) +
  labs(title = "Reservoir Balance", y = "cfs") +
  xlab(element_blank()) + 
  theme(legend.position = "none")

lower_reservoir_balance

combined_plot <- lower_reservoir_ensemble_plot / lower_reservoir_balance +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(title = "Lower Reservoir Inflow vs. Outflow and Net Flow")

combined_plot
