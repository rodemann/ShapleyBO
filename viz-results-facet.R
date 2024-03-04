# Load ggplot2 library
library(ggplot2)
library(dplyr)
library(mlrMBO)
#load results
# shapleyBO_results = readRDS("~/julian/uq-in-bo/shapley_res_complete-S15-90-20-80-40-2-2-16:53:55-.rds")
# baseline_results <- readRDS("~/julian/uq-in-bo/baseline_res_complete-S15-90-20-80-40-2-2-16:53:55-.rds")
# 
# shapleyBO_results = readRDS("~/julian/uq-in-bo/shapley_res_complete-S15-80-20-80-40-2-2-13:45:59-.rds")
# baseline_results <- readRDS("~/julian/uq-in-bo/baseline_res_complete-S15-80-20-80-40-2-2-13:45:59-.rds")

#shapleyBO_results = readRDS("~/julian/uq-in-bo/shapley_res_complete-S14-80-20-80-40-2-2-18:02:33-.rds")
#baseline_results = readRDS("~/julian/uq-in-bo/baseline_res_complete-S14-80-20-80-40-2-2-18:02:33-.rds")

# shapleyBO_results = readRDS("~/julian/uq-in-bo/shapley_res_complete-S15-90-20-80-40-2-2-15:58:59-.rds")
# baseline_results = readRDS("~/julian/uq-in-bo/baseline_res_complete-S15-90-20-80-40-2-2-15:58:59-.rds")

#shapleyBO_results <- readRDS("~/julian/uq-in-bo/shapley_res_complete-S15-80-20-80-40-2-2-13:57:26-.rds")
#baseline_results <- readRDS("~/julian/uq-in-bo/baseline_res_complete-S15-80-20-80-40-2-2-13:57:26-.rds")

combined_data = list()

d=1
source("make-df-facet.R")


combined_data_facet = data.frame(do.call(rbind, combined_data))


plot <- ggplot(combined_data_facet, aes(x, y, color = Agent)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_wrap(~ subject) + #, scales = "free", ncol = 2) + # Adjust 'ncol' as needed
  scale_color_manual(values = pal) +
  ggthemes::theme_economist(base_family = "verdana") +
  labs(title = "",
       x = "Iteration",
       y = "Best Incumbent Utility") +
  theme(legend.position = "bottom", legend.key.size = unit(1.2, 'cm'),
        legend.box.spacing = unit(0.2, 'cm'),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 21),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 18, vjust = 1.9)) +
  scale_fill_manual(values = pal)

print(plot)



pal = c("steelblue","#DB6D00","red3")
# Create ggplot with geom_lines and confidence intervals
plot <- ggplot(combined_data, aes(x, y, color = Agent)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Agent), alpha = 0.13) +
  labs(title = "",
       x = "Iteration",
       y = "Best Utility") +
  ggthemes::theme_economist(base_family = "verdana") +
  theme(legend.position = "bottom", legend.key.size = unit(1.2, 'cm'),
        legend.box.spacing = unit(0.2, 'cm'), 
        legend.text = element_text(size = 16),
        legend.title = element_text(size=21)) +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=18), plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.y = element_text(size = 18, vjust= 1.9)) + #, margin = margin(t = 20, r = 20, b = 0, l = 0))) +
  scale_color_manual(values=pal) +
  scale_fill_manual(values=pal)

print(plot)
# Assuming 'combined_data' has columns: x, y, Agent, ymin, ymax
# and 'pal' is defined for color and fill aesthetics

plot <- ggplot(combined_data, aes(x, y, color = Agent)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(title = "",
       x = "Iteration",
       y = "Best Incumbent Utility") +
  ggthemes::theme_economist(base_family = "verdana") +
  theme(legend.position = "bottom", legend.key.size = unit(1.2, 'cm'),
        legend.box.spacing = unit(0.2, 'cm'), 
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 21)) +
  theme(axis.text = element_text(size = 13), axis.title = element_text(size = 18), plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.y = element_text(size = 18, vjust = 1.9)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal)


print(plot)






