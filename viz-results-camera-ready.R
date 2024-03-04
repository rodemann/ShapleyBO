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


shapleyBO_results = shap
shapleyBO_results_cb = cb
baseline_results = base
baseline_mod_results = mod
human_results = hum
baseline_par_results = par

n_exp = 40

init_design_size = 3
budget = 10 + init_design_size +1 # for final fit


#####
# shapley-CB-assisted results
res_frames = list()
utilities_frame = list()
for(i in 1:n_exp){
  res_frames[[i]] = shapleyBO_results_cb[[i]]$opt.path %>% getOptPathY()
  #utilities_frame[[i]] = res_frames[[i]]$y
}  
# ATTENTION only for minimization (no problem here, since we restrict our tests to it)
opt_paths <- lapply(res_frames, function(proposals){
  for (o in 2:length(proposals)) {
    if(proposals[o] > proposals[o - 1])
      proposals[o] = proposals[o - 1]
  }
  proposals
})


optpaths_all = data.frame(do.call(rbind, opt_paths))
# 
# #remove initial design
#optpaths_all = optpaths_all[,-c(1:init_design_size)]
# BO_paths_initial <- BO_paths_fun_config %>% slice_head(n = initial_design_size)
# BO_paths_optim <- BO_paths_fun_config %>% slice_tail(n = nrow(BO_paths_fun_config) - initial_design_size)
#get lower bound/upper bound/mean per iteration:
BO_paths_sd_shap_cb <- apply(optpaths_all, 2, sd)
BO_paths_mop <- apply(optpaths_all, 2, mean)

#mean_utilities_sh = lapply(opt_paths, mean)
#sd_utilities_sh = lapply(opt_paths, sd)
data_shap_cb <- data.frame(
  x = 1:budget,
  y = - BO_paths_mop
)
data_shap_cb = data_shap_cb[1:budget-1,]
BO_paths_sd_shap_cb = BO_paths_sd_shap_cb[1:budget-1]
# # shapley-assisted results
# res_frames = list()
# utilities_frame = list()
# for(i in 1:n_exp){
#   res_frames[[i]] = shapleyBO_results[[i]]$opt.path %>% getOptPathY()
#   #utilities_frame[[i]] = res_frames[[i]]$y
# }  
# # ATTENTION only for minimization (no problem here, since we restrict our tests to it)
# opt_paths <- lapply(res_frames, function(proposals){
#   for (o in 2:length(proposals)) {
#     if(proposals[o] > proposals[o - 1])
#       proposals[o] = proposals[o - 1]
#   }
#   proposals
# })
# 
# 
# optpaths_all = data.frame(do.call(rbind, opt_paths))
# # 
# # #remove initial design
# #optpaths_all = optpaths_all[,-c(1:init_design_size)]
# # BO_paths_initial <- BO_paths_fun_config %>% slice_head(n = initial_design_size)
# # BO_paths_optim <- BO_paths_fun_config %>% slice_tail(n = nrow(BO_paths_fun_config) - initial_design_size)
# #get lower bound/upper bound/mean per iteration:
# BO_paths_sd_shap <- apply(optpaths_all, 2, sd)
# BO_paths_mop <- apply(optpaths_all, 2, mean)
# 
# #mean_utilities_sh = lapply(opt_paths, mean)
# #sd_utilities_sh = lapply(opt_paths, sd)
# data_shap <- data.frame(
#   x = 1:budget,
#   y = - BO_paths_mop
# )


########
# baseline results
res_frames = list()
utilities_frame = list()
for(i in 1:n_exp){
  res_frames[[i]] = baseline_results[[i]]$opt.path %>% getOptPathY()
  #utilities_frame[[i]] = res_frames[[i]]$y
}  
# ATTENTION only for minimization (no problem here, since we restrict our tests to it)
opt_paths <- lapply(res_frames, function(proposals){
  for (o in 2:length(proposals)) {
    if(proposals[o] > proposals[o - 1])
      proposals[o] = proposals[o - 1]
  }
  proposals
})

optpaths_all = data.frame(do.call(rbind, opt_paths))
# 
# #remove initial design
#optpaths_all = optpaths_all[,-c(1:init_design_size)]
# BO_paths_initial <- optpaths_all %>% slice_head(n = initial_design_size)
# BO_paths_optim <- BO_paths_fun_config %>% slice_tail(n = nrow(BO_paths_fun_config) - initial_design_size)
#get lower bound/upper bound/mean per iteration:
BO_paths_sd_base <- apply(optpaths_all, 2, sd)
BO_paths_mop <- apply(optpaths_all, 2, mean)

#BO_paths_mop = append(BO_paths_mop, BO_paths_mop[25])

#mean_utilities_sh = lapply(opt_paths, mean)
#sd_utilities_sh = lapply(opt_paths, sd)
data_base <- data.frame(
  x = 1:budget,
  y = - BO_paths_mop
)
data_base = data_base[1:budget-1,]
BO_paths_sd_base = BO_paths_sd_base[1:budget-1]

########
# human baseline results
res_frames = list()
utilities_frame = list()
for(i in 1:n_exp){
  res_frames[[i]] = human_results[[i]]$opt.path %>% getOptPathY()
  #utilities_frame[[i]] = res_frames[[i]]$y
}  
# ATTENTION only for minimization (no problem here, since we restrict our tests to it)
opt_paths <- lapply(res_frames, function(proposals){
  for (o in 2:length(proposals)) {
    if(proposals[o] > proposals[o - 1])
      proposals[o] = proposals[o - 1]
  }
  proposals
})

optpaths_all = data.frame(do.call(rbind, opt_paths))
# 
# #remove initial design
#optpaths_all = optpaths_all[,-c(1:init_design_size)]
# BO_paths_initial <- optpaths_all %>% slice_head(n = initial_design_size)
# BO_paths_optim <- BO_paths_fun_config %>% slice_tail(n = nrow(BO_paths_fun_config) - initial_design_size)
#get lower bound/upper bound/mean per iteration:
BO_paths_sd_hum <- apply(optpaths_all, 2, sd)
BO_paths_mop <- apply(optpaths_all, 2, mean)

#mean_utilities_sh = lapply(opt_paths, mean)
#sd_utilities_sh = lapply(opt_paths, sd)
data_hum <- data.frame(
  x = 1:budget,
  y = - BO_paths_mop
)
data_hum = data_hum[1:budget-1,]
BO_paths_sd_hum = BO_paths_sd_hum[1:budget-1]


##########
# baseline mod results 
res_frames = list()
utilities_frame = list()
for(i in 1:n_exp){
  res_frames[[i]] = baseline_mod_results[[i]]$opt.path %>% getOptPathY()
  #utilities_frame[[i]] = res_frames[[i]]$y
}  
# ATTENTION only for minimization (no problem here, since we restrict our tests to it)
opt_paths <- lapply(res_frames, function(proposals){
  for (o in 2:length(proposals)) {
    if(proposals[o] > proposals[o - 1])
      proposals[o] = proposals[o - 1]
  }
  proposals
})

optpaths_all = data.frame(do.call(rbind, opt_paths))
# 
# #remove initial design
#optpaths_all = optpaths_all[,-c(1:init_design_size)]
# BO_paths_initial <- optpaths_all %>% slice_head(n = initial_design_size)
# BO_paths_optim <- BO_paths_fun_config %>% slice_tail(n = nrow(BO_paths_fun_config) - initial_design_size)
#get lower bound/upper bound/mean per iteration:
BO_paths_sd_mod <- apply(optpaths_all, 2, sd)
BO_paths_mop <- apply(optpaths_all, 2, mean)

#mean_utilities_sh = lapply(opt_paths, mean)
#sd_utilities_sh = lapply(opt_paths, sd)
data_mod <- data.frame(
  x = 1:budget,
  y = - BO_paths_mop
)
data_mod = data_mod[1:budget-1,]
BO_paths_sd_mod = BO_paths_sd_mod[1:budget-1]

##########
# baseline par results 
res_frames = list()
utilities_frame = list()
for(i in 1:n_exp){
  res_frames[[i]] = baseline_par_results[[i]]$opt.path %>% getOptPathY()
  #utilities_frame[[i]] = res_frames[[i]]$y
}  
# ATTENTION only for minimization (no problem here, since we restrict our tests to it)
opt_paths <- lapply(res_frames, function(proposals){
  for (o in 2:length(proposals)) {
    if(proposals[o] > proposals[o - 1])
      proposals[o] = proposals[o - 1]
  }
  proposals
})

optpaths_all = data.frame(do.call(rbind, opt_paths))
# 
# #remove initial design
#optpaths_all = optpaths_all[,-c(1:init_design_size)]
# BO_paths_initial <- optpaths_all %>% slice_head(n = initial_design_size)
# BO_paths_optim <- BO_paths_fun_config %>% slice_tail(n = nrow(BO_paths_fun_config) - initial_design_size)
#get lower bound/upper bound/mean per iteration:
BO_paths_sd_par <- apply(optpaths_all, 2, sd)
BO_paths_mop <- apply(optpaths_all, 2, mean)


BO_paths_mop = BO_paths_mop[1:budget]

#mean_utilities_sh = lapply(opt_paths, mean)
#sd_utilities_sh = lapply(opt_paths, sd)
data_par <- data.frame(
  x = 1:budget,
  y = - BO_paths_mop
)

data_par = data_par[1:budget-1,]
BO_paths_sd_par = BO_paths_sd_par[1:budget-1]


# Combine the datasets into one dataframe
combined_data <- rbind(#data.frame(Agent = "Shapely-assisted-tree", data_shap),
  data.frame(Agent = "A0: BO", data_base),
  data.frame(Agent = "A3: Venkatesh et al.", data_mod),
  data.frame(Agent = "A1: Human", data_hum),
  data.frame(Agent = "A4: Shap-Team", data_shap_cb),
  data.frame(Agent = "A2: Param-Team", data_par)
  
)

sd_vec = c(BO_paths_sd_base, BO_paths_sd_hum, BO_paths_sd_mod, BO_paths_sd_shap_cb, BO_paths_sd_par)
# Calculate confidence intervals (assuming normal distribution due to CLT)
combined_data$lower <- combined_data$y - 1.96 * sd_vec / sqrt(n_exp)
combined_data$upper <- combined_data$y + 1.96 * sd_vec / sqrt(n_exp)


pal = c("steelblue", "forestgreen","#DB6D00","red3", "magenta")
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
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(title = "",
       x = "Iteration",
       y = "Best Incumbent Utility") +
  ggthemes::theme_economist(base_family = "verdana") +
  theme(legend.position = "bottom", legend.key.size = unit(1.2, 'cm'),
        legend.box.spacing = unit(0.5, 'cm'), 
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 21)) +
  theme(axis.text = element_text(size = 13), axis.title = element_text(size = 18), 
        plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.y = element_text(size = 18, vjust = 2.9)) +
  theme(axis.title.x = element_text(size = 18, vjust = -0.9)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal)


print(plot)






