
shapleyBO_results = shap
baseline_results = base
baseline_mod_results = mod

n_exp = 40

init_design_size = 3
budget = 20 + init_design_size
# shapley-assisted results
res_frames = list()
utilities_frame = list()
for(i in 1:n_exp){
  res_frames[[i]] = shapleyBO_results[[i]]$opt.path %>% getOptPathY()
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
BO_paths_sd_shap <- apply(optpaths_all, 2, sd)
BO_paths_mop <- apply(optpaths_all, 2, mean)

#mean_utilities_sh = lapply(opt_paths, mean)
#sd_utilities_sh = lapply(opt_paths, sd)
data1 <- data.frame(
  x = 1:budget,
  y = - BO_paths_mop
)


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

#mean_utilities_sh = lapply(opt_paths, mean)
#sd_utilities_sh = lapply(opt_paths, sd)
data2 <- data.frame(
  x = 1:budget,
  y = - BO_paths_mop
)



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
BO_paths_sd_base <- apply(optpaths_all, 2, sd)
BO_paths_mop <- apply(optpaths_all, 2, mean)

#mean_utilities_sh = lapply(opt_paths, mean)
#sd_utilities_sh = lapply(opt_paths, sd)
data3 <- data.frame(
  x = 1:budget,
  y = - BO_paths_mop
)


# Combine the datasets into one dataframe

combined_data[[d]] <- rbind(data.frame(Agent = "Shapely-assisted", data1),
                         data.frame(Agent = "Baseline", data2),
                         data.frame(Agent = "Baseline-mod", data3)
)


sd_vec = append(BO_paths_sd_shap, BO_paths_sd_base)
# Calculate confidence intervals (assuming normal distribution due to CLT)
combined_data[[d]]$lower <- combined_data[[d]]$y - 1.96 * sd_vec / sqrt(n_exp)
combined_data[[d]]$upper <- combined_data[[d]]$y + 1.96 * sd_vec / sqrt(n_exp)

combined_data[[d]]$subject = paste("Subject", as.character(d))
