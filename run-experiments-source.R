set.seed(23424652)

features = c("low","lif")
start_low <- 1
start_lif <- 1
end_low = 100
end_lif = 100
griddf <- expand.grid(low = seq(from = start_low, to = end_low, by = 1),
                      lif = seq(from = start_lif, to = end_lif, by = 1) )
sub_griddf = griddf[sample.int(100*100, size = 70),] %>% as.matrix
#sub_griddf = griddf %>% as.matrix ## no subsampling

get_util = function(x) data[x[1],x[2]] %>% as.numeric
utilities = apply(sub_griddf, 1, get_util) #%>% unlist
hyper_data = cbind(sub_griddf,utilities) %>% as.data.frame()
# #learn (hyper)surrogate
#lrn_hyper = makeLearner("regr.km", predict.type = "se", covtype = "gauss", control = list(trace = FALSE))
lrn_hyper = makeLearner("regr.randomForest", predict.type = "se")
#lrn_hyper = makeLearner("regr.gbm", par.vals = list(n.trees = 500, interaction.depth = 3))
#lrn_hyper = makeLearner("regr.glmboost")
lrn_hyper = makeLearner("regr.km",control = list(trace = FALSE))

# Nuggets = 1e-8
# lrn_hyper = setHyperPars(learner = lrn_hyper, nugget=Nuggets)
hyper_estim = makeRegrTask(data = hyper_data, target = "utilities")
hyper_model = train(lrn_hyper, hyper_estim)

# design <- generateDesign(n = 200, par.set = parameter_set, fun = lhs::randomLHS)
# 
# newdata = design
# names(newdata) = features
# predict(hyper_model, newdata = newdata) %>% getPredictionResponse()


var_function = function(x) 0.001  #0.001x # 2*abs(x) optional: make heteroscedastic

utility = function(x){
  #browser()
  x = matrix(x, nrow = 1)
  sd = var_function(x) %>% sum %>% sqrt 
  newdata = as.data.frame(x)
  names(newdata) = features
  mean = predict(hyper_model, newdata = newdata) %>% getPredictionResponse()
  #mean = apply(newdata, 1, utility)
  #mean = utility(simulated_data_x)
  - rnorm(1, mean = mean, sd = sd)
}

obj_fun = makeSingleObjectiveFunction(name = "exo utility", 
                                      fn = utility, has.simple.signature = TRUE,
                                      par.set = makeNumericParamSet(
                                        len = 2, id = "x", 
                                        lower = 0, upper = 50,
                                        vector = TRUE),
                                      minimize = TRUE 
)
parameter_set = getParamSet(obj_fun)

# sample = generateDesign(n = 1000, par.set = parameter_set, fun = lhs::maximinLHS)
# 
# y = apply(sample,1,obj_fun)
# 
# plot(sample$x1, y)
# plot(sample$x2, y)
# 
# plot3D(sample$x2, sample$x1, y)
# matrix = cbind(sample$x2, sample$x1, y)
# persp(z=matrix)
# persp(sample$x2, sample$x1, y, col='blue')
# 


# # Create a 2D grid of points
# grid <- expand.grid(x1 = seq(min(sample$x1), max(sample$x1), length.out = 10),
#                     x2 = seq(min(sample$x2), max(sample$x2), length.out = 10))
# 
# # Evaluate the objective function over the grid
# y <- matrix(NA, nrow = 10, ncol = 10)
# for (i in 1:nrow(grid)) {
#   y[i] <- obj_fun(c(grid$x1[i], grid$x2[i]))
# }
# 
# # Create the contour plot
# contour(seq(min(sample$x1), max(sample$x1), length.out = 10),
#         seq(min(sample$x2), max(sample$x2), length.out = 10),
#         y,
#         xlab = "x1", ylab = "x2", main = "Contour Plot of Objective Function")


################
# end of obj-fun 
################

###############
# begin BO
##

parameter_set = getParamSet(obj_fun)


####
## generatl settings BO
#lambda = 5
infill_crit = makeMBOInfillCritCB(cb.lambda = lambda)
# set Control Argument of BO 
# store all models (here; 1:2 because only one iter internally)  
ctrl = makeMBOControl(propose.points = 1L, store.model.at = 1:2)
ctrl = setMBOControlInfill(ctrl, crit = infill_crit, opt = "focussearch", 
                           opt.focussearch.points = 1000, opt.focussearch.maxit = 1)
lrn = makeLearner("regr.km", predict.type = "se", covtype = "gauss", control = list(trace = FALSE))
#lrn = makeLearner("regr.randomForest", predict.type = "se")
# ensure numerical stability in km {DiceKriging} cf. github issue and recommendation by Bernd Bischl 
des_num = generateDesign(n = 20, par.set = parameter_set, fun = lhs::maximinLHS)
y = apply(des_num, 1, obj_fun)
Nuggets = 1e-8*var(y)
lrn = setHyperPars(learner = lrn, nugget=Nuggets)








baseline_results = list()
baseline_mod_results = list()
shapleyBO_results = list()
shapleyBO_results_cb = list()
human_results = list()
baseline_par_results = list()

baseline_par_opts = list()
baseline_mod_opts = list()
baseline_opts = list()
shapley_opts = list()
shapley_opts_cb = list()
human_opts = list()

number_interventions_tot = list()
number_interventions_tot_cb = list()

param_set_biased = makeParamSet(
  makeNumericVectorParam("x", len = 2, lower = 20, upper = 40)
)

sample = generateDesign(n = 1000, par.set = param_set_biased, fun = lhs::maximinLHS)

#sample = generateDesign(n = 1000, par.set = parameter_set, fun = lhs::maximinLHS)

designs_list = list()
for (i in 1:n_exp) {
  design <- sample[sample.int(nrow(sample),init_design_size),]
  designs_list[[i]] = design
}

###### pre-BO 
## agent knowledge simulation 
#init_design_size_agent = 5
# param_set_biased_agent = makeParamSet(
#   makeNumericVectorParam("x", len = 2, lower = 5, upper = 50)
# )

design_agent <- generateDesign(n = init_design_size_agent, par.set = parameter_set, fun = lhs::maximinLHS)
#ctrl <- makeMBOControl(final.method = "best.true.y", final.evals = 5)
#lambda_agent = 5 # for init shapleys
infill_crit_agent = makeMBOInfillCritCB(cb.lambda = lambda_agent)
# set Control Argument of BO 
# store all models (here; 1:2 because only one iter internally)  
ctrl_agent = makeMBOControl(propose.points = 1L, store.model.at = 1:2)
ctrl_agent = setMBOControlInfill(ctrl, crit = infill_crit_agent, opt = "focussearch", 
                                 opt.focussearch.points = 1000, opt.focussearch.maxit = 1)
lrn_agent = makeLearner("regr.km", predict.type = "se", covtype = "gauss", control = list(trace = FALSE))
#lrn_agent = makeLearner("regr.randomForest", predict.type = "se")
# ensure numerical stability in km {DiceKriging} cf. github issue and recommendation by Bernd Bischl 
y = apply(design, 1, obj_fun)
Nuggets = 1e-8*var(y)
lrn_agent = setHyperPars(learner = lrn_agent, nugget=Nuggets)
ctrl_agent = setMBOControlTermination(ctrl_agent, iters = 1)
# initial setup of agent
initial_iters_agent = 100
ctrl_agent = makeMBOControl(propose.points = 1L, store.model.at = 1:initial_iters_agent)
ctrl_agent = setMBOControlTermination(ctrl_agent, iters = initial_iters_agent)
res_mbo_agent = mbo(fun = obj_fun, design = design_agent, control = ctrl_agent, learner = lrn_agent, show.info = F)
res_mbo_agent_df = res_mbo_agent$opt.path %>% as.data.frame()

# par intervention crit
prop_x1 = res_mbo_agent_df$x1
prop_x2 = res_mbo_agent_df$x2
par_ratio_agent = mean( prop_x1 / prop_x2 )

#shapley intervention crit
shapleys = ShapleyMBO(res.mbo = res_mbo_agent, iter.interest = 1:initial_iters_agent, contribution = TRUE)
x1_ind = subset(1:nrow(shapleys), 1:nrow(shapleys) %% 2 == 1)
x2_ind = subset(1:nrow(shapleys), 1:nrow(shapleys) %% 2 == 0)
mean_shapley_x1 = shapleys$phi_mean_scaled[x1_ind] %>% mean
mean_shapley_x2 = shapleys$phi_mean_scaled[x2_ind] %>% mean
shapley_ratio_agent = mean_shapley_x1 / mean_shapley_x2

mean_shapley_x1 = shapleys$phi_cb[x1_ind] %>% mean
mean_shapley_x2 = shapleys$phi_cb[x2_ind] %>% mean
shapley_ratio_agent_cb = mean_shapley_x1 / mean_shapley_x2

# edit
#shapley_ratio_agent_cb = shapley_ratio_agent

# viz shapleys
plotShapleyMBO(shapleys, "cb", lambda.mbo = lambda_agent, type = "line")


### regression

dat_reg= data.frame("phi_mean_x1" = shapleys$phi_mean_scaled[x1_ind],
                    "phi_mean_x2" = shapleys$phi_mean_scaled[x2_ind],
                    #"shapley_ratio" = shapleys$phi_mean_scaled[x1_ind]/shapleys$phi_mean_scaled[x2_ind],
                    "y" = res_mbo_agent_df$y[(init_design_size_agent+1):(init_design_size_agent+initial_iters_agent)])

lm_fit = lm(y ~ phi_mean_x1 + phi_mean_x2, data = dat_reg)
lm_fit %>% summary



tree <- rpart(y ~ phi_mean_x1 + phi_mean_x2, data=dat_reg, control=rpart.control(cp=.0000001))
#view results
#printcp(tree)
#prp(tree)

tree$splits

#identify best cp value to use
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

#produce a pruned tree based on the best cp value
pruned_tree <- prune(tree, cp=best)
pruned_tree
# #plot the pruned tree
prp(pruned_tree,
    faclen=1, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=4) #display 5 decimal places in output

split_var = pruned_tree$frame$var[1]
split_crit = pruned_tree$splits[1,4]




#lambda_agent = 5
infill_crit_agent = makeMBOInfillCritCB(cb.lambda = lambda_agent)
# set Control Argument of BO 
# store all models (here; 1:2 because only one iter internally)  
ctrl_agent = makeMBOControl(propose.points = 1L, store.model.at = 1:2)
ctrl_agent = setMBOControlInfill(ctrl, crit = infill_crit_agent, opt = "focussearch", 
                                 opt.focussearch.points = 1000, opt.focussearch.maxit = 1)

# # update human design
# res_mbo_agent_df = res_mbo_agent$opt.path %>% as.data.frame()
# init_proposal_agent = res_mbo_agent_df[(init_design_size_agent+1):(init_design_size_agent+initial_iters_agent),1:2]
# design_agent = rbind(design_agent, init_proposal_agent)



##########
## outer loop experiments
for (i in 1:n_exp) {
  #browser()
  ####################
  # Start of actual BO
  
  ## baseline BO (no interventions at all, A0)
  design = designs_list[[i]]
  for (b in 1:budget) {
    #browser()
    ctrl = setMBOControlTermination(ctrl, iters = 1)
    res_mbo = mbo(fun = obj_fun, design = design, control = ctrl, learner = lrn, show.info = F)
    mbo_design = res_mbo$opt.path %>% as.data.frame()
    design = mbo_design[,1:2]
  }
  # optimum
  #res_mbo$y
  # final fit
  ctrl = setMBOControlTermination(ctrl, iters = 1)
  res_mbo = mbo(fun = obj_fun, design = design, control = ctrl, learner = lrn, show.info = F)
  # final res
  baseline_res = res_mbo
  
  
  ## baseline human (only interventions)
  design = designs_list[[i]]
  for (b in 1:budget) {
      ctrl_agent = setMBOControlTermination(ctrl_agent, iters = 1)
      #design_agent_in_loop = rbind(design_agent, design)
      design_agent_in_loop = design_agent
      res_mbo_agent = mbo(fun = obj_fun, design = design_agent_in_loop, control = ctrl_agent, learner = lrn_agent, show.info = F)
      mbo_design_agent = res_mbo_agent$opt.path %>% as.data.frame()
      proposal_agent = mbo_design_agent[nrow(mbo_design_agent),1:2]
      design = rbind(design, proposal_agent)
}
# final fit
ctrl = setMBOControlTermination(ctrl, iters = 1)
res_mbo = mbo(fun = obj_fun, design = design, control = ctrl, learner = lrn, show.info = F)
# final result
human_res = res_mbo
  
  
  # this script runs the baseline BO for an agent who has no access to shapleyBO,
  # but intervenes in each kth-iteration
  ## BO loop
  design = designs_list[[i]]
  for (b in 1:budget) {
    #browser()
    ctrl = setMBOControlTermination(ctrl, iters = 1)
    res_mbo = mbo(fun = obj_fun, design = design, control = ctrl, learner = lrn, show.info = F)
    mbo_design = res_mbo$opt.path %>% as.data.frame()
    if(b %% 2 == 0){
      ctrl_agent = setMBOControlTermination(ctrl_agent, iters = 1)
      #design_agent_in_loop = rbind(design_agent, design)
      design_agent_in_loop = design_agent
      res_mbo_agent = mbo(fun = obj_fun, design = design_agent_in_loop, control = ctrl_agent, learner = lrn_agent, show.info = F)
      mbo_design_agent = res_mbo_agent$opt.path %>% as.data.frame()
      proposal_agent = mbo_design_agent[nrow(mbo_design_agent),1:2]
      design = rbind(design, proposal_agent)
    }else{
      design = mbo_design[,1:2]
    }
  }
  # final fit
  ctrl = setMBOControlTermination(ctrl, iters = 1)
  res_mbo = mbo(fun = obj_fun, design = design, control = ctrl, learner = lrn, show.info = F)
  
  # optimum
  #res_mbo$y
  baseline_mod_res = res_mbo
  
  
  
  
  # this script runs the baseline BO for an agent who has no access to shapleyBO,
  # but intervenes in based on parameter values
  ## BO loop
  design = designs_list[[i]]
  for (b in 1:budget) {
    #browser()
    ctrl = setMBOControlTermination(ctrl, iters = 1)
    res_mbo = mbo(fun = obj_fun, design = design, control = ctrl, learner = lrn, show.info = F)
    mbo_design = res_mbo$opt.path %>% as.data.frame()
    prop_x1 = mbo_design$x1[length(mbo_design$x1)]
    prop_x2 = mbo_design$x2[length(mbo_design$x2)]
    par_ratio_bo = prop_x1 / prop_x2
    if(par_ratio_agent/par_ratio_bo > 2 |
       par_ratio_agent/par_ratio_bo < 0.5 ){
      ctrl_agent = setMBOControlTermination(ctrl_agent, iters = 1)
      #design_agent_in_loop = rbind(design_agent, design)
      design_agent_in_loop = design_agent
      res_mbo_agent = mbo(fun = obj_fun, design = design_agent_in_loop, control = ctrl_agent, learner = lrn_agent, show.info = F)
      mbo_design_agent = res_mbo_agent$opt.path %>% as.data.frame()
      proposal_agent = mbo_design_agent[nrow(mbo_design_agent),1:2]
      design = rbind(design, proposal_agent)
    }else{
      design = mbo_design[,1:2]
    }
  }
  # final fit
  ctrl = setMBOControlTermination(ctrl, iters = 1)
  res_mbo = mbo(fun = obj_fun, design = design, control = ctrl, learner = lrn, show.info = F)
  
  # optimum
  #res_mbo$y
  baseline_par_res = res_mbo
  
  
  design = designs_list[[i]]
  # this script runs the BO for an agent who has access to shapleyBO
  # SHAPLEYBO
  number_interventions = 0
  ## BO loop
  for (b in 1:budget) {
    #browser()
    ctrl = setMBOControlTermination(ctrl, iters = 1)
    ctrl_agent = makeMBOControl(propose.points = 1L, store.model.at = 1:2)
    # auto BO:
    res_mbo = mbo(fun = obj_fun, design = design, control = ctrl, learner = lrn, show.info = F)
    mbo_design = res_mbo$opt.path %>% as.data.frame()
    shapleys = ShapleyMBO(res.mbo = res_mbo, iter.interest = 1, contribution = TRUE)
    # regularity conditions:
    if(shapleys$phi_mean_scaled[1] == 0 & shapleys$phi_mean_scaled[2] == 0){
      shapley_ratio_bo = 1
    }else{
      shapley_ratio_bo = shapleys$phi_mean_scaled[1] / shapleys$phi_mean_scaled[2]
    }
    if(is.nan(shapley_ratio_agent)==TRUE)
      shapley_ratio_agent = 1
    
    # human bo interface: does the proposal align with agent's knowledge?
    # if(shapley_ratio_agent/shapley_ratio_bo > 2 |
    #    shapley_ratio_agent/shapley_ratio_bo < 0.5 ){
    if(split_var == "phi_mean_x1"){
      crit_var = shapleys$phi_mean_scaled[1]
    }else{
      crit_var = shapleys$phi_mean_scaled[2]
    }
    if(is.null(split_crit) == TRUE){
      split_crit = -3
    }
    if(crit_var >  split_crit){
      number_interventions = number_interventions +1
      ctrl_agent = setMBOControlTermination(ctrl_agent, iters = 1)
      # agent BO:
      #design_agent_in_loop = rbind(design_agent, design)
      design_agent_in_loop = design_agent
      res_mbo_agent = mbo(fun = obj_fun, design = design_agent_in_loop, control = ctrl_agent, learner = lrn_agent, show.info = F)
      mbo_design_agent = res_mbo_agent$opt.path %>% as.data.frame()
      proposal_agent = mbo_design_agent[nrow(mbo_design_agent),1:2]
      design = rbind(design, proposal_agent)
      # update shapleys:
      design_update_sh = rbind(design, design_agent)
      res_mbo_agent_sh = mbo(fun = obj_fun, design = design_update_sh, control = ctrl_agent, learner = lrn_agent, show.info = F)
      shapleys_update = ShapleyMBO(res.mbo = res_mbo_agent_sh, iter.interest = NULL, contribution = TRUE)
      x1_ind = subset(1:nrow(shapleys_update), 1:nrow(shapleys_update) %% 2 == 1)
      x2_ind = subset(1:nrow(shapleys_update), 1:nrow(shapleys_update) %% 2 == 0)
      mean_shapley_x1 = shapleys_update$phi_mean_scaled[x1_ind] %>% mean
      mean_shapley_x2 = shapleys_update$phi_mean_scaled[x2_ind] %>% mean
      shapley_ratio_agent = mean_shapley_x1 / mean_shapley_x2
      
    }else{
      design = mbo_design[,1:2]
    }
  }
  # final fit
  ctrl = setMBOControlTermination(ctrl, iters = 1)
  res_mbo = mbo(fun = obj_fun, design = design, control = ctrl, learner = lrn, show.info = F)
  shapley_res = res_mbo
  
  
  
  design = designs_list[[i]]
  # this script runs the BO for an agent who has access to shapleyBO with cb shapley values
  # SHAPLEYBO with cb
  number_interventions_cb = 0
  ## BO loop
  for (b in 1:budget) {
    #browser()
    ctrl = setMBOControlTermination(ctrl, iters = 1)
    ctrl_agent = makeMBOControl(propose.points = 1L, store.model.at = 1:2)
    # auto BO:
    res_mbo = mbo(fun = obj_fun, design = design, control = ctrl, learner = lrn, show.info = F)
    mbo_design = res_mbo$opt.path %>% as.data.frame()
    shapleys = ShapleyMBO(res.mbo = res_mbo, iter.interest = 1, contribution = TRUE)
    # regularity conditions:
    if(shapleys$phi_cb[1] == 0 & shapleys$phi_cb[2] == 0){
      shapley_ratio_bo_cb = 1
    }else{
      shapley_ratio_bo_cb = shapleys$phi_cb[1] / shapleys$phi_cb[2]
    }
    if(is.nan(shapley_ratio_agent_cb)==TRUE)
      shapley_ratio_agent_cb = 1
    
    # regularity conditions:
    if(shapleys$phi_mean_scaled[1] == 0 & shapleys$phi_mean_scaled[2] == 0){
      shapley_ratio_bo = 1
    }else{
      shapley_ratio_bo = shapleys$phi_mean_scaled[1] / shapleys$phi_mean_scaled[2]
    }
    if(is.nan(shapley_ratio_agent)==TRUE)
      shapley_ratio_agent = 1

    # human bo interface: does the proposal align with agent's knowledge?
    if(shapley_ratio_agent/shapley_ratio_bo > 2 |
       shapley_ratio_agent/shapley_ratio_bo < 0.5 ){
      number_interventions_cb = number_interventions_cb +1
      ctrl_agent = setMBOControlTermination(ctrl_agent, iters = 1)
      # agent BO:
      #design_agent_in_loop = rbind(design_agent, design)
      design_agent_in_loop = design_agent
      res_mbo_agent = mbo(fun = obj_fun, design = design_agent_in_loop, control = ctrl_agent, learner = lrn_agent, show.info = F)
      mbo_design_agent = res_mbo_agent$opt.path %>% as.data.frame()
      proposal_agent = mbo_design_agent[nrow(mbo_design_agent),1:2]
      design = rbind(design, proposal_agent)
      # update shapleys:
      design_update_sh = rbind(design, design_agent)
      res_mbo_agent_sh = mbo(fun = obj_fun, design = design_update_sh, control = ctrl_agent, learner = lrn_agent, show.info = F)
      shapleys_update = ShapleyMBO(res.mbo = res_mbo_agent_sh, iter.interest = NULL, contribution = TRUE)
      x1_ind = subset(1:nrow(shapleys_update), 1:nrow(shapleys_update) %% 2 == 1)
      x2_ind = subset(1:nrow(shapleys_update), 1:nrow(shapleys_update) %% 2 == 0)
      mean_shapley_x1 = shapleys_update$phi_mean_scaled[x1_ind] %>% mean
      mean_shapley_x2 = shapleys_update$phi_mean_scaled[x2_ind] %>% mean
      shapley_ratio_agent = mean_shapley_x1 / mean_shapley_x2

    }else{
      design = mbo_design[,1:2]
    }
  }
  # final fit
  ctrl = setMBOControlTermination(ctrl, iters = 1)
  res_mbo = mbo(fun = obj_fun, design = design, control = ctrl, learner = lrn, show.info = F)
   shapley_res_cb = res_mbo
  # 
  
  
  # design_final = res_mbo$opt.path %>% as.data.frame()
  # shapley_res_y = apply(design_final[,1:2],1, obj_fun)
  # shapley_res = cbind(design_final[,1:2],shapley_res_y)
  # 
  baseline_results[[i]] = baseline_res
  baseline_mod_results[[i]] = baseline_mod_res
  shapleyBO_results[[i]] = shapley_res_cb
  human_results[[i]] = human_res
  baseline_par_results[[i]] = baseline_par_res
  
  number_interventions_tot[[i]] = number_interventions
  number_interventions_tot_cb[[i]] = number_interventions_cb
  
  human_opts[[i]] = human_res$y
  baseline_opts[[i]] = baseline_res$y
  shapley_opts[[i]] = shapley_res_cb$y
  baseline_mod_opts[[i]] = baseline_mod_res$y
  baseline_par_opts[[i]] = baseline_par_res$y
  print(i)
}

human_opts %>% unlist %>% mean
baseline_mod_opts %>% unlist %>% mean
baseline_opts %>% unlist %>% mean
shapley_opts %>% unlist %>% mean
shapley_opts_cb %>% unlist %>% mean
baseline_par_opts %>% unlist %>% mean

time = Sys.time()
print(time)
time = substring(time, 12,19)
# saveRDS(baseline_opts,file =paste("baseline_res",
#                                   data_name,budget,init_design_size,init_design_size_agent,
#                                   initial_iters_agent,lambda, lambda_agent,".rds",
#                                   sep = "-"))
# saveRDS(baseline_mod_opts,file =paste("baseline_mod_res",
#                                       data_name,budget,init_design_size,init_design_size_agent,
#                                       initial_iters_agent,lambda, lambda_agent,".rds",
#                                       sep = "-"))
# saveRDS(shapley_opts,file =paste("shapley_res",
#                                  data_name,budget,init_design_size,init_design_size_agent,
#                                  initial_iters_agent,lambda, lambda_agent,".rds",
#                                  sep = "-"))
# saveRDS(number_interventions_tot,file =paste("n_interventions_res",
#                                              data_name,budget,init_design_size,init_design_size_agent,
#                                              initial_iters_agent,lambda, lambda_agent,".rds",
#                                              sep = "-"))
saveRDS(number_interventions_tot_cb,file =paste("n_interventions_res",
                                             data_name,budget,init_design_size,init_design_size_agent,
                                             initial_iters_agent,lambda, lambda_agent,".rds",
                                             sep = "-"))
saveRDS(baseline_mod_results,file =paste("baseline_mod_res_complete",
                                         data_name,budget,init_design_size,init_design_size_agent,
                                         initial_iters_agent,lambda, lambda_agent,".rds",
                                         sep = "-"))
saveRDS(baseline_results,file =paste("baseline_res_complete",
                                     data_name,budget,init_design_size,init_design_size_agent,
                                     initial_iters_agent,lambda, lambda_agent,".rds",
                                     sep = "-"))
# saveRDS(shapleyBO_results,file =paste("shapley_res_complete",
#                                       data_name,budget,init_design_size,init_design_size_agent,
#                                       initial_iters_agent,lambda, lambda_agent,".rds",
#                                       sep = "-"))
saveRDS(shapleyBO_results,file =paste("shapley_res_complete",
                                      data_name,budget,init_design_size,init_design_size_agent,
                                      initial_iters_agent,lambda, lambda_agent,".rds",
                                      sep = "-"))
saveRDS(human_results,file =paste("human_res_complete",
                                      data_name,budget,init_design_size,init_design_size_agent,
                                      initial_iters_agent,lambda, lambda_agent,".rds",
                                      sep = "-"))
saveRDS(baseline_par_results,file =paste("baseline_par_res_complete",
                                  data_name,budget,init_design_size,init_design_size_agent,
                                  initial_iters_agent,lambda, lambda_agent,".rds",
                                  sep = "-"))

source("viz-results-auto.R")
