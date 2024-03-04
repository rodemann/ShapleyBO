# compare agent with shapleys to agent without 
# both agents have own interal utility model which is simulated by another BO
# agents have different knowledge than automatic BO
# agent 1 intervenes each 3rd intervention (or randomly?)
# agent 2 intervenes only if shapleys fulfill certain conditions
# e.g., internal shapleys suggest lifting gain to be more relevant than lowering gain
# i.e., only intervene if shapley ratio diverges from internally learned ratio
# (similar setup as in human-bo team paper)
#TODO possible extension: third agent that uses information on proposals but not on shapleys

library(mlrMBO)
library(ggplot2)
library(dplyr)
library(smoof)
library(iml)
library(readr)
library(readxl)
library(mvtnorm)
library(DiceKriging)
library(randomForest)
source("R/makeMBOInfillCritUACB.R")
source("R/initCrit.InfillCritUACB.R")
source("R/makeMBOInfillCritRACB.R")
source("R/initCrit.InfillCritRACB.R")
source("R/ShapleyMBO.R")
#source("R/_Explore_Exploit_Measures/xplxpl-jr.R")

# first create ground truth by estimating hypersurrogate utility from data
# simulated_data_x <- read_csv("C:/Users/Julian Rodemann/Downloads/simulated_data_x.csv")
# simulated_data_pref <- read_csv("C:/Users/Julian Rodemann/Downloads/simulated_data_pref.csv")
# save(simulated_data_x, file = "data/simulated_data_x.csv")
# save(simulated_data_pref, file = "data/simulated_data_pref.csv")

library(readr)
#load utilities for one person, average over all three blocks
data_name = "S13"
add <- function(x) Reduce("+", x)

S13_B1 <- read_csv("data/S13-B1.csv")
data_1 = S13_B1 %>% as.matrix
S13_B2 <- read_csv("data/S13-B2.csv")
data_2 = S13_B2 %>% as.matrix
S13_B3 <- read_csv("data/S13-B3.csv")
data_3 = S13_B3 %>% as.matrix
#get average of utilities for each config over the 3 trials
data = list(data_1, data_2, data_3) %>% add
data = data / 3


features = c("low","lif")
start_low <- 1
start_lif <- 1
end_low = 100
end_lif = 100
griddf <- expand.grid(low = seq(from = start_low, to = end_low, by = 1),
                      lif = seq(from = start_lif, to = end_lif, by = 1) )
sub_griddf = griddf[sample.int(100*100, size = 800),] %>% as.matrix
get_util = function(x) data[x[1],x[2]] %>% as.numeric
utilities = apply(sub_griddf, 1, get_util) #%>% unlist
hyper_data = cbind(sub_griddf,utilities) %>% as.data.frame()
# #learn (hyper)surrogate
lrn_hyper = makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))
#lrn_hyper = makeLearner("regr.randomForest", predict.type = "se")
Nuggets = 1e-8
lrn_hyper = setHyperPars(learner = lrn_hyper, nugget=Nuggets)
hyper_estim = makeRegrTask(data = hyper_data, target = "utilities")
hyper_model = train(lrn_hyper, hyper_estim)


var_function = function(x) 0.001  #0.001x # 2*abs(x) optional: make heteroscedastic

utility = function(x){
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




################
# end of obj-fun 
################

###############
# begin BO
##
budget = 5
init_design_size = 10
parameter_set = getParamSet(obj_fun)


####
## auto BO
# same design for all approaches
design <- generateDesign(n = init_design_size, par.set = parameter_set, fun = lhs::randomLHS)
# same final evaluation method
#ctrl <- makeMBOControl(final.method = "best.true.y", final.evals = 5)
lambda = 5
infill_crit = makeMBOInfillCritCB(cb.lambda = lambda)
# set Control Argument of BO 
# store all models (here; 1:2 because only one iter internally)  
ctrl = makeMBOControl(propose.points = 1L, store.model.at = 1:2)
ctrl = setMBOControlInfill(ctrl, crit = infill_crit, opt = "focussearch", 
                           opt.focussearch.points = 1000, opt.focussearch.maxit = 1)
lrn = makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))
#lrn = makeLearner("regr.randomForest", predict.type = "se")
# ensure numerical stability in km {DiceKriging} cf. github issue and recommendation by Bernd Bischl 
y = apply(design, 1, obj_fun)
Nuggets = 1e-8*var(y)
lrn = setHyperPars(learner = lrn, nugget=Nuggets)

plot(design$x1, y)
plot(design$x2, y)









baseline_results = list()
shapleyBO_results = list()
baseline_opts = list()
shapley_opts = list()
number_interventions_tot = list()

sample = generateDesign(n = 2000, par.set = parameter_set, fun = lhs::maximinLHS)

n_exp= 30
designs_list = list()
for (i in 1:n_exp) {
  design <- sample[sample.int(nrow(sample),init_design_size),]
  designs_list[[i]] = design
}

##########
## outer loop experiments
for (i in 1:n_exp) {
  
  ###### pre-BO 
  ## agent knowledge simulation 
  init_design_size_agent = 8
  design_agent <- generateDesign(n = init_design_size_agent, par.set = parameter_set, fun = lhs::maximinLHS)
  #ctrl <- makeMBOControl(final.method = "best.true.y", final.evals = 5)
  lambda_agent = 2
  infill_crit_agent = makeMBOInfillCritCB(cb.lambda = lambda_agent)
  # set Control Argument of BO 
  # store all models (here; 1:2 because only one iter internally)  
  ctrl_agent = makeMBOControl(propose.points = 1L, store.model.at = 1:2)
  ctrl_agent = setMBOControlInfill(ctrl, crit = infill_crit_agent, opt = "focussearch", 
                                   opt.focussearch.points = 2000, opt.focussearch.maxit = 1)
  lrn_agent = makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))
  #lrn_agent = makeLearner("regr.randomForest", predict.type = "se")
  # ensure numerical stability in km {DiceKriging} cf. github issue and recommendation by Bernd Bischl 
  y = apply(design, 1, obj_fun)
  Nuggets = 1e-8*var(y)
  lrn_agent = setHyperPars(learner = lrn_agent, nugget=Nuggets)
  ctrl_agent = setMBOControlTermination(ctrl_agent, iters = 1)
  # initial setup of agent
  initial_iters_agent = 8
  ctrl_agent = makeMBOControl(propose.points = 1L, store.model.at = 1:initial_iters_agent)
  ctrl_agent = setMBOControlTermination(ctrl_agent, iters = initial_iters_agent)
  res_mbo_agent = mbo(fun = obj_fun, design = design_agent, control = ctrl_agent, learner = lrn_agent, show.info = F)
  shapleys = ShapleyMBO(res.mbo = res_mbo_agent, iter.interest = 1:initial_iters_agent, contribution = TRUE)
  
  x1_ind = subset(1:nrow(shapleys), 1:nrow(shapleys) %% 2 == 1)
  x2_ind = subset(1:nrow(shapleys), 1:nrow(shapleys) %% 2 == 0)
  mean_shapley_x1 = shapleys$phi_mean_scaled[x1_ind] %>% mean
  mean_shapley_x2 = shapleys$phi_mean_scaled[x2_ind] %>% mean
  
  shapley_ratio_agent = mean_shapley_x1 / mean_shapley_x2
  
  
  
  # Start of actual BO
  # this script runs the baseline BO for an agent who has no access to shapleyBO
  #
  ## BO loop
  design = designs_list[[i]]
  for (b in 1:budget) {
    #browser()
    ctrl = setMBOControlTermination(ctrl, iters = 1)
    suppressWarnings({
      res_mbo = mbo(fun = obj_fun, design = design, control = ctrl, learner = lrn, show.info = F)
    })
    mbo_design = res_mbo$opt.path %>% as.data.frame()
    if(b %% 5 == 0){
      ctrl_agent = setMBOControlTermination(ctrl_agent, iters = 1)
      suppressWarnings({
        res_mbo_agent = mbo(fun = obj_fun, design = design_agent, control = ctrl_agent, learner = lrn_agent, show.info = F)
      })
      mbo_design_agent = res_mbo_agent$opt.path %>% as.data.frame()
      proposal_agent = mbo_design_agent[nrow(mbo_design_agent),1:2]
      design = rbind(design, proposal_agent)
    }else{
      design = mbo_design[,1:2]
    }
  }
  # optimum
  #res_mbo$y
  baseline_res = res_mbo
  
  
  design = designs_list[[i]]
  #source("exp-shapley-agent.R")
  # this script runs the BO for an agent who has access to shapleyBO
  # SHAPLEYBO
  number_interventions = 0
  ## BO loop
  for (b in 1:budget) {
    browser()
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
    if(shapley_ratio_agent/shapley_ratio_bo > 2 |
       shapley_ratio_agent/shapley_ratio_bo < 0.5 ){
      number_interventions = number_interventions +1
      ctrl_agent = setMBOControlTermination(ctrl_agent, iters = 1)
      # agent BO:
      design_agent_in_loop = rbind(design_agent, design)
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
  shapley_res = res_mbo
  
  baseline_results[[i]] = baseline_res
  shapleyBO_results[[i]] = shapley_res
  number_interventions_tot[[i]] = number_interventions
  baseline_opts[[i]] = baseline_res$y
  shapley_opts[[i]] = shapley_res$y
  print(i)
}

baseline_opts %>% unlist %>% mean
shapley_opts %>% unlist %>% mean
time = Sys.time()
time = substring(time, 12,19)
saveRDS(baseline_opts,file =paste("baseline_res",
                                  data_name,budget,init_design_size,init_design_size_agent,
                                  initial_iters_agent,lambda, lambda_agent,time,".rds",
                                  sep = "-"))
saveRDS(shapley_opts,file =paste("shapley_res",
                                 data_name,budget,init_design_size,init_design_size_agent,
                                 initial_iters_agent,lambda, lambda_agent,time,".rds",
                                 sep = "-"))
saveRDS(number_interventions_tot,file =paste("n_interventions_res",
                                             data_name,budget,init_design_size,init_design_size_agent,
                                             initial_iters_agent,lambda, lambda_agent,time,".rds",
                                             sep = "-"))

saveRDS(baseline_results,file =paste("baseline_res_complete",
                                     data_name,budget,init_design_size,init_design_size_agent,
                                     initial_iters_agent,lambda, lambda_agent,time,".rds",
                                     sep = "-"))
saveRDS(shapleyBO_results,file =paste("shapley_res_complete",
                                      data_name,budget,init_design_size,init_design_size_agent,
                                      initial_iters_agent,lambda, lambda_agent,time,".rds",
                                      sep = "-"))


#baseline_results[[10]]

## possible extensions: different infill crits
# infill_crit = makeMBOInfillCritUACB(cb.lambda = 5, 
#                                     cb.rho = 1,
#                                     cb.alpha = 10,
#                                     base_kernel= "matern3_2", 
#                                     imprecision= 1, 
#                                     noise_proxy_fun = var_function)
# 
# infill_crit = makeMBOInfillCritRACB(cb.lambda = 0.5, 
#                                     cb.alpha = 0.4,
#                                     noise_proxy_fun = var_function)

