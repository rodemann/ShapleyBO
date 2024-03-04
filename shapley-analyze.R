
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
library(readr)
library(rpart)
library(rpart.plot)

source("R/makeMBOInfillCritUACB.R")
source("R/initCrit.InfillCritUACB.R")
source("R/makeMBOInfillCritRACB.R")
source("R/initCrit.InfillCritRACB.R")
source("R/ShapleyMBO.R")
source("R/plotShapleyMBO.R")


set.seed(23424652)

dat_nr = "13"
subject = paste("S",as.character(dat_nr),sep="")
#load utilities for one person, average over all three blocks
data_name = subject 
add <- function(x) Reduce("+", x)

data_1st_trial <- read_csv(paste("data/",subject,"-B1.csv",sep=""))
data_1st_trial = data_1st_trial %>% as.matrix
data_2nd_trial <- read_csv(paste("data/",subject,"-B2.csv",sep=""))
data_2nd_trial = data_2nd_trial %>% as.matrix
data_3rd_trial <- read_csv(paste("data/",subject,"-B3.csv",sep=""))
data_3rd_trial = data_3rd_trial %>% as.matrix
#get average of utilities for each config over the 3 trials
data = list(data_1st_trial, data_2nd_trial, data_3rd_trial) %>% add
data = data / 3

features = c("low","lif")
start_low <- 1
start_lif <- 1
end_low = 100
end_lif = 100
griddf <- expand.grid(low = seq(from = start_low, to = end_low, by = 1),
                      lif = seq(from = start_lif, to = end_lif, by = 1) )
sub_griddf = griddf[sample.int(100*100, size = 75),] %>% as.matrix
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


var_function = function(x) 0.00001  #0.001x # 2*abs(x) optional: make heteroscedastic

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
lambda = 20
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

baseline_mod_opts = list()
baseline_opts = list()
shapley_opts = list()
shapley_opts_cb = list()
human_opts = list()

number_interventions_tot = list()
number_interventions_tot_cb = list()

param_set_biased = makeParamSet(
  makeNumericVectorParam("x", len = 2, lower = 20, upper = 25)
)

sample = generateDesign(n = 1000, par.set = param_set_biased, fun = lhs::maximinLHS)

#sample = generateDesign(n = 1000, par.set = parameter_set, fun = lhs::maximinLHS)



###### pre-BO 
## agent knowledge simulation 
init_design_size_agent = 50
design_agent <- generateDesign(n = init_design_size_agent, par.set = parameter_set, fun = lhs::maximinLHS)
#ctrl <- makeMBOControl(final.method = "best.true.y", final.evals = 5)
lambda_agent = 20 # for init shapleys
infill_crit_agent = makeMBOInfillCritCB(cb.lambda = lambda_agent)
# set Control Argument of BO 
# store all models (here; 1:2 because only one iter internally)  
ctrl_agent = makeMBOControl(propose.points = 1L, store.model.at = 1:2)
ctrl_agent = setMBOControlInfill(ctrl, crit = infill_crit_agent, opt = "focussearch", 
                                 opt.focussearch.points = 1000, opt.focussearch.maxit = 1)
lrn_agent = makeLearner("regr.km", predict.type = "se", covtype = "gauss", control = list(trace = FALSE))
#lrn_agent = makeLearner("regr.randomForest", predict.type = "se")
# ensure numerical stability in km {DiceKriging} cf. github issue and recommendation by Bernd Bischl 
y = apply(design_agent, 1, obj_fun)
Nuggets = 1e-8*var(y)
lrn_agent = setHyperPars(learner = lrn_agent, nugget=Nuggets)
ctrl_agent = setMBOControlTermination(ctrl_agent, iters = 1)
# initial setup of agent
initial_iters_agent = 30
ctrl_agent = makeMBOControl(propose.points = 1L, store.model.at = 1:initial_iters_agent)
ctrl_agent = setMBOControlTermination(ctrl_agent, iters = initial_iters_agent)
res_mbo_agent = mbo(fun = obj_fun, design = design_agent, control = ctrl_agent, learner = lrn_agent, show.info = F)
res_mbo_agent_df = res_mbo_agent$opt.path %>% as.data.frame()
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
shapley_ratio_agent_cb = shapley_ratio_agent

# viz shapleys
plotShapleyMBO(shapleys, "cb", lambda.mbo = lambda_agent, type = "line")



### regression

dat_reg= data.frame("phi_mean_x1" = shapleys$phi_mean_scaled[x1_ind],
           "phi_mean_x2" = shapleys$phi_mean_scaled[x2_ind],
           "shapley_ratio" = shapleys$phi_mean_scaled[x1_ind]/shapleys$phi_mean_scaled[x2_ind],
           "y" = res_mbo_agent_df$y[(init_design_size_agent+1):(init_design_size_agent+initial_iters_agent)])

lm_fit = lm(y ~ phi_mean_x1 + phi_mean_x2, data = dat_reg)
lm_fit %>% summary

lm_fit = lm(y ~ shapley_ratio, data = dat_reg)
lm_fit %>% summary


tree <- rpart(y ~ phi_mean_x1 + phi_mean_x2, data=dat_reg, control=rpart.control(cp=.0000001))
#view results
printcp(tree)
prp(tree)

tree$splits

#identify best cp value to use
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

#produce a pruned tree based on the best cp value
pruned_tree <- prune(tree, cp=best)
pruned_tree
#plot the pruned tree
prp(pruned_tree,
    faclen=1, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=4) #display 5 decimal places in output

split_var = pruned_tree$frame$var[1]
split_crit = pruned_tree$splits[1,4]

