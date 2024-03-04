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
library(readr)

source("R/makeMBOInfillCritUACB.R")
source("R/initCrit.InfillCritUACB.R")
source("R/makeMBOInfillCritRACB.R")
source("R/initCrit.InfillCritRACB.R")
source("R/ShapleyMBO.R")
source("R/plotShapleyMBO.R")
#source("R/_Explore_Exploit_Measures/xplxpl-jr.R")



set.seed(234523)

n_exp = 30 # number of experimental repetition per subject

budget = 15
init_design_size = 5
#init_design_size_agent = 3

lambda = 5

# run experiments for all study subjects
for(dat_nr in 13:15){
  for (lambda_agent in c(2,3,5)) {
    for (init_design_size_agent in c(5,10,20,30,40,60)) {
      

  print(dat_nr)
  
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
  
  source("run-experiments-source.R")
    }
  }
}




