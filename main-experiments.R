# compare agent with shapleys to agent without 
# all agents have own interal utility model which is simulated by another BO, see section 5 

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
library(patchwork)

source("R/makeMBOInfillCritUACB.R")
source("R/initCrit.InfillCritUACB.R")
source("R/makeMBOInfillCritRACB.R")
source("R/initCrit.InfillCritRACB.R")
source("R/ShapleyMBO.R")
source("R/plotShapleyMBO.R")



set.seed(234523)


n_exp = 3 # number of experimental repetition per subject (in paper: 40)
budget = 3
init_design_size_agent = 90
lambda = 20

# run experiments for all study subjects
for(dat_nr in 1:3){
  for (lambda_agent in c(200)) {
    for (init_design_size in c(3)) {

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
      tryCatch(
        source("run-experiments-source.R")
      )
    }
  }
}




