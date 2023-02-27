#Libraries
suppressWarnings(library(tidyverse))
suppressWarnings(library(doSNOW)) # parallel processing
suppressWarnings(library(caret))
suppressWarnings(library(ROCR)) # for AUC / ROC curve
suppressWarnings(library(readr))
suppressWarnings(library(dplyr))
suppressWarnings(library(InformationValue))
suppressWarnings(library(classInt))
#suppressWarnings(library(parallel))



# init parallel processing
print(paste0('Available processors: ', Sys.getenv('NUMBER_OF_PROCESSORS')))
cl <- makeCluster(24)
#registerDoSNOW(cl) # ON LINUX: registerDoParallel(cl)
#print(cl)




#### Import ####
# fast load:
load('data/df_train.RData') # => df_train
load('models_db/db.RData')


## Important functions
source("add_to_models.R")
source("bin.R")



















