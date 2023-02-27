#
# (CH) 08.03.2022 Idea: Poisson model including exposure via offset
#
# Competition Home Page: https://uniqa4ward.com/en/challenge.html
#

rm(list=ls())

#install.packages('tidyverse')
suppressWarnings(library(tidyverse))

#install.packages('doSNOW')
suppressWarnings(library(doSNOW)) # parallel processing

#install.packages('caret')
suppressWarnings(library(caret))

#install.packages('ROCR')
suppressWarnings(library(ROCR)) # for AUC / ROC curve

suppressWarnings(library(readr))



#install.packages('h2o')
library(h2o)

# utility function: increase x-label space in plots
plot_x_space <- function(shift=0) {
  par(mar=c(5+shift,4,2,2))
}

# init parallel processing
print(paste0('Available processors: ', Sys.getenv('NUMBER_OF_PROCESSORS')))
cl <- makeCluster(28)
registerDoSNOW(cl) # ON LINUX: registerDoParallel(cl)
print(cl)

#### Import ####

# fast load:
load('data/df_train.RData') # => df_train
# load('data/df_test.RData') # => df_test

str(df_train, list.len=1000)

(n_train <- nrow(df_train))

# Variables:
#
# nominal: 24
# integer: 90
# real: 23
# target: 1
# existing model: 1
# => overall 139 columns
#
# Missing values are coded as:
# * '-1' (variable type: REAL, INT)
# * '000' (variable type: NOM)

features_cat <- c('CAR_2_NOM',
                  'CAR_3_NOM',
                  'CAR_8_NOM',
                  'CAR_10_NOM',
                  'CAR_11_NOM',
                  'CAR_12_NOM',
                  'CAR_13_NOM',
                  'CAR_14_NOM',
                  'CAR_17_NOM',
                  'CAR_30_NOM',
                  'GEO_1_NOM',
                  'INS_HISTORY_19_NOM',
                  'INS_HISTORY_31_NOM',
                  'OTHER_2_NOM',
                  'OTHER_3_NOM',
                  'OTHER_4_NOM',
                  'PERSON_7_NOM',
                  'PERSON_9_NOM',
                  'PERSON_10_NOM',
                  'PERSON_13_NOM',
                  'POLICY_DATA_3_NOM',
                  'POLICY_DATA_5_NOM',
                  'POLICY_DATA_6_NOM',
                  'POLICY_DATA_9_NOM')

features_int <- c('CAR_1_INT',
                  'CAR_6_INT',
                  'PERSON_20_REAL',
                  'CAR_7_INT',
                  'CAR_9_INT',
                  'CAR_15_INT',
                  'CAR_16_INT',
                  'CAR_18_INT',
                  'CAR_19_INT',
                  'CAR_20_INT',
                  'CAR_21_INT',
                  'CAR_22_INT',
                  'CAR_23_INT',
                  'CAR_24_INT',
                  'CAR_25_INT',
                  'CAR_26_INT',
                  'CAR_27_INT',
                  'CAR_28_INT',
                  'CAR_29_INT',
                  'CAR_31_INT',
                  'INS_HISTORY_5_INT',
                  'INS_HISTORY_6_INT',
                  'INS_HISTORY_9_INT',
                  'INS_HISTORY_10_INT',
                  'INS_HISTORY_11_INT',
                  'INS_HISTORY_12_INT',
                  'INS_HISTORY_13_INT',
                  'INS_HISTORY_14_INT',
                  'INS_HISTORY_15_INT',
                  'INS_HISTORY_16_INT',
                  'INS_HISTORY_17_INT',
                  'INS_HISTORY_18_INT',
                  'INS_HISTORY_30_INT',
                  'OTHER_1_INT',
                  'OTHER_5_INT',
                  'PERSON_5_INT',
                  'PERSON_6_INT',
                  'PERSON_8_INT',
                  'PERSON_11_INT',
                  'PERSON_12_INT',
                  'PERSON_14_INT',
                  'PERSON_15_INT',
                  'PERSON_16_INT',
                  'PERSON_17_INT',
                  'PERSON_18_INT',
                  'PERSON_19_INT',
                  'PERSON_21_INT',
                  'PERSON_22_INT',
                  'PERSON_23_INT',
                  'PERSON_24_INT',
                  'PERSON_25_INT',
                  'PERSON_26_INT',
                  'PERSON_27_INT',
                  'PERSON_28_INT',
                  'PERSON_29_INT',
                  'PERSON_30_INT',
                  'PERSON_31_INT',
                  'PERSON_32_INT',
                  'PERSON_33_INT',
                  'PERSON_34_INT',
                  'PERSON_35_INT',
                  'PERSON_36_INT',
                  'PERSON_37_INT',
                  'PERSON_38_INT',
                  'PERSON_39_INT',
                  'PERSON_40_INT',
                  'PERSON_41_INT',
                  'PERSON_42_INT',
                  'PERSON_43_INT',
                  'PERSON_44_INT',
                  'PERSON_45_INT',
                  'PERSON_46_INT',
                  'PERSON_47_INT',
                  'PERSON_48_INT',
                  'PERSON_49_INT',
                  'PERSON_50_INT',
                  'PERSON_51_INT',
                  'PERSON_52_INT',
                  'PERSON_53_INT',
                  'PERSON_54_INT',
                  'PERSON_55_INT',
                  'PERSON_56_INT',
                  'PERSON_57_INT',
                  'PERSON_58_INT',
                  'PERSON_59_INT',
                  'PERSON_60_INT',
                  'POLICY_DATA_1_INT',
                  'POLICY_DATA_2_INT',
                  'POLICY_DATA_4_INT',
                  'POLICY_DATA_7_INT',
                  'POLICY_DATA_8_INT')

features_num <- c('EXPOSURE_REAL',
                  'CAR_4_REAL',
                  'INS_HISTORY_1_REAL',
                  'INS_HISTORY_20_REAL',
                  'INS_HISTORY_21_REAL',
                  'INS_HISTORY_22_REAL',
                  'INS_HISTORY_23_REAL',
                  'INS_HISTORY_24_REAL',
                  'INS_HISTORY_25_REAL',
                  'INS_HISTORY_26_REAL',
                  'INS_HISTORY_27_REAL',
                  'INS_HISTORY_28_REAL',
                  'INS_HISTORY_29_REAL',
                  'INS_HISTORY_2_REAL',
                  'INS_HISTORY_3_REAL',
                  'INS_HISTORY_4_REAL',
                  'INS_HISTORY_7_REAL',
                  'INS_HISTORY_8_REAL',
                  'PERSON_1_REAL',
                  'PERSON_2_REAL',
                  'PERSON_3_REAL',
                  'PERSON_4_REAL')

target <- 'CLAIMS_REAL'

model_result <- 'CURRENT_MODEL_REAL'

# #### Handling class imbalance by SMOTE####
# #install.packages("ROSE")
# library(ROSE)
# table(df_train$CLAIMS_REAL)
# 
# #check classes distribution
# prop.table(table(df_train$CLAIMS_REAL))
# 
# t1 <- Sys.time()
# df_train.rose <- ROSE(CLAIMS_REAL ~ ., data = df_train, seed = 1,)$data
# t2 <- Sys.time()
# print(t2-t1)
# 
# 
# 
# #check updated classes distribution
# prop.table(table(df_train.rose$CLAIMS_REAL))
# 
# df_train <- df_train.rose
# 
# #Action items : revert to the initial %distribution
# 
# # #build decision tree models
# # library(rpart)
# # tree.rose <- rpart(CLAIMS_REAL ~ ., data = df_train.rose)
# #
# # # load test data from file
# # load('data/df_test.RData') # => df_test
# #
# # #make predictions on unseen data
# # > pred.tree.rose <- predict(tree.rose, newdata = df_test)


#### Model Fit ####
set.seed(125)
data <- df_train[sample(nrow(df_train),10000),]


predictors <- c(features_cat, features_int, features_num)
predictors <- setdiff(predictors, 'EXPOSURE_REAL') # exposure cannot be used due to strong dependency with loss occurrence
features_num_no_exposure <- setdiff(features_num,'EXPOSURE_REAL') #New
(length(predictors))

formula <- as.formula(paste(target, '~', paste(predictors,collapse=' + ') ))

formula1 <- as.formula(paste(target, '~', paste(features_cat,collapse=' + '),
                             ' + ',paste(features_int,collapse=' + '),
                             ' + s(',paste(features_num_no_exposure,collapse=') + s('),')' ))


formula_curr <- as.formula(paste(target, '~', paste(predictors,collapse=' + ') ))

formula1_sp_curr <- as.formula(paste(target, '~', paste(features_cat,collapse=' + '),
                             ' + ',paste(features_int,collapse=' + '),
                             ' + s(',paste(features_num_no_exposure,collapse=') + s('),')' ))



print(formula)




gam_vs_h2o <- mgcv::bam(formula = formula,data = df_train,
                        family = "poisson", offset = log(data$EXPOSURE_REAL))

gam_vs_h2o_2 <- mgcv::bam(formula = formula1,data = df_train,
                          family = "poisson", offset = log(data$EXPOSURE_REAL))








# 
# for(i in 1:length(features_num_no_exposure)){
#   paste(print(features_num_no_exposure[i]),collapse = print(length(unique(data[,features_num_no_exposure[i]]))))
# }




###
pred1 <- predict(gam_vs_h2o_2, data,type ="response")*data$EXPOSURE_REAL

summary(pred1)
hist(pred1, n=500); grid()



act_train1 <- as.data.frame(data$CLAIMS_REAL)



# check calibration
(bam1_train_pred <- sum(pred1)) # sum of E(N), not probs
(bam1_train_act <- sum(act_train1))
(bam1_cal_train <- bam1_train_pred/bam1_train_act)

