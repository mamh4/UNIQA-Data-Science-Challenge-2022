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
cl <- makeCluster(6)
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
                  'PERSON_20_REAL',
                  'PERSON_2_REAL',
                  'PERSON_3_REAL',
                  'PERSON_4_REAL')

target <- 'CLAIMS_REAL'

model_result <- 'CURRENT_MODEL_REAL'

# #### Handling class imbalance by SMOTE####
#install.packages("ROSE")
library(ROSE)
table(df_train$CLAIMS_REAL)

#check classes distribution
prop.table(table(df_train$CLAIMS_REAL))


df_train.rose <- ROSE(CLAIMS_REAL ~ ., data = df_train, seed = 1)$data

#check updated classes distribution
prop.table(table(df_train.rose$CLAIMS_REAL))

#df_train <- df_train.rose

# #build decision tree models
# library(rpart)
# tree.rose <- rpart(CLAIMS_REAL ~ ., data = df_train.rose)
# 
# # load test data from file
# load('data/df_test.RData') # => df_test
# 
# #make predictions on unseen data
# > pred.tree.rose <- predict(tree.rose, newdata = df_test)


#### Model Fit ####

predictors <- c(features_cat, features_int, features_num)
predictors <- setdiff(predictors, 'EXPOSURE_REAL') # exposure cannot be used due to strong dependency with loss occurrence
(length(predictors))

formula <- as.formula(paste(target, '~', paste(predictors,collapse=' + ') ))
print(formula)

# start H2O
h2o.init(max_mem_size='8G') # start H2O, Web Interface: http://localhost:54321/flow/index.html

# upload training data to H2O environment
t1 <- Sys.time()
train_hex <- as.h2o(df_train)
t2 <- Sys.time()
print(t2-t1)

# memory management
rm(df_train)
invisible(gc())

train_hex['OFFSET'] <- log(train_hex['EXPOSURE_REAL'])

# Using exposure as offset by adding ln(exposure) on the "right hand side":
#
# ln(y) ~ ln(exposure) + b_1 x_1 + ... + b_n x_n
# <=> y ~ exposure * exp(b_1 x_1 + ... b_n x_n) = exposure * f_1 * ... * f_n, f_i = exp(b_i x_i)
#

# fit GBM model
set.seed(1234)
t1 <- Sys.time()
fit <- h2o.gbm(x=predictors, y=target,
               training_frame = train_hex,
               distribution = 'poisson',
               offset_column = 'OFFSET',
               seed = 1234,
               nfolds=3)
t2 <- Sys.time()
print(t2-t1)

skip <- function() {
  h2o.saveModel(fit, path='models/')

  # load w/o running training again
  fit <- h2o.loadModel('models/H2O_05_pseudo_poisson_full_data/GBM_model_R_1646753408568_1')
}

fit

# calc predictions
pred_train <- predict(fit, newdata = train_hex)
pred_train <- as.data.frame(pred_train)$predict
summary(pred_train)
hist(pred_train, n=50); grid()

# convert to probabilities
# Poisson: E(N) = lambda
#          P(N>=1) = 1-exp(-lambda) = 1-exp(E(N))
#
pred_train_prob <- 1-exp(-pred_train)
summary(pred_train_prob)
hist(pred_train_prob, n=50); grid()

act_train <- as.data.frame(train_hex[target])[,1]

# check calibration
(n_train_pred <- sum(pred_train)) # sum of E(N), not probs
(n_train_act <- sum(act_train))
(cal_train <- n_train_pred/n_train_act)

# variable importance
h2o.varimp_plot(fit,25)
vi <- h2o.varimp(fit)
write.csv(vi, file='var_imp.csv')


#### Compare with existing result ####

my_exposure <- as.data.frame(train_hex['EXPOSURE_REAL'])[[1]]

pred_exist <- train_hex['CURRENT_MODEL_REAL'] # annual figure!
pred_exist <- as.data.frame(pred_exist)[[1]]
pred_exist_exp <- pred_exist * my_exposure # adjusted for exposure
summary(pred_exist_exp)

# scatter plot
smoothScatter(pred_exist_exp, pred_train_prob, 
              xlim=c(0,0.5), ylim=c(0,0.5))

# alternative plots
#install.packages('hexbin')
hexbin::hexbinplot(pred_train_prob ~ pred_exist_exp,
                   xlim=c(0,0.5), ylim=c(0,0.5))

# detailed scatter plot
skip <- function() {
  # quick preview
  n_preview <- 10000
  plot(pred_exist_exp[1:n_preview], pred_train_prob[1:n_preview],
       pch=1, col='#00008020',
       xlim=c(0,0.5), ylim=c(0,0.5))
  grid()
  
  # full plot => takes quite some time!
  plot(pred_exist_exp, pred_train_prob, 
       pch=1, col='#00008020',
       xlim=c(0,0.5), ylim=c(0,0.5))
  grid()

  # full plot, w/o probability transform => takes quite some time!
  plot(pred_exist_exp, pred_train, 
       pch=1, col='#00008020',
       xlim=c(0,0.5), ylim=c(0,0.5))
  grid()
  
}

# correlation our prediction vs existing result
cor(pred_exist_exp, pred_train_prob, method='pearson')
cor(pred_exist_exp, pred_train_prob, method='spearman')

# check w/o probability transform
cor(pred_exist_exp, pred_train, method='pearson')

# compare regression metrics on training data
caret::postResample(act_train, pred_train_prob)
caret::postResample(act_train, pred_exist_exp)

# Area under Curve and ROC curve for new model
pred_train_prob_annual <- pred_train_prob / my_exposure # upscale to same view as existing model
my_pred_train <- prediction(pred_train_prob_annual, act_train)
AUC_train <- performance(my_pred_train, 'auc' )@y.values[[1]]
cat('AUC train new model:', AUC_train)

# ... plot ROC curve
my_roc_curve_train <- performance(my_pred_train,'tpr','fpr')
plot(my_roc_curve_train, main='ROC train')
abline(0,1,col='purple')
grid()

# Area under Curve and ROC curve for existing model
my_pred_train <- prediction(pred_exist, act_train)
AUC_train_exist <- performance(my_pred_train, 'auc' )@y.values[[1]]
cat('AUC train existing model:', AUC_train_exist)

# ... plot ROC curve
my_roc_curve_train <- performance(my_pred_train,'tpr','fpr')
plot(my_roc_curve_train, main='ROC train')
abline(0,1,col='purple')
grid()


#### Check on Test Set ####

# memory management
h2o.rm(train_hex)
invisible(gc())

# load test data from file
load('data/df_test.RData') # => df_test

# upload to H2O
t1 <- Sys.time()
test_hex <- as.h2o(df_test)
t2 <- Sys.time()
print(t2-t1)

test_hex['OFFSET'] <- log(test_hex['EXPOSURE_REAL'])

# calc predictions
pred_test <- predict(fit, newdata = test_hex)
pred_test <- as.data.frame(pred_test)$predict
summary(pred_test)
hist(pred_test, n=50); grid()

# convert to probabilities
pred_test_prob <- 1-exp(-pred_test)
summary(pred_test_prob)
hist(pred_test_prob, n=50); grid()

# compare with existing result
pred_exist_test <- df_test$CURRENT_MODEL_REAL # annual figure!
pred_exist_exp_test <- df_test$CURRENT_MODEL_REAL * df_test$EXPOSURE_REAL # adjusted for exposure
summary(pred_exist_exp_test)

# scatter plot
smoothScatter(pred_exist_exp_test, pred_test_prob,
              xlim=c(0,0.8),ylim=c(0,0.8))

# zoom in
smoothScatter(pred_exist_exp_test, pred_test_prob,
              xlim=c(0,0.5),ylim=c(0,0.5))

# alternative plots
hexbin::hexbinplot(pred_test_prob ~ pred_exist_exp_test,
                   xlim=c(0,0.8),ylim=c(0,0.8))

# correlation our prediction vs existing result
cor(pred_exist_exp_test, pred_test_prob, method='pearson')
cor(pred_exist_exp_test, pred_test_prob, method='spearman')

# check w/o probability transform
cor(pred_exist_exp_test, pred_test, method='pearson')

# detailed scatter plot
skip <- function() {
  # quick preview
  n_preview <- 10000
  plot(pred_exist_exp_test[1:n_preview], pred_test_prob[1:n_preview], 
       pch=1, col='#00008020',
       xlim=c(0,0.8), ylim=c(0,0.8))
  grid()
  
  # full plot => takes quite some time!
  plot(pred_exist_exp_test, pred_test_prob, 
       pch=1, col='#00008020',
       xlim=c(0,0.8), ylim=c(0,0.8))
  grid()
}


# close H2O
h2o.shutdown(prompt=FALSE)

# stop parallel cluster
stopCluster(cl)
