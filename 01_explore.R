#
# (CH) 07.03.2022
#
# Competition Home Page: https://uniqa4ward.com/en/challenge.html
#

rm(list=ls())

suppressWarnings(library(tidyverse))

suppressWarnings(library(doSNOW)) # parallel processing
suppressWarnings(library(caret))
suppressWarnings(library(ROCR)) # for AUC / ROC curve
suppressWarnings(library(readr))
suppressWarnings(library(mgcv))


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

# first time import - training data
skip <- function() {
  
  t1 <- Sys.time()
  df_train <- read.csv('data/DSCH_TRAIN_dataset.csv', sep=';', dec=',')
  t2 <- Sys.time()
  print(t2-t1)
  
  save(df_train, file='df_train.RData')
}

# first time import - test data
skip <- function() {
  
  t1 <- Sys.time()
  df_test <- read.csv('data/DSCH_TEST_dataset.csv', sep=';', dec=',')
  t2 <- Sys.time()
  print(t2-t1)
  
  save(df_test, file='df_test.RData')
}

# fast load:
load('data/df_train.RData') # => df_train
# load('data/df_test.RData') # => df_test

str(df_train, list.len=1000)

(n_train <- nrow(df_train)) # Brackets around would print the value

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

# target is binary:
df_train[,target] <- as.factor(df_train[,target]) #<--- Are you sure??


# conversion to factor
for (v in features_cat) {
  df_train[,v] <- as.factor(df_train[,v])
  # df_test[,v] <- as.factor(df_test[,v])
}


#### Basic summaries ####

summary(df_train[features_cat])

summary(df_train[features_int])

summary(df_train[features_num])


#### Visualize features ####

# categorical / nominal
plot_x_space(1)
for (f in features_cat) {
  plot(df_train[,f], main=f, xlab='', las=2,col=df_train$CLAIMS_REAL)
  grid()
}                        
plot_x_space(0)

# integer features, plot as factors
plot_x_space(1)
for (f in features_int) {
  plot(as.factor(df_train[,f]), main=f, xlab='', las=2,col=df_train$CLAIMS_REAL)
  grid()
}                        
plot_x_space(0)

# numerical features
for (f in features_num) {
  hist(df_train[,f], n=100, main=f, xlab='')
  grid()
}                        

# additional plots for integers with many values
features_int_many <- c('CAR_9_INT',
                       'CAR_15_INT',
                       'CAR_20_INT',
                       'CAR_21_INT',
                       'CAR_31_INT',
                       'PERSON_5_INT',
                       'PERSON_6_INT',
                       'PERSON_21_INT',
                       'PERSON_22_INT',
                       'PERSON_23_INT',
                       'PERSON_30_INT',
                       'PERSON_32_INT',
                       'PERSON_33_INT',
                       'PERSON_34_INT',
                       'PERSON_41_INT',
                       'PERSON_43_INT',
                       'PERSON_44_INT',
                       'PERSON_45_INT',
                       'PERSON_49_INT',
                       'PERSON_52_INT',
                       'PERSON_53_INT',
                       'PERSON_55_INT',
                       'PERSON_57_INT',
                       'PERSON_58_INT')

# integer features with many values, plot histograms (w/o missings)
for (f in features_int_many) {
  foo <- df_train[,f]
  foo <- foo[foo!=-1] # remove missings
  perc_miss <- 1-length(foo) / n_train
  hist(foo, n=100, 
       main=paste0(f, ' - numeric plot - missing(-1): ', 100*round(perc_miss,6), '%'),
       xlab='')
  grid()
}

# same but show frequency in log scale
for (f in features_int_many) {
  foo <- df_train[,f]
  foo <- foo[foo!=-1] # remove missing values first
  perc_miss <- 1-length(foo) / n_train
  h <- hist(foo, n=100, plot=FALSE)
  plot(h$mids, h$counts, type='p', log='y',
       main=paste0(f, ' - numeric plot/y log - missing(-1): ', 100*round(perc_miss,6), '%'),
       xlab='', ylab='Frequency')
  grid()
}

#### Visualize target / model results ####

summary(df_train[,target])
table(df_train[,target]) / n_train
plot(df_train[,target], main='Target'); grid()

summary(df_train[,model_result])
hist(df_train[,model_result], n=100, main='GLM result'); grid()

#### Visualize target vs features ####

# plot target vs categorical features
plot_x_space(0)
for (f in features_cat) {
  my_title <- paste0('Target vs ',f)
  plot(df_train[,f], df_train[,target], las=2, main=my_title,
       ylim=c(0,0.05))
  grid()
}
plot_x_space(0)

# plot target vs integer features
plot_x_space(0)
for (f in features_int) {
  my_title <- paste0('Target vs ',f)
  ymax <- ifelse(f=='CAR_9_INT', 1, 0.05)
  plot(as.factor(df_train[,f]), df_train[,target], las=2, main=my_title,
       ylim=c(0,ymax))
  grid()
}
plot_x_space(0)

# plot target vs numerical features
plot_x_space(0)
for (f in features_num) {
  my_title <- paste0('Target vs ',f)
  qq <- quantile(df_train[,f], probs=seq(0,1,0.02))
  qq <- unique(qq)
  ymax <- ifelse(f=='EXPOSURE_REAL', 0.14, 0.05)
  plot(cut(df_train[,f], qq), df_train[,target],
       xlab='', las=2,
       main=my_title,
       ylim=c(0,ymax))
  grid()
}
plot_x_space(0)

# plot target vs integer features with many values
plot_x_space(0)
for (f in features_int_many) {
  my_title <- paste0('Target vs ',f, ' (numeric)')
  qq <- quantile(df_train[,f], probs=seq(0,1,0.1))
  qq <- unique(qq)
  plot(cut(df_train[,f], qq), df_train[,target],
       xlab='', las=2,
       main=my_title,
       ylim=c(0,0.05))
  grid()
}
plot_x_space(0)

#### Further analytics ####

# numerical version of target
df_train$target_num <- as.numeric(df_train[,target])-1
summary(df_train$target_num)

# target vs exposure - deep dive
plot(cut(df_train$EXPOSURE_REAL, breaks=seq(0,1,0.02)), df_train[,target],
     xlab='', las=2,
     main='Target vs EXPOSURE_REAL',
     ylim=c(0,0.5)); grid()

# model result vs exposure
plot(cut(df_train$EXPOSURE_REAL, breaks=seq(0,1,0.1)), df_train$CURRENT_MODEL_REAL,
     xlab='', las=2,
     main='Model Result vs EXPOSURE_REAL',
     ylim=c(0,0.5)); grid()

# adjust existing model for exposure
summary(df_train$CURRENT_MODEL_REAL) # model result is calibrated to full year exposure!

df_train$CURRENT_MODEL_EXP_ADJ_REAL <- df_train$CURRENT_MODEL_REAL * df_train$EXPOSURE_REAL
summary(df_train$CURRENT_MODEL_EXP_ADJ_REAL)

# model result adjusted for exposure vs exposure
plot(cut(df_train$EXPOSURE_REAL, breaks=seq(0,1,0.1)), df_train$CURRENT_MODEL_EXP_ADJ_REAL,
     xlab='', las=2,
     main='Model Result exposure adj. vs EXPOSURE_REAL',
     ylim=c(0,0.5)); grid()


#------------
#model_1 <- gam(CLAIMS_REAL~POLICY_DATA_7_INT + CAR_1_INT ,offset = log(EXPOSURE_REAL), data = df_train)

#family = poisson
#bam just diff algorithm quicker




# stop parallel cluster
stopCluster(cl)
