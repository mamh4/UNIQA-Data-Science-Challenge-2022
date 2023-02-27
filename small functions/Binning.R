set.seed(123)

data <- df_train[sample(nrow(df_train),5000),]

#data[data=="000"] <- NA
#data[data==-1] <- NA

bam_binned <- mgcv::bam(formula = formula_sp_curr,
                                   data = data, family = "poisson",offset = log(data$EXPOSURE_REAL))

pred <-  predict(bam_binned, newdata = data, type = "terms", terms = "s(INS_HISTORY_24_REAL)")

dt_pred <- tibble(INS = data$INS_HISTORY_24_REAL, pred)

dt_pred <- dplyr::arrange(dt_pred,INS)

test <- unique(dt_pred)

library(classInt)

num_bins = 10

classint_fisher <-  classIntervals(
  dt_pred$pred,
  num_bins,
  style = "fisher")

classint_fisher$brks

data <- data %>% select(INS_HISTORY_24_REAL)

data <-  left_join(x = data, y = test, by = c("INS_HISTORY_24_REAL"="INS"))


data$bin <-  as.factor(cut(data$pred,
                            breaks = classint_fisher$brks, right = FALSE,
                            include.lowest = TRUE, dig.lab = 2))


head(data)

##################################################################







pred_binned <- predict(bam_binned, data,"response")*data$EXPOSURE_REAL

pred_prob_1000 <- 1-exp(-pred_1000)

pred_annual_1000 <- pred_prob_1000 / data$EXPOSURE_REAL

pred_train_1000 <- prediction(as.numeric(pred_annual_1000),as.numeric(data$CLAIMS_REAL))

final_pred_1000 <- ifelse(pred_train_1000@predictions[[1]] < optimalCutoff(data$CLAIMS_REAL,pred_train_1000@predictions[[1]]) ,
                          yes = 0,
                          no = 1)



(t1<- Sys.time())
df_train_bins <- bin(features_num,num_bins,df_train[1:5000,],bam_base_sp_curr)
(t2<-Sys.time())
(t2-t1)

(t1<- Sys.time())
df_train_bins <- bin(features_num,num_bins,df_train[1:100000,],bam_base_sp_curr)
(t2<-Sys.time())
(t2-t1)


