add_to_models <- function(x){

  
  t1 <- Sys.time()
  
  #Step 0 # Calculate variables
  pred <-predict(x,df_train,type = "response")#*df_train$EXPOSURE_REAL
  
  pred_prob <- 1-exp(-pred)
  
  pred_annual <- pred_prob #/ df_train$EXPOSURE_REAL
  
  pred_train <- prediction(as.numeric(pred_annual),as.numeric(df_train$CLAIMS_REAL))
  #pred_train <- prediction(as.numeric(pred_prob),as.numeric(df_train$CLAIMS_REAL))
  final_pred <- ifelse(pred_train@predictions[[1]]<optimalCutoff(df_train$CLAIMS_REAL,pred_train@predictions[[1]]) ,yes = 0,no = 1)
  
  
  
  paste("==============",print(Sys.time()),"===============")
  paste("start sourcing Main")
  
  #source("main.R")
  paste("==============",print(Sys.time()),"===============")
  
  model_file = paste0('models_db/',deparse(substitute(x)),'.RData')
  
  if(file.exists(model_file)){
     print("Warning! model already exists!!")
  }else{
    # Step 1 # Save the model
    print('=============== add model to models ==============')
    paste("==============",print(Sys.time()),"===============")
    save(x, file= paste0('./models/',deparse(substitute(x)),'.RData'))
    print('============= add model to models DONE ===========')
    paste("==============",print(Sys.time()),"===============")    
    
    
    # Step 2 # Save the model prediction to predictions
    print('========== add prediction to predictions =========')
    paste("==============",print(Sys.time()),"===============")
    save(pred, file= paste0('./predictions/',deparse(substitute(x)),'_pred.RData'))
    print('======== add prediction to predictions DONE ======')
    paste("==============",print(Sys.time()),"===============")   
    
    
    
    #Step 3 # Read models db
    print('=============== Reading models db ================')
    paste("==============",print(Sys.time()),"===============")
    load('models_db/db.RData') #//
    print('============= Reading modelS db DONE =============')
    paste("==============",print(Sys.time()),"===============")
    
    
    #Step 4 Add to models db
    print('================= add model to db ================')
    paste("==============",print(Sys.time()),"===============")
    model_result <- c(deparse(substitute(x)),
                      toString(x$pred.formula),
                      toString(x$method),
                      expected_div_actual_claims = sum((predict(x,df_train,type = "response")*df_train$EXPOSURE_REAL)) / sum(df_train$CLAIMS_REAL),
                      model_auc = performance(pred_train,'auc')@y.values[[1]],
                      model_precision = (table(final_pred,df_train$CLAIMS_REAL)[2,2] + table(final_pred,df_train$CLAIMS_REAL)[1,1] )/ sum(table(final_pred,df_train$CLAIMS_REAL)),
                      conf_TP = table(final_pred,df_train$CLAIMS_REAL)[2,2],
                      conf_TN = table(final_pred,df_train$CLAIMS_REAL)[1,1],
                      conf_FN = table(final_pred,df_train$CLAIMS_REAL)[1,2],
                      conf_FP = table(final_pred,df_train$CLAIMS_REAL)[2,1],
                      conf_total = sum(table(final_pred,df_train$CLAIMS_REAL))
                      )
    
    db[nrow(db) + 1,] <- model_result
    save(db, file='./models_db/db.RData')
    load('./models_db/db.RData')
    print('============== add model to db DONE ==============')
    paste("==============",print(Sys.time()),"===============")
    
    # Calculate time taken
    t2 <- Sys.time()
    print("================== TIME TAKEN ====================")
    paste("=================",print(t2-t1),"=================")
    
    }
  
}
