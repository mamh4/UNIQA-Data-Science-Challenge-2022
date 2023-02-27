bin <- function(variables, bins, dataset, model){
  
  for(i in 1:length(variables)){
    
    pred <- predict(model, newdata = dataset, type = "terms", terms = paste0("s(",variables[i],")"))
    
    dt_pred <- tibble(dataset[,paste(variables[i])], as.vector(pred))
    names(dt_pred) <- c(paste(variables[i]),paste0(variables[i],"_pred"))
    
    num_bins = bins[i]
    
    classint_fisher <-  classIntervals(
      dt_pred %>% as.data.frame() %>% .[,paste0(variables[i],"_pred")],
      num_bins,
      style = "fisher")
    
    dataset <-  left_join(dataset, unique(dt_pred), by = paste(variables[i]))
    
    dataset[,paste0(variables[i],"_bin")] <-  as.factor(cut(dataset[,paste0(variables[i],"_pred")],
                                                            breaks = classint_fisher$brks, right = FALSE,
                                                            include.lowest = TRUE, dig.lab = 2))
    print(paste(paste0(variables[i])," is now binned!"))
    paste("==============",print(Sys.time()),"===============")
  }
  return(dataset)
}
