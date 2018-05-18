

Output_lme_actiWatch_nurses_study <- function(acti_watch, post_hoc){
  
  library(nlme)
  library(lsmeans)
  library(ggplot2)
  library(Rmisc)
  library(MuMIn)
  
  ctrl <- lmeControl(opt='optim');
  options(warn=-1)
  

  acti_watch$Condition_period <- paste(acti_watch$light, acti_watch$period, sep = "_")
  acti_watch$Shift <- as.factor(ifelse(as.numeric(acti_watch$subject) > 140, "Night", "Day"))
  
  acti_watch$subject <- as.factor(acti_watch$subject )
  acti_watch$Condition_period <- as.factor(acti_watch$Condition_period )
  acti_watch$Duration <- as.numeric(acti_watch$Duration )
  acti_watch$`Onset Latency` <- as.numeric(acti_watch$`Onset Latency` )
  acti_watch$Efficiency <- as.numeric(acti_watch$Efficiency )
  acti_watch$WASO <- as.numeric(acti_watch$WASO )
  acti_watch$`Wake Time` <- as.numeric(acti_watch$`Wake Time` )
  acti_watch$`%Wake` <- as.numeric(acti_watch$`%Wake`  )
  acti_watch$`Sleep Time` <- as.numeric(acti_watch$`Sleep Time`)
  acti_watch$`%Sleep` <- as.numeric(acti_watch$`%Sleep`)
  colnames(acti_watch)[8] <- "Onset_Latency"
  colnames(acti_watch)[11] <- "Wake_Time"
  colnames(acti_watch)[12] <- "Wake_percent"
  colnames(acti_watch)[13] <- "Sleep_Time"
  colnames(acti_watch)[14] <- "Sleep_percent"
  
  
  duration_model <- lme(Duration ~ Condition_period*Shift , random = ~1|subject/Condition_period,
                       data=acti_watch)
  sigList_output1 <- pval_postHoc_OutPut("Duration", duration_model, acti_watch, post_hoc)
  
  
  onsetLatency_model <- lme(Onset_Latency ~ Condition_period*Shift , random = ~1|subject/Condition_period,
                        data=acti_watch)
  sigList_output2 <- pval_postHoc_OutPut("Onset Latency", onsetLatency_model, acti_watch, post_hoc)
  
  
  efficiency_model <- lme(Efficiency ~ Condition_period*Shift , random = ~1|subject/Condition_period,
                            data=acti_watch)
  sigList_output3 <- pval_postHoc_OutPut("Efficiency", efficiency_model, acti_watch, post_hoc)
  
  
  WASO_model <- lme(WASO ~ Condition_period*Shift , random = ~1|subject/Condition_period,
                          data=acti_watch)
  sigList_output4 <- pval_postHoc_OutPut("WASO", WASO_model, acti_watch, post_hoc)
  
  
  wakeTime_model <- lme(Wake_Time ~ Condition_period*Shift , random = ~1|subject/Condition_period,
                    data=acti_watch)
  sigList_output5 <- pval_postHoc_OutPut("Wake time", wakeTime_model, acti_watch, post_hoc)
  
  
  wakepercent_model <- lme(Wake_percent ~ Condition_period*Shift , random = ~1|subject/Condition_period,
                        data=acti_watch)
  sigList_output6 <- pval_postHoc_OutPut("Wake percent", wakepercent_model, acti_watch, post_hoc)
  
  
  sleepTime_model <- lme(Sleep_Time ~ Condition_period*Shift , random = ~1|subject/Condition_period,
                           data=acti_watch)
  sigList_output7 <- pval_postHoc_OutPut("Sleep Time", sleepTime_model, acti_watch, post_hoc)
  
  
  sleepPercent_model <- lme(Sleep_percent ~ Condition_period*Shift , random = ~1|subject/Condition_period,
                         data=acti_watch)
  sigList_output8 <- pval_postHoc_OutPut("Sleep Percent", sleepPercent_model, acti_watch, post_hoc)
  
  
  output_list1 <- list(sigList_output1, sigList_output2, sigList_output3,  sigList_output4, sigList_output5, sigList_output6, sigList_output7, sigList_output8)
  
  return(output_list1)
}


pval_postHoc_OutPut <- function(outcomemeasureTitle, nlme_model, modelData, post_hoc){
  
  model_r2 <- r.squaredGLMM(nlme_model)
  
  modelPvals <- data.frame(anova(nlme_model))
  sigList <- rownames(modelPvals[modelPvals$p.value < .05,])
  sigList_output <- list(outcomemeasureTitle,model_r2, modelPvals)
  if(post_hoc){
    post_hoc_list <- list()
    
    if(length(sigList) <= 1){
      sigList_output2 <- list(paste("No significance found in ", outcomemeasureTitle, ". Therefore no post-hoc tests."))
      post_hoc_list <- sigList_output2
      
    }
    
    
    if('Condition' %in% sigList){
      sigList_output2 <- list('Condition', lsmeans(nlme_model, pairwise~ Condition, adjust="tukey", data = modelData))    
      
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Period' %in% sigList){
      sigList_output2 <- list('Period', lsmeans(nlme_model, pairwise~ Period, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('TimeBin' %in% sigList){
      sigList_output2 <- list('TimeBin', lsmeans(nlme_model, pairwise~ TimeBin, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Shift' %in% sigList){
      sigList_output2 <- list('Shift', lsmeans(nlme_model, pairwise~ Shift, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Condition_period' %in% sigList){
      sigList_output2 <- list('Condition_period', lsmeans(nlme_model, pairwise~ Condition_period, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Condition:TimeBin' %in% sigList){
      sigList_output2 <- list('Condition:TimeBin', lsmeans(nlme_model, pairwise~ Condition|TimeBin , adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Period:TimeBin' %in% sigList){
      sigList_output2 <- list('Period:TimeBin', lsmeans(nlme_model, pairwise~ Period|TimeBin, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Condition:Shift' %in% sigList){
      sigList_output2 <- list('Condition:Shift', lsmeans(nlme_model, pairwise~ Condition|Shift , adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Period:Shift' %in% sigList){
      sigList_output2 <- list('Period:Shift', lsmeans(nlme_model, pairwise~ Period|Shift  , adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('TimeBin:Shift' %in% sigList){
      sigList_output2 <- list('TimeBin:Shift', lsmeans(nlme_model, pairwise~ TimeBin|Shift  , adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Condition:Period:TimeBin' %in% sigList){
      sigList_output2 <- list('Condition:Period:TimeBin', lsmeans(nlme_model, pairwise~ Condition|Period|TimeBin  , adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Condition:Period:Shift' %in% sigList){
      sigList_output2 <- list('Condition:Period:Shift', lsmeans(nlme_model, pairwise~ Condition|Period|Shift  , adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Condition:TimeBin:Shift' %in% sigList){
      sigList_output2 <- list('Condition:TimeBin:Shift', lsmeans(nlme_model, pairwise~ Condition|TimeBin|Shift  , adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Period:TimeBin:Shift' %in% sigList){
      sigList_output2 <- list('Period:TimeBin:Shift', lsmeans(nlme_model, pairwise~ Period|TimeBin|Shift  , adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Condition:Period:TimeBin:Shift' %in% sigList){
      sigList_output2 <- list('Condition:Period:TimeBin:Shift', lsmeans(nlme_model, pairwise~ Condition|Period|TimeBin|Shift  , adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    sigList_output <- list(sigList_output, post_hoc_list)
    return(sigList_output)  }else{
      return(sigList_output) 
      
    }
  
  
}

