

Output_lme_actiWatch_nurses_study <- function(acti_watch, post_hoc, completeSubs){
  
  library(nlme)
  library(lsmeans)
  library(ggplot2)
  library(Rmisc)
  library(MuMIn)
  
  ctrl <- lmeControl(opt='optim');
  options(warn=-1)
  
  colnames(acti_watch)[colnames(acti_watch)=="light"] <- "Color"
  colnames(acti_watch)[colnames(acti_watch)=="period"] <- "Condition"

  #acti_watch$Shift <- as.factor(ifelse(as.numeric(acti_watch$subject) > 140, "Night", "Day"))
  
  acti_watch$subject <- as.factor(acti_watch$subject )
  #acti_watch$Color/Condition <- as.factor(acti_watch$Color/Condition )
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
  
  if(completeSubs){
    #########
    
    subList <- acti_watch %>% group_by(subject) %>% filter(n()==6) 
    completeSubs <- unique(subList$subject)
    
    ############
    acti_watch <- acti_watch[acti_watch$subject %in% completeSubs, ]
    
  }
  
  
  duration_model <- lme(Duration ~ Shift*Color*Condition , random = ~1|subject/Color/Condition,
                       data=acti_watch)
  sigList_output1 <- pval_postHoc_OutPut("Duration", duration_model, acti_watch, post_hoc)
  
  
  onsetLatency_model <- lme(Onset_Latency ~ Shift*Color*Condition , random = ~1|subject/Color/Condition,
                        data=acti_watch)
  sigList_output2 <- pval_postHoc_OutPut("Onset Latency", onsetLatency_model, acti_watch, post_hoc)
  
  
  efficiency_model <- lme(Efficiency ~ Shift*Color*Condition , random = ~1|subject/Color/Condition,
                            data=acti_watch)
  sigList_output3 <- pval_postHoc_OutPut("Efficiency", efficiency_model, acti_watch, post_hoc)
  
  
  WASO_model <- lme(WASO ~ Shift*Color*Condition , random = ~1|subject/Color/Condition,
                          data=acti_watch[acti_watch$subject != 134,])
  sigList_output4 <- pval_postHoc_OutPut("WASO", WASO_model, acti_watch, post_hoc)
  
  
  wakeTime_model <- lme(Wake_Time ~ Shift*Color*Condition , random = ~1|subject/Color/Condition,
                    data=acti_watch)
  sigList_output5 <- pval_postHoc_OutPut("Wake time", wakeTime_model, acti_watch, post_hoc)
  
  
  wakepercent_model <- lme(Wake_percent ~ Shift*Color*Condition , random = ~1|subject/Color/Condition,
                        data=acti_watch)
  sigList_output6 <- pval_postHoc_OutPut("Wake percent", wakepercent_model, acti_watch, post_hoc)
  
  
  sleepTime_model <- lme(Sleep_Time ~ Shift*Color*Condition , random = ~1|subject/Color/Condition,
                           data=acti_watch)
  sigList_output7 <- pval_postHoc_OutPut("Sleep Time", sleepTime_model, acti_watch, post_hoc)
  
  
  sleepPercent_model <- lme(Sleep_percent ~ Shift*Color*Condition , random = ~1|subject/Color/Condition,
                         data=acti_watch)
  sigList_output8 <- pval_postHoc_OutPut("Sleep Percent", sleepPercent_model, acti_watch, post_hoc)
  
  
  output_list1 <- list(sigList_output1, sigList_output2, sigList_output3,  sigList_output4, sigList_output5, sigList_output6, sigList_output7, sigList_output8)
  
  return(output_list1)
}


Output_lme_actiWatchNorm_nurses_study <- function(acti_watch, post_hoc, completeSubs){
  
  library(nlme)
  library(lsmeans)
  library(ggplot2)
  library(Rmisc)
  library(MuMIn)
  
  ctrl <- lmeControl(opt='optim');
  options(warn=-1)
  
  colnames(acti_watch)[colnames(acti_watch)=="light"] <- "Color"

  #acti_watch$Shift <- as.factor(ifelse(as.numeric(acti_watch$subject) > 140, "Night", "Day"))
  
  acti_watch$subject <- as.factor(acti_watch$subject )
  #acti_watch$Color <- as.factor(acti_watch$Color )
  acti_watch$Duration <- as.numeric(acti_watch$Duration )
  acti_watch$`Onset Latency` <- as.numeric(acti_watch$`Onset Latency` )
  acti_watch$Efficiency <- as.numeric(acti_watch$Efficiency )
  acti_watch$WASO <- as.numeric(acti_watch$WASO )
  acti_watch$`Wake Time` <- as.numeric(acti_watch$`Wake Time` )
  acti_watch$`%Wake` <- as.numeric(acti_watch$`%Wake`  )
  acti_watch$`Sleep Time` <- as.numeric(acti_watch$`Sleep Time`)
  acti_watch$`%Sleep` <- as.numeric(acti_watch$`%Sleep`)
  colnames(acti_watch)[5] <- "Onset_Latency"
  colnames(acti_watch)[8] <- "Wake_Time"
  colnames(acti_watch)[9] <- "Wake_percent"
  colnames(acti_watch)[10] <- "Sleep_Time"
  colnames(acti_watch)[11] <- "Sleep_percent"
  
  
  
  if(completeSubs){
    #########
    
    subList <- acti_watch %>% group_by(subject) %>% filter(n()==3) 
    completeSubs <- unique(subList$subject)
    
    ############
    acti_watch <- acti_watch[acti_watch$subject %in% completeSubs, ]
    
  }
  
  
  
  duration_model <- lme(Duration ~ Shift*Color, random = ~1|subject/Color,
                        data=acti_watch)
  sigList_output1 <- pval_postHoc_OutPut("Duration", duration_model, acti_watch, post_hoc)
  
  
  onsetLatency_model <- lme(Onset_Latency ~ Shift*Color, random = ~1|subject/Color,
                            data=acti_watch[!is.na(acti_watch$Onset_Latency),])
  sigList_output2 <- pval_postHoc_OutPut("Onset Latency", onsetLatency_model, acti_watch, post_hoc)
  
  
  efficiency_model <- lme(Efficiency ~ Shift*Color, random = ~1|subject/Color,
                          data=acti_watch)
  sigList_output3 <- pval_postHoc_OutPut("Efficiency", efficiency_model, acti_watch, post_hoc)
  
  
  WASO_model <- lme(WASO ~ Shift*Color, random = ~1|subject/Color,
                    data=acti_watch[acti_watch$subject != 134 & !is.na(acti_watch$WASO),])
  sigList_output4 <- pval_postHoc_OutPut("WASO", WASO_model, acti_watch, post_hoc)
  
  
  wakeTime_model <- lme(Wake_Time ~ Shift*Color, random = ~1|subject/Color,
                        data=acti_watch)
  sigList_output5 <- pval_postHoc_OutPut("Wake time", wakeTime_model, acti_watch, post_hoc)
  
  
  wakepercent_model <- lme(Wake_percent ~ Shift*Color, random = ~1|subject/Color,
                           data=acti_watch)
  sigList_output6 <- pval_postHoc_OutPut("Wake percent", wakepercent_model, acti_watch, post_hoc)
  
  
  sleepTime_model <- lme(Sleep_Time ~ Shift*Color, random = ~1|subject/Color,
                         data=acti_watch)
  sigList_output7 <- pval_postHoc_OutPut("Sleep Time", sleepTime_model, acti_watch, post_hoc)
  
  
  sleepPercent_model <- lme(Sleep_percent ~ Shift*Color, random = ~1|subject/Color,
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
    
    
    if('Color' %in% sigList){
      sigList_output2 <- list('Color', lsmeans(nlme_model, pairwise~ Color, adjust="tukey", data = modelData))    
      
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Condition' %in% sigList){
      sigList_output2 <- list('Condition', lsmeans(nlme_model, pairwise~ Condition, adjust="tukey", data = modelData))    
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
    if('Color/Condition' %in% sigList){
      sigList_output2 <- list('Color/Condition', lsmeans(nlme_model, pairwise~ Color/Condition, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Color:TimeBin' %in% sigList){
      sigList_output2 <- list('Color:TimeBin', lsmeans(nlme_model, pairwise~ Color|TimeBin , adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Condition:TimeBin' %in% sigList){
      sigList_output2 <- list('Condition:TimeBin', lsmeans(nlme_model, pairwise~ Condition|TimeBin, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Color:Shift' %in% sigList){
      sigList_output2 <- list('Color:Shift', lsmeans(nlme_model, pairwise~ Color|Shift , adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Condition:Shift' %in% sigList){
      sigList_output2 <- list('Condition:Shift', lsmeans(nlme_model, pairwise~ Condition|Shift  , adjust="tukey", data = modelData))    
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
    if('Color:Condition:TimeBin' %in% sigList){
      sigList_output2 <- list('Color:Condition:TimeBin', lsmeans(nlme_model, pairwise~ Color|Condition|TimeBin  , adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Color:Condition:Shift' %in% sigList){
      sigList_output2 <- list('Color:Condition:Shift', lsmeans(nlme_model, pairwise~ Color|Condition|Shift  , adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        post_hoc_list <- list(post_hoc_list, sigList_output2)
        
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Color:TimeBin:Shift' %in% sigList){
      sigList_output2 <- list('Color:TimeBin:Shift', lsmeans(nlme_model, pairwise~ Color|TimeBin|Shift  , adjust="tukey", data = modelData))    
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
    if('Color:Condition:TimeBin:Shift' %in% sigList){
      sigList_output2 <- list('Color:Condition:TimeBin:Shift', lsmeans(nlme_model, pairwise~ Color|Condition|TimeBin|Shift  , adjust="tukey", data = modelData))    
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

