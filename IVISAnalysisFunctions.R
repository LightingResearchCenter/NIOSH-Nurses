

Output_lme_ISIV_nurses_study <- function(ISIV, post_hoc){
  
  library(nlme)
  library(lsmeans)
  library(ggplot2)
  library(Rmisc)
  library(MuMIn)
  
  ctrl <- lmeControl(opt='optim');
  options(warn=-1)
  

  ISIV$Condition_period <- paste(ISIV$light, ISIV$period, sep = "_")
  ISIV$Shift <- as.factor(ifelse(as.numeric(ISIV$subject) > 140, "Night", "Day"))
  
  ISIV$subject <- as.factor(ISIV$subject )
  ISIV$Condition_period <- as.factor(ISIV$Condition_period )

  
  IV <- lme(IV ~ Condition_period*Shift , random = ~1|subject/Condition_period,
                       data=ISIV)
  sigList_output1 <- pval_postHoc_OutPut("IV", IV, ISIV, post_hoc)
  
  
  IS <- lme(IS ~ Condition_period*Shift , random = ~1|subject/Condition_period,
                        data=ISIV)
  
  
  IS <- lme(IS ~ light*period*Shift , random = ~1|subject/Condition_period,
            data=ISIV)
  
  lsmeans(IS, pairwise~ Shift*light*period , adjust="tukey", data = ISIV)
  
  sigList_output2 <- pval_postHoc_OutPut("IS", IS, ISIV, post_hoc)
  
  
  
  output_list1 <- list(sigList_output1, sigList_output2)
  
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

