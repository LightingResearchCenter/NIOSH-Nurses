

Output_lme_actiWatch_nurses_study <- function(PVT, OB, GNG, post_hoc){
  
  library(nlme)
  library(lsmeans)
  library(ggplot2)
  library(Rmisc)
  library(MuMIn)
  
  ctrl <- lmeControl(opt='optim');
  options(warn=-1)
  
  
  
  ###GNG
  
  GNG$Condition_period <- paste(GNG$Condition, GNG$Period, sep = "_")
  
  GNG$SubjectID <- as.factor(GNG$SubjectID )
  GNG$Shift <- as.factor(GNG$Shift )
  GNG$Condition <- as.factor(GNG$Condition )
  GNG$Period <- as.factor(GNG$Period )
  GNG$Condition_period <- as.factor(GNG$Condition_period )
  GNG$TimeBin <- as.factor(GNG$TimeBin )
  
  GNG$avgResponseTimeCorrect <- as.numeric(GNG$avgResponseTimeCorrect )
  GNG$HitRate <- as.numeric(GNG$HitRate )
  GNG$FalsePositive <- as.numeric(GNG$FalsePositive )
  
 
  
  GNG2 <- subset(GNG, ValidPairedBin == "TRUE" & ValidTest == "TRUE" & !is.na(TimeBin))
  
  
  hitrate_model <- lme(HitRate ~ Condition_period * TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                       data=GNG2)
  
  
  sigList_output1 <- pval_postHoc_OutPut("GNG Hits", hitrate_model, GNG2, post_hoc)
  
  
  fp_rate_model <- lme(FalsePositive ~ Condition_period * TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                       data=GNG2)
  
  sigList_output2 <- pval_postHoc_OutPut("GNG false positve", fp_rate_model, GNG2, post_hoc)
  
  
  gng_rt_model <- lme(avgResponseTimeCorrect ~ Condition_period * TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                       data=GNG2)
  
  sigList_output3 <- pval_postHoc_OutPut("GNG response time", gng_rt_model, GNG2, post_hoc)
  
  gng_rt_model2 <- lme(avgResponseTime ~ Condition_period * TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                      data=GNG2)
  
  sigList_output4 <- pval_postHoc_OutPut("GNG response time", gng_rt_model, GNG2, post_hoc)
  
  ##OB
  
  OB$Condition_period <- paste(OB$Condition, OB$Period, sep = "_")
  
  
  OB$SubjectID <- as.factor(OB$SubjectID )
  OB$Shift <- as.factor(OB$Shift )
  OB$Condition <- as.factor(OB$Condition )
  OB$Period <- as.factor(OB$Period )
  OB$Condition_period <- as.factor(OB$Condition_period )
  
  OB$TimeBin <- as.factor(OB$TimeBin )
  
  OB$avgResponseTimeCorrect <- as.numeric(OB$avgResponseTimeCorrect )
  OB$CorrectMatch <- as.numeric(OB$CorrectMatch )
  OB$CorrectNoMatch <- as.numeric(OB$CorrectNoMatch )
  
  
  
  OB2 <- subset(OB, ValidPairedBin == "TRUE" & isValid == "TRUE" & !is.na(TimeBin))
  
  OB_half <-subset(OB2, HalfAnswered == "TRUE" )
  OB_normal <-subset(OB2, HalfAnswered == "FALSE" )
  

  
  
  correct_match_OBnormal <- lme(CorrectMatch ~ Condition_period* TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                       data=OB_normal)
  
  
  sigList_output5 <- pval_postHoc_OutPut("OB correct matches", correct_match_OBnormal, OB_normal, post_hoc)
  
  
  correct_NoMatch_OBnormal <- lme(CorrectNoMatch ~ Condition_period * TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                                data=OB_normal)
  
  
  sigList_output6 <- pval_postHoc_OutPut("OB no correct matches", correct_NoMatch_OBnormal, OB_normal, post_hoc)
  
  
  OB_rt_model <- lme(avgResponseTimeCorrect ~ Condition_period * TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                      data=OB_normal[!is.na(OB_normal$avgResponseTimeCorrect),])
  
  sigList_output7 <- pval_postHoc_OutPut("OB response time", OB_rt_model, OB_normal[!is.na(OB_normal$avgResponseTimeCorrect),], post_hoc)
  
  OB_rt_model2 <- lme(avgResponseTime~ Condition_period * TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                     data=OB_normal[!is.na(OB_normal$avgResponseTime),])
  
  sigList_output8 <- pval_postHoc_OutPut("OB response time", OB_rt_model2, OB_normal[!is.na(OB_normal$avgResponseTime),], post_hoc)
  
  
  
  
  
  
  correct_match_OBnormal <- lme(CorrectMatch ~ Condition_period* TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                                data=OB2)
  
  
  sigList_output9 <- pval_postHoc_OutPut("OB correct matches", correct_match_OBnormal, OB, post_hoc)
  
  
  correct_NoMatch_OBnormal <- lme(CorrectNoMatch ~ Condition_period * TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                                  data=OB2)
  
  
  sigList_output10 <- pval_postHoc_OutPut("OB no correct matches", correct_NoMatch_OBnormal, OB2, post_hoc)
  
  
  OB_rt_model <- lme(avgResponseTimeCorrect ~ Condition_period* TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                     data=OB2[!is.na(OB2$avgResponseTimeCorrect),])
  
  sigList_output11 <- pval_postHoc_OutPut("OB response time", OB_rt_model, OB_normal[!is.na(OB_normal$avgResponseTimeCorrect),], post_hoc)
  
  OB_rt_model2 <- lme(avgResponseTime~ Condition_period * TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                      data=OB2[!is.na(OB2$avgResponseTime),])
  
  sigList_output12 <- pval_postHoc_OutPut("OB response time", OB_rt_model2, OB_normal[!is.na(OB_normal$avgResponseTime),], post_hoc)
  
  
  
  #PVT
  
  PVT$Condition_period <- paste(PVT$Condition, PVT$Period, sep = "_")
  
  
  PVT$SubjectID <- as.factor(PVT$SubjectID )
  PVT$Shift <- as.factor(PVT$Shift )
  PVT$Condition <- as.factor(PVT$Condition )
  PVT$Period <- as.factor(PVT$Period )
  PVT$Condition_period <- as.factor(PVT$Condition_period )
  
  PVT$TimeBin <- as.factor(PVT$TimeBin )
  
  PVT$avgResponseTime <- as.numeric(PVT$avgResponseTime )
  PVT$accuracy <- PVT$nCorrect/PVT$nTrials

  
  PVT2 <- subset(PVT, ValidPairedBin == "TRUE" & ValidTest == "TRUE" & !is.na(TimeBin))
  
  correct_PVT <- lme(accuracy ~ Condition_period* TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                                data=PVT2)
  
  
  sigList_output13 <- pval_postHoc_OutPut("PVT correct", correct_PVT, PVT2, post_hoc)
  
  

  
  
  PVT_rt_model <- lme(avgResponseTime ~ Condition_period* TimeBin *Shift, random = ~1|SubjectID/Condition/Period,
                     data=PVT2)
  
  sigList_output14 <- pval_postHoc_OutPut("PVT response time", PVT_rt_model, PVT2, post_hoc)
  
  
  
  
  
  output_list1 <- list(sigList_output1, sigList_output2, sigList_output3,  sigList_output4, sigList_output5, sigList_output6, sigList_output7, sigList_output8, sigList_output9, sigList_output10,
                       sigList_output11, sigList_output12, sigList_output13, sigList_output14)
  
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

