
eliminateOutlier_missingData <- function(first_data, outcomeMeasureName){
  
}
Output_lme_performance_nurses_study <- function(PVT, OB, GNG, post_hoc, completeSubs){
  
  if(TRUE){
    ##renaming columns to be consistent with actiwatch data
    colnames(PVT)[colnames(PVT)=="Condition"] <- "Color"
    colnames(PVT)[colnames(PVT)=="Period"] <- "Condition"
    
    colnames(GNG)[colnames(GNG)=="Condition"] <- "Color"
    colnames(GNG)[colnames(GNG)=="Period"] <- "Condition"
    
    colnames(OB)[colnames(OB)=="Condition"] <- "Color"
    colnames(OB)[colnames(OB)=="Period"] <- "Condition"
  }

  
  
  library(nlme)
  library(lsmeans)
  library(ggplot2)
  library(Rmisc)
  library(MuMIn)
  library(dplyr)
  
  ctrl <- lmeControl(opt='optim');
  options(warn=-1)
  
  
  ctrl <- lmeControl(opt='optim')
  ###GNG
  
  GNG$Color_Condition <- paste(GNG$Color, GNG$Condition, sep = "_")
  
  GNG$SubjectID <- as.factor(GNG$SubjectID )
  GNG$Shift <- as.factor(GNG$Shift )
  GNG$Color <- as.factor(GNG$Color )
  GNG$Condition <- as.factor(GNG$Condition )
  GNG$Color_Condition <- as.factor(GNG$Color_Condition )
  GNG$TimeBin <- as.factor(GNG$TimeBin )
  
  GNG$avgResponseTimeCorrect <- as.numeric(GNG$avgResponseTimeCorrect )
  GNG$HitRate <- as.numeric(GNG$HitRate )
  GNG$FalsePositive <- as.numeric(GNG$FalsePositive )
  
 
  
  #GNG2 <- subset(GNG, ValidPairedBin == "TRUE" & ValidTest == "TRUE" & !is.na(TimeBin))
  #GNG2 <- subset(GNG,ValidTest == "TRUE" & !is.na(TimeBin))
  GNG2 <- subset(GNG,!is.na(TimeBin) & !is.na(avgResponseTimeCorrect) & !is.na(avgResponseTime) & HasMaxTrials == "TRUE" & Color_Condition != "NA_NA" & ValidPairedBin == "TRUE" & ValidPairedBin == "TRUE")
  
  #eliminate Outlier
  highRT <- mean(GNG2$avgResponseTimeCorrect) + sd(GNG2$avgResponseTimeCorrect) *2
  hightFP <- mean(GNG2$FalsePositive) + sd(GNG2$FalsePositive) *2
  
  lowHit <- mean(GNG2$HitRate) - sd(GNG2$HitRate) *2
    
  
  
  GNG3 <- subset(GNG2, HitRate > lowHit &  FalsePositive < hightFP & avgResponseTimeCorrect < highRT)
  if(completeSubs){
    #########
    
    GNGsum <- aggregate(HitRate ~ SubjectID + Shift + Color + Condition + TimeBin, FUN = mean, data = GNG3)
    GNGsum <- GNGsum %>% group_by(SubjectID) %>% filter(n()==18) 
    completeSubs <- unique(GNGsum$SubjectID)
    
    ############
    GNG3 <- GNG3[GNG3$SubjectID %in% completeSubs, ]
    
  }

  
  
  hitrate_model <- lme(HitRate ~ Shift * Color * Condition  *  TimeBin,random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                       data=GNG3)
  
  
  sigList_output1 <- pval_postHoc_OutPut("GNG Hits", hitrate_model, GNG2, post_hoc)
  
  
  

  
  fp_rate_model <- lme(FalsePositive ~ Shift * Color * Condition  *  TimeBin,random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                       data=GNG3)
  
  sigList_output2 <- pval_postHoc_OutPut("GNG false positve", fp_rate_model, GNG2, post_hoc)
  
  

  
  gng_rt_model <- lme(avgResponseTimeCorrect ~ Shift * Color * Condition  *  TimeBin,random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                       data=GNG3)
  
  sigList_output3 <- pval_postHoc_OutPut("GNG mean response time", gng_rt_model, GNG2, post_hoc)
  
  gng_rt_model2 <- lme(medResponseTimeCorrect ~ Shift * Color * Condition  *  TimeBin,random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                      data=GNG3)
  
  sigList_output4 <- pval_postHoc_OutPut("GNG median response time", gng_rt_model2, GNG2, post_hoc)
  
  ##OB
  
  OB$Color_Condition <- paste(OB$Color, OB$Condition, sep = "_")
  
  
  OB$SubjectID <- as.factor(OB$SubjectID )
  OB$Shift <- as.factor(OB$Shift )
  OB$Color <- as.factor(OB$Color )
  OB$Condition <- as.factor(OB$Condition )
  OB$Color_Condition <- as.factor(OB$Color_Condition )
  
  OB$TimeBin <- as.factor(OB$TimeBin )
  
  OB$avgResponseTimeCorrect <- as.numeric(OB$avgResponseTimeCorrect )
  OB$CorrectMatch <- as.numeric(OB$CorrectMatch )
  OB$CorrectNoMatch <- as.numeric(OB$CorrectNoMatch )
  
  
  
  #eliminate Outlier
  #lowAcc <- mean(OB_normal$PercentCorrect) - sd(OB_normal$PercentCorrect) *2
  
  
  
  #OB2 <- subset(OB, ValidPairedBin == "TRUE" & isValid == "TRUE" & !is.na(TimeBin))
  #OB2 <- subset(OB,isValid == "TRUE" & !is.na(TimeBin))
  OB2 <- subset(OB,!is.na(TimeBin) & !is.na(avgResponseTimeCorrect) & !is.na(avgResponseTime) & HasMaxTrials == "TRUE" & Color_Condition != "NA_NA" & ValidPairedBin == "TRUE" )
  
  highRTob <- mean(OB2$avgResponseTimeCorrect) + sd(OB2$avgResponseTimeCorrect) *2
  
  OB_half <-subset(OB2, avgResponseTimeCorrect < highRTob & HalfAnswered == "TRUE" )
  OB_normal <-subset(OB2, avgResponseTimeCorrect < highRTob & HalfAnswered == "FALSE"& avgResponseTimeCorrectMatch < 2.5  )
  
  if(completeSubs){
    #########
    
    OBsum <- aggregate(PercentCorrect ~ SubjectID + Shift + Color + Condition + TimeBin, FUN = mean, data = OB_normal)
    OBsum <- OBsum %>% group_by(SubjectID) %>% filter(n()==18) 
    completeSubs <- unique(OBsum$SubjectID)
    
    ############
    OB_normal <- OB_normal[OB_normal$SubjectID %in% completeSubs, ]
    
  }
  
  
  correct_accuracy_OBnormal <- lme(PercentCorrect ~ Shift * Color * Condition  *  TimeBin, random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                                data=OB_normal)
  sigList_output5 <- pval_postHoc_OutPut("OB accuracy", correct_accuracy_OBnormal, OB_normal, post_hoc)
  
  
  correct_match_OBnormal <- lme(CorrectMatch ~ Shift * Color * Condition  *  TimeBin, random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                       data=OB_normal)
  sigList_output6 <- pval_postHoc_OutPut("OB correct matches", correct_match_OBnormal, OB_normal, post_hoc)
  
  
  correct_NoMatch_OBnormal <- lme(CorrectNoMatch ~ Shift * Color * Condition  *  TimeBin,random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                                data=OB_normal)
  sigList_output7 <- pval_postHoc_OutPut("OB no correct matches", correct_NoMatch_OBnormal, OB_normal, post_hoc)
  
  
  OB_rt_model <- lme(avgResponseTimeCorrect ~ Shift * Color * Condition  *  TimeBin,random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                      data=OB_normal[!is.na(OB_normal$avgResponseTimeCorrect),])
  sigList_output8 <- pval_postHoc_OutPut("OB mean response time", OB_rt_model, OB_normal[!is.na(OB_normal$avgResponseTimeCorrect),], post_hoc)
  
  
  
  OB_rt_model2 <- lme(medResponseTimeCorrect~ Shift * Color * Condition  *  TimeBin,random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                     data=OB_normal[!is.na(OB_normal$medResponseTimeCorrect),])
  sigList_output9 <- pval_postHoc_OutPut("OB median response time", OB_rt_model2, OB_normal[!is.na(OB_normal$medResponseTimeCorrect),], post_hoc)
  
  
  OB_rt_model3 <- lme(avgResponseTimeCorrectMatch ~ Shift * Color * Condition  *  TimeBin,random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                     data=OB_normal[!is.na(OB_normal$avgResponseTimeCorrectMatch),])
  sigList_output10 <- pval_postHoc_OutPut("OB mean response time, correct match", OB_rt_model3, OB_normal[!is.na(OB_normal$avgResponseTimeCorrectMatch),], post_hoc)
  
  OB_rt_model4<- lme(avgResponseTimeCorrectNoMatch ~ Shift * Color * Condition  *  TimeBin,random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                      data=OB_normal[!is.na(OB_normal$avgResponseTimeCorrectNoMatch),])
  sigList_output11 <- pval_postHoc_OutPut("OB mean response time, correct no match", OB_rt_model4, OB_normal[!is.na(OB_normal$avgResponseTimeCorrectNoMatch),], post_hoc)
  
  
  if(FALSE){
    correct_accuracy_OBnormal <- lme(nCorrect ~ Shift * Color * Condition  *  TimeBin, random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                                     data=OB2)
    sigList_output5 <- pval_postHoc_OutPut("OB accuracy", correct_accuracy_OBnormal, OB_normal, post_hoc)
    
    
    
    correct_match_OBnormal <- lme(CorrectMatch ~ Shift * Color * Condition  *  TimeBin, random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                                  data=OB2)
    sigList_output9 <- pval_postHoc_OutPut("OB correct matches", correct_match_OBnormal, OB, post_hoc)
    
    
    correct_NoMatch_OBnormal <- lme(CorrectNoMatch ~ Shift * Color * Condition  *  TimeBin,random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                                    data=OB2)
    sigList_output10 <- pval_postHoc_OutPut("OB no correct matches", correct_NoMatch_OBnormal, OB2, post_hoc)
    
    
    OB_rt_model <- lme(avgResponseTimeCorrect ~ Shift * Color * Condition  *  TimeBin, random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                       data=OB2[!is.na(OB2$avgResponseTimeCorrect),])
    sigList_output11 <- pval_postHoc_OutPut("OB response time", OB_rt_model, OB_normal[!is.na(OB_normal$avgResponseTimeCorrect),], post_hoc)
    
    #OB_rt_model2 <- lme(avgResponseTime~ Shift * Color * Condition  *  TimeBin,random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, ,
    #                    data=OB2[!is.na(OB2$avgResponseTime),])
    
    #sigList_output12 <- pval_postHoc_OutPut("OB response time", OB_rt_model2, OB_normal[!is.na(OB_normal$avgResponseTime),], post_hoc)
    
    
  }
  
 
  
  
  #PVT
  
  PVT$Color_Condition <- paste(PVT$Color, PVT$Condition, sep = "_")
  
  
  PVT$SubjectID <- as.factor(PVT$SubjectID )
  PVT$Shift <- as.factor(PVT$Shift )
  PVT$Color <- as.factor(PVT$Color )
  PVT$Condition <- as.factor(PVT$Condition )
  PVT$Color_Condition <- as.factor(PVT$Color_Condition )
  
  PVT$TimeBin <- as.factor(PVT$TimeBin )
  
  PVT$avgResponseTime <- as.numeric(PVT$avgResponseTime )
  PVT$accuracy <- PVT$nCorrect/PVT$nTrials

  
  
  #PVT2 <- subset(PVT, ValidPairedBin == "TRUE" & ValidTest == "TRUE" & !is.na(TimeBin))
  #PVT2 <- subset(PVT,ValidTest == "TRUE" & !is.na(TimeBin))
  PVT2 <- subset(PVT, avgResponseTime < .7 & nCorrect > 20 &  !is.na(TimeBin) & !is.na(avgResponseTime) & HasMaxTrials == "TRUE" & Color_Condition != "NA_NA" & ValidPairedBin == "TRUE")
  
  if(completeSubs){
    #########
    
    PVTsum <- aggregate(nCorrect ~ SubjectID + Shift + Color + Condition + TimeBin, FUN = mean, data = PVT)
    PVTsum <- PVTsum %>% group_by(SubjectID) %>% filter(n()==18) 
    completeSubs <- unique(PVTsum$SubjectID)
    
    ############
    PVT2 <- PVT2[PVT2$SubjectID %in% completeSubs, ]
    
  }
  
  correct_PVT <- lme(accuracy ~ Shift * Color * Condition  *  TimeBin, random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                                data=PVT2)
  
  
  sigList_output13 <- pval_postHoc_OutPut("PVT correct", correct_PVT, PVT2, post_hoc)
  
  

  
  
  PVT_rt_model <- lme(avgResponseTime ~ Shift * Color * Condition  *  TimeBin, random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                     data=PVT2)
  
  sigList_output14 <- pval_postHoc_OutPut("PVT mean response time", PVT_rt_model, PVT2, post_hoc)
  
  
  PVT_rt_model2 <- lme(medResponseTime ~ Shift * Color * Condition  *  TimeBin, random = ~1|SubjectID/Color/Condition/TimeBin, control=ctrl, 
                      data=PVT2)
  
  sigList_output15 <- pval_postHoc_OutPut("PVT median response time", PVT_rt_model2, PVT2, post_hoc)
  
  
  
  
  output_list1 <- list(sigList_output1, sigList_output2, sigList_output3,  sigList_output4, sigList_output5, sigList_output6, sigList_output7, sigList_output8, sigList_output9, sigList_output10, sigList_output11, sigList_output13, sigList_output14, sigList_output15)
  
  return(output_list1)
}


pval_postHoc_OutPut <- function(outcomemeasureTitle, nlme_model, modelData, post_hoc){
  library(dplyr)
  post_hoc2 <- FALSE
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
    if('Color_Condition' %in% sigList){
      sigList_output2 <- list('Color_Condition', lsmeans(nlme_model, pairwise~ Color_Condition, adjust="tukey", data = modelData))    
      if(length(post_hoc_list) > 0){
        
        if(post_hoc2){
          sumDataOBrt <- aggregate(avgResponseTimeCorrect ~ SubjectID + Color_Condition + Color + Condition, data =   OB_normal[!is.na(OB_normal$avgResponseTimeCorrect),], FUN = mean)
          OB_avgResponseTimeCorrect3 <- summarySE(sumDataOBrt, measurevar="avgResponseTimeCorrect", groupvars=c("Color_Condition", "Color", "Condition"))
          
          

          
          ggplot(OB_avgResponseTimeCorrect3, aes(x=Color_Condition, y=avgResponseTimeCorrect, fill = Color_Condition)) + 
            geom_bar(position=position_dodge(), stat="identity") +
            geom_errorbar(aes(ymin=avgResponseTimeCorrect-se, ymax=avgResponseTimeCorrect+se),
                          width=.2,                    # Width of the error bars
                          position=position_dodge(.9)) + # facet_grid(.~ Color) +
            coord_cartesian(ylim=c(.5,1))+
            geom_text(aes(label = round(avgResponseTimeCorrect, digits = 3), vjust=2))+
            scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
            geom_signif(comparisons = list(c("Red baseline", "Red intervention")), annotations=".003")
          
          
          
          
          redOBrt <- sumDataOBrt[sumDataOBrt$Color == "Red",]
          redOBrt2 <-  redOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
          
          blueOBrt <- sumDataOBrt[sumDataOBrt$Color == "Blue",]
          blueOBrt2 <-  blueOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
          
          whiteOBrt <- sumDataOBrt[sumDataOBrt$Color == "White",]
          whiteOBrt2 <-  whiteOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
          
          t.test(avgResponseTimeCorrect ~ Color_Condition, data = redOBrt2, paired = TRUE)        
          t.test(avgResponseTimeCorrect ~ Color_Condition, data = blueOBrt2, paired = TRUE)
          t.test(avgResponseTimeCorrect ~ Color_Condition, data = whiteOBrt2, paired = TRUE)
          
          wilcox.test(avgResponseTimeCorrect ~ Color_Condition, data = redOBrt2, paired = TRUE) 
          wilcox.test(avgResponseTimeCorrect ~ Color_Condition, data = blueOBrt2, paired = TRUE) 
          wilcox.test(avgResponseTimeCorrect ~ Color_Condition, data = whiteOBrt2, paired = TRUE) 
          
          ##Using medians for individual tests
          sumDataOBrt2 <- aggregate(medResponseTimeCorrect ~ SubjectID + Color_Condition + Color + Condition, data =   OB_normal[!is.na(OB_normal$medResponseTimeCorrect),], FUN = mean)
          
          redOBrt <- sumDataOBrt2[sumDataOBrt2$Color == "Red",]
          redOBrt2 <-  redOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
          
          blueOBrt <- sumDataOBrt2[sumDataOBrt2$Color == "Blue",]
          blueOBrt2 <-  blueOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
          
          whiteOBrt <- sumDataOBrt2[sumDataOBrt2$Color == "White",]
          whiteOBrt2 <-  whiteOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
          
          t.test(medResponseTimeCorrect ~ Color_Condition, data = redOBrt2, paired = TRUE)        
          t.test(medResponseTimeCorrect ~ Color_Condition, data = blueOBrt2, paired = TRUE)
          t.test(medResponseTimeCorrect ~ Color_Condition, data = whiteOBrt2, paired = TRUE)
          
          wilcox.test(medResponseTimeCorrect ~ Color_Condition, data = redOBrt2, paired = TRUE) 
          wilcox.test(medResponseTimeCorrect ~ Color_Condition, data = blueOBrt2, paired = TRUE) 
          wilcox.test(medResponseTimeCorrect ~ Color_Condition, data = whiteOBrt2, paired = TRUE)
          
          ###Using medians of means
          sumDataOBrt <- aggregate(avgResponseTimeCorrect ~ SubjectID + Color_Condition + Color, data =   OB_normal[!is.na(OB_normal$avgResponseTimeCorrect),], FUN = median)
          
          redOBrt <- sumDataOBrt[sumDataOBrt$Color == "Red",]
          redOBrt2 <-  redOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
          
          blueOBrt <- sumDataOBrt[sumDataOBrt$Color == "Blue",]
          blueOBrt2 <-  blueOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
          
          whiteOBrt <- sumDataOBrt[sumDataOBrt$Color == "White",]
          whiteOBrt2 <-  whiteOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
          
          t.test(avgResponseTimeCorrect ~ Color_Condition, data = redOBrt2, paired = TRUE)        
          t.test(avgResponseTimeCorrect ~ Color_Condition, data = blueOBrt2, paired = TRUE)
          t.test(avgResponseTimeCorrect ~ Color_Condition, data = white2OBrt2, paired = TRUE)
          
          wilcox.test(avgResponseTimeCorrect ~ Color_Condition, data = redOBrt2, paired = TRUE) 
          wilcox.test(avgResponseTimeCorrect ~ Color_Condition, data = blueOBrt2, paired = TRUE) 
          wilcox.test(avgResponseTimeCorrect ~ Color_Condition, data = whiteOBrt2, paired = TRUE) 
          
          
          ###medians of medians
          
          sumDataOBrt <- aggregate(medResponseTimeCorrect ~ SubjectID + Color_Condition + Color, data =   OB_normal[!is.na(OB_normal$medResponseTimeCorrect),], FUN = median)
          
          redOBrt <- sumDataOBrt[sumDataOBrt$Color == "Red",]
          redOBrt2 <-  redOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
          
          blueOBrt <- sumDataOBrt[sumDataOBrt$Color == "Blue",]
          blueOBrt2 <-  blueOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
          
          whiteOBrt <- sumDataOBrt[sumDataOBrt$Color == "White",]
          whiteOBrt2 <-  whiteOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
          
          t.test(medResponseTimeCorrect ~ Color_Condition, data = redOBrt2, paired = TRUE)        
          t.test(medResponseTimeCorrect ~ Color_Condition, data = blueOBrt2, paired = TRUE)
          t.test(medResponseTimeCorrect ~ Color_Condition, data = white2OBrt2, paired = TRUE)
          
          wilcox.test(medResponseTimeCorrect ~ Color_Condition, data = redOBrt2, paired = TRUE) 
          wilcox.test(medResponseTimeCorrect ~ Color_Condition, data = blueOBrt2, paired = TRUE) 
          wilcox.test(medResponseTimeCorrect ~ Color_Condition, data = whiteOBrt2, paired = TRUE) 
          
          
          OB_medResponseTimeCorrect3 <- summarySE2(sumDataOBrt, measurevar="medResponseTimeCorrect", groupvars=c("Color_Condition", "Color"))
          
          
          ggplot(OB_medResponseTimeCorrect3, aes(x=Color_Condition, y=`medResponseTimeCorrect median`, fill = Color_Condition)) + 
            geom_bar(position=position_dodge(), stat="identity") +
            geom_errorbar(aes(ymin=`medResponseTimeCorrect median`-se, ymax=`medResponseTimeCorrect median`+se),
                          width=.2,                    # Width of the error bars
                          position=position_dodge(.9)) +
            coord_cartesian(ylim=c(.5,1))+
            geom_text(aes(label = round(`medResponseTimeCorrect median`, digits = 3), vjust=2))+
            scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90")) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
          
          
          
          ####PVT
          
          
          sumData <- aggregate(avgResponseTimeCorrect ~ SubjectID + Color_Condition + Color, data =  OB2, FUN = mean)
          sumData <- aggregate(accuracy ~ SubjectID + Color_Condition + Color, data =  PVT2, FUN = mean)
          sumData <- aggregate(accuracy ~ SubjectID + Color_Condition + Color, data =  PVT2, FUN = median)
          
          sumData$accuracy <- log(sumData$accuracy) 
          red <- sumData[sumData$Color == "Red",]
          red2 <-  red %>% group_by(SubjectID) %>% filter(n()>1) 
          
          blue <- sumData[sumData$Color == "Blue",]
          blue2 <-  blue %>% group_by(SubjectID) %>% filter(n()>1) 
          
          white <- sumData[sumData$Color == "White",]
          white2 <-  white %>% group_by(SubjectID) %>% filter(n()>1) 
          
          t.test(accuracy ~ Color_Condition, data = red2, paired = TRUE)        
          t.test(accuracy ~ Color_Condition, data = blue2, paired = TRUE)
          t.test(accuracy ~ Color_Condition, data = white2, paired = TRUE)
          
          wilcox.test(accuracy ~ Color_Condition, data = red2, paired = TRUE) 
          wilcox.test(accuracy ~ Color_Condition, data = blue2, paired = TRUE) 
          wilcox.test(accuracy ~ Color_Condition, data = white2, paired = TRUE) 
          
          sumData <- aggregate(avgResponseTimeCorrect ~ SubjectID + Color_Condition + Color, data =   OB_normal[!is.na(OB_normal$avgResponseTimeCorrect),], FUN = mean)
          
          red <- sumData[sumData$Color == "Red",]
          red2 <-  red %>% group_by(SubjectID) %>% filter(n()>1) 
          
          blue <- sumData[sumData$Color == "Blue",]
          blue2 <-  blue %>% group_by(SubjectID) %>% filter(n()>1) 
          
          white <- sumData[sumData$Color == "White",]
          white2 <-  white %>% group_by(SubjectID) %>% filter(n()>1) 
          
          t.test(avgResponseTimeCorrect ~ Color_Condition, data = red2, paired = TRUE)        
          t.test(avgResponseTimeCorrect ~ Color_Condition, data = blue2, paired = TRUE)
          t.test(avgResponseTimeCorrect ~ Color_Condition, data = white2, paired = TRUE)
          
          
          sumData <- aggregate(avgResponseTimeCorrect ~ SubjectID + Color_Condition + Color, data =  OB2, FUN = mean)
          sumData <- aggregate(avgResponseTimeCorrect ~ SubjectID + Color_Condition + Color, data =  OB_normal, FUN = mean)
          
          red <- sumData[sumData$Color == "r",]
          red2 <-  red %>% group_by(SubjectID) %>% filter(n()>1) 
          
          blue <- sumData[sumData$Color == "b",]
          blue2 <-  blue %>% group_by(SubjectID) %>% filter(n()>1) 
          
          white <- sumData[sumData$Color == "w",]
          white2 <-  white %>% group_by(SubjectID) %>% filter(n()>1) 
          
          t.test(avgResponseTimeCorrect ~ Color_Condition, data = red2, paired = TRUE)        
          t.test(avgResponseTimeCorrect ~ Color_Condition, data = blue2, paired = TRUE)
          t.test(avgResponseTimeCorrect ~ Color_Condition, data = white2, paired = TRUE)
          
        }
       
        
        post_hoc_list <- list(post_hoc_list, sigList_output2)
      }else{
        post_hoc_list <- sigList_output2
        
      }
      
    }
    if('Color_Condition:TimeBin:Shift' %in% sigList){
      sigList_output2 <- list('Color_Condition:TimeBin:Shift', lsmeans(nlme_model, pairwise~ Color_Condition|Shift|TimeBin , adjust="tukey", data = modelData))    
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

