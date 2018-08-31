
  library(readxl)
  source('C:/Users/roohac/Desktop/NureseStudyReportMaterials/exportMeansPval.R', echo=TRUE)
  
  

  
  
  acti_watch <- read_excel("//root/projects/NIOSH_RedLightForShiftWorkers/Actiware data/Sleep-analysis-data_8-17-18.xlsx")

  acti_watch2 <- acti_watch

  library(nlme)
  library(lsmeans)
  library(ggplot2)
  library(Rmisc)
  library(MuMIn)
  library(emmeans)
  
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
  
  
  completeSubs <- TRUE
  if(completeSubs){
    #########
    
    subList <- acti_watch %>% group_by(subject) %>% filter(n()==6) 
    completeSubs <- unique(subList$subject)
    
    ############
    acti_watch <- acti_watch[acti_watch$subject %in% completeSubs, ]
    
  }
  
  
  
  duration_model <- lme(Duration ~ Shift*Color*Condition , random = ~1|subject/Color/Condition,
                       data=acti_watch)

  anova(duration_model)
  
  emmeans(duration_model, pairwise~ Condition:Shift, adjust="none")
  
  acti_watch$Shift_Condition <- paste(acti_watch$Shift, acti_watch$Condition, sep = "_")
  acti_watchDursum1 <- aggregate(Duration ~ Shift + Condition + Shift_Condition + subject, data = acti_watch, FUN = mean)
  duration_conditionShift <- summarySE(data =acti_watchDursum1,  measurevar = "Duration", groupvars = c("Condition", "Shift", "Shift_Condition"))
  
  


  comparisonList0   <-  createComparisons(unique(acti_watch$Shift_Condition))

  for(i in 1:length(comparisonList0)){
    currComparison <-  strsplit(comparisonList0[i], "-")

    tTest_Data <- acti_watchDursum1[acti_watchDursum1$Shift_Condition == currComparison[[1]][1] | acti_watchDursum1$Shift_Condition == currComparison[[1]][2],]
    
    ##check if data is paired or unpaired
    subs <- tTest_Data %>% group_by(subject) %>% filter(n()==2) 
    if(subs > 0){
      paired <- TRUE
      t_test100 <- t.test(Duration ~ Shift_Condition, data = returnComplete2(tTest_Data), var.equal = TRUE, paired = TRUE) 
      
    }else{
      t_test100 <- t.test(Duration ~ Shift_Condition, data = tTest_Data, var.equal = TRUE) 
    }
    
    
    t_test<- t_test100
    Compare <- c()
    t <- c()
    df <- c()
    p_value <- c()
    if(t_test$p.value[[1]] < .06){
      Compare[i]<- comparisonList0[i]
      t[i] <-  round(t_test$statistic[[1]], digits = 6)
      df[i] <- round(t_test$parameter[[1]], digits = 6)
      p_value[i] <-  round(t_test$p.value[[1]], digits = 6)
    }
    t_testTable <- data.frame(Compare, t, df, p_value)
    ############
    
  }
  
  
  
 t_test1 <- t.test(Duration ~ Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Shift_Condition == "Day_intervention" | acti_watchDursum1$Shift_Condition == "Day_baseline",]), var.equal = TRUE, paired = TRUE) 
 t_test10 <-list("Day_intervention, Day_baseline", t_test1)
 
 t_test2 <- t.test(Duration ~ Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Shift_Condition == "Night_intervention" | acti_watchDursum1$Shift_Condition == "Night_baseline",]), var.equal = TRUE, paired = TRUE) 
 t_test20 <-list("Night_intervention, Night_baseline", t_test2)
 
 t_test3 <- t.test(Duration ~ Shift, data = acti_watchDursum1[acti_watchDursum1$Shift_Condition == "Day_baseline" | acti_watchDursum1$Shift_Condition == "Night_baseline",], var.equal = TRUE) 
 t_test30 <-list("Day_baseline, Night_baseline", t_test3)
 
 t_test4 <- t.test(Duration ~ Shift, data = acti_watchDursum1[acti_watchDursum1$Shift_Condition == "Day_intervention" | acti_watchDursum1$Shift_Condition == "Night_intervention",], var.equal = TRUE) 
 t_test40 <-list("Day_intervention, Night_intervention", t_test4)
 
 
 t_test5 <- t.test(Duration ~ Shift, data = acti_watchDursum1[acti_watchDursum1$Shift_Condition == "Day_baseline" | acti_watchDursum1$Shift_Condition == "Night_intervention",], var.equal = TRUE) 
 t_test50 <-list("Day_baseline, Night_intervention", t_test5)
 
 t_test6 <- t.test(Duration ~ Shift, data = acti_watchDursum1[acti_watchDursum1$Shift_Condition == "Day_intervention" | acti_watchDursum1$Shift_Condition == "Night_baseline",], var.equal = TRUE) 
 t_test60 <-list("Day_intervention, Night_baseline", t_test6)
 
 
 t_testList <- list(t_test10, t_test20, t_test30, t_test40, t_test50, t_test60)
 

  
  exportMeansPval2(duration_conditionShift, "All-Subjects-Sleep-Duration-ShiftXCondition", t_testList)  
  
  

  
  ####COlor X Condition
  emmeans(duration_model, pairwise~ Color:Condition, adjust="none")
  
  acti_watch$Color_Condition <- paste(acti_watch$Color, acti_watch$Condition, sep = "_")
  acti_watchDursum1 <- aggregate(Duration ~ Color + Condition + Color_Condition + subject, data = acti_watch, FUN = mean)
  duration_conditionColor <- summarySE(data =acti_watchDursum1,  measurevar = "Duration", groupvars = c("Condition", "Color", "Color_Condition"))
  
  
  unique(acti_watch$Color_Condition)
  t_test1 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_baseline" | acti_watchDursum1$Color_Condition == "red_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test10 <-list("red_baseline, red_intervention", t_test1)
  
  t_test2 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "blue_baseline" | acti_watchDursum1$Color_Condition == "blue_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test20 <-list("blue_baseline, blue_intervention", t_test2)
  
  #***
  t_test3 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "white_baseline" | acti_watchDursum1$Color_Condition == "white_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test30 <-list("white_baseline, white_intervention", t_test3)
  
  t_test4 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_baseline" | acti_watchDursum1$Color_Condition == "blue_baseline",]), var.equal = TRUE, paired = TRUE) 
  t_test40 <-list("red_baseline, blue_baseline", t_test4)
  
  t_test5 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_baseline" | acti_watchDursum1$Color_Condition == "white_baseline",]), var.equal = TRUE, paired = TRUE) 
  t_test50 <-list("red_baseline, white_baseline", t_test5)
  
  t_test6 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "white_baseline" | acti_watchDursum1$Color_Condition == "blue_baseline",]), var.equal = TRUE, paired =  TRUE) 
  t_test60 <-list("white_baseline, blue_baseline", t_test6)
  
  t_test7 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_intervention" | acti_watchDursum1$Color_Condition == "blue_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test70 <-list("red_intervention, blue_intervention", t_test7)
  
  t_test8 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_intervention" | acti_watchDursum1$Color_Condition == "white_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test80 <-list("red_intervention, white_intervention", t_test8)
  
  #***
  t_test9 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "white_intervention" | acti_watchDursum1$Color_Condition == "blue_intervention",]), var.equal = TRUE, paired =  TRUE) 
  t_test90 <-list("white_intervention, blue_intervention", t_test9)
  
  
  
  t_test10 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_baseline" | acti_watchDursum1$Color_Condition == "blue_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test100 <-list("red_baseline, blue_intervention", t_test10)
  
  t_test11 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_baseline" | acti_watchDursum1$Color_Condition == "white_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test110 <-list("red_baseline, white_baseline", t_test11)
  
  t_test12 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "blue_baseline" | acti_watchDursum1$Color_Condition == "red_intervention",]), var.equal = TRUE, paired =  TRUE) 
  t_test120 <-list("white_baseline, blue_baseline", t_test12)
  
  t_test13 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "blue_baseline" | acti_watchDursum1$Color_Condition == "white_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test130 <-list("red_intervention, blue_intervention", t_test13)
  
  t_test14 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "white_baseline" | acti_watchDursum1$Color_Condition == "red_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test140 <-list("red_intervention, white_intervention", t_test14)
  
  t_test15 <- t.test(Duration ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "white_baseline" | acti_watchDursum1$Color_Condition == "blue_intervention",]), var.equal = TRUE, paired =  TRUE) 
  t_test150 <-list("white_intervention, blue_intervention", t_test15)
  
  
  
  
  t_testList <- list(t_test10, t_test20, t_test30, t_test40, t_test50, t_test60,  t_test70,  t_test80,  t_test90)
  
  
  
  exportMeansPval2(duration_conditionColor, "Completed-Subjects-Sleep-Duration-ColorXCondition", t_testList)  
  
  exportMeansPval(duration_conditionColor, "Completed-Subjects-Sleep-Duration-ColorXCondition", t_test3)  
  
  #####

  
  
  sleepTime_model <- lme(Sleep_Time ~ Shift*Color*Condition , random = ~1|subject/Color/Condition,
                           data=acti_watch)

  anova(sleepTime_model)
  
  acti_watchSTsum1 <- aggregate(Sleep_Time ~ Shift + Condition + Shift_Condition + subject, data = acti_watch, FUN = mean)
  Sleep_Time_conditionShift <- summarySE(data =acti_watchSTsum1,  measurevar = "Sleep_Time", groupvars = c("Condition", "Shift", "Shift_Condition"))
  
  


  t_test1 <- t.test(Sleep_Time ~ Condition, data = returnComplete2(acti_watchSTsum1[acti_watchSTsum1$Shift_Condition == "Day_intervention" | acti_watchSTsum1$Shift_Condition == "Day_baseline",]), var.equal = TRUE, paired = TRUE) 
  t_test10 <-list("Day_intervention, Day_baseline", t_test1)
  
  t_test2 <- t.test(Sleep_Time ~ Condition, data = returnComplete2(acti_watchSTsum1[acti_watchSTsum1$Shift_Condition == "Night_intervention" | acti_watchSTsum1$Shift_Condition == "Night_baseline",]), var.equal = TRUE, paired = TRUE) 
  t_test20 <-list("Night_intervention, Night_baseline", t_test2)
  
  t_test3 <- t.test(Sleep_Time ~ Shift, data = acti_watchSTsum1[acti_watchSTsum1$Shift_Condition == "Day_baseline" | acti_watchSTsum1$Shift_Condition == "Night_baseline",], var.equal = TRUE) 
  t_test30 <-list("Day_baseline, Night_baseline", t_test3)
  
  t_test4 <- t.test(Sleep_Time ~ Shift, data = acti_watchSTsum1[acti_watchSTsum1$Shift_Condition == "Day_intervention" | acti_watchSTsum1$Shift_Condition == "Night_intervention",], var.equal = TRUE) 
  t_test40 <-list("Day_intervention, Night_intervention", t_test4)
  
  
  t_test5 <- t.test(Sleep_Time ~ Shift, data = acti_watchSTsum1[acti_watchSTsum1$Shift_Condition == "Day_baseline" | acti_watchSTsum1$Shift_Condition == "Night_intervention",], var.equal = TRUE) 
  t_test50 <-list("Day_baseline, Night_intervention", t_test5)
  
  t_test6 <- t.test(Sleep_Time ~ Shift, data = acti_watchSTsum1[acti_watchSTsum1$Shift_Condition == "Day_intervention" | acti_watchSTsum1$Shift_Condition == "Night_baseline",], var.equal = TRUE) 
  t_test60 <-list("Day_intervention, Night_baseline", t_test6)
  
  
  t_testList <- list(t_test10, t_test20, t_test30, t_test40, t_test50, t_test60)
  
  
  
  exportMeansPval2(Sleep_Time_conditionShift, "Completed-Subjects-Sleep-Time-ShiftXCondition", t_testList)  
  
  
  acti_watchDursum1 <- aggregate(Sleep_Time ~ Color + Condition + Color_Condition + subject, data = acti_watch, FUN = mean)
  Sleep_Time_conditionColor <- summarySE(data =acti_watchDursum1,  measurevar = "Sleep_Time", groupvars = c("Condition", "Color", "Color_Condition"))
  
  
  unique(acti_watch$Color_Condition)
  t_test1 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_baseline" | acti_watchDursum1$Color_Condition == "red_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test10 <-list("red_baseline, red_intervention", t_test1)
  
  t_test2 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "blue_baseline" | acti_watchDursum1$Color_Condition == "blue_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test20 <-list("blue_baseline, blue_intervention", t_test2)
  

#***  
  t_test3 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "white_baseline" | acti_watchDursum1$Color_Condition == "white_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test30 <-list("white_baseline, white_intervention", t_test3)
  
  t_test4 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_baseline" | acti_watchDursum1$Color_Condition == "blue_baseline",]), var.equal = TRUE, paired = TRUE) 
  t_test40 <-list("red_baseline, blue_baseline", t_test4)
  
  t_test5 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_baseline" | acti_watchDursum1$Color_Condition == "white_baseline",]), var.equal = TRUE, paired = TRUE) 
  t_test50 <-list("red_baseline, white_baseline", t_test5)
  
  t_test6 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "white_baseline" | acti_watchDursum1$Color_Condition == "blue_baseline",]), var.equal = TRUE, paired =  TRUE) 
  t_test60 <-list("white_baseline, blue_baseline", t_test6)
  
  t_test7 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_intervention" | acti_watchDursum1$Color_Condition == "blue_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test70 <-list("red_intervention, blue_intervention", t_test7)
  
  t_test8 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_intervention" | acti_watchDursum1$Color_Condition == "white_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test80 <-list("red_intervention, white_intervention", t_test8)
  
#****
  t_test9 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "white_intervention" | acti_watchDursum1$Color_Condition == "blue_intervention",]), var.equal = TRUE, paired =  TRUE) 
  t_test90 <-list("white_intervention, blue_intervention", t_test9)
  
  
  
  t_test10 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_baseline" | acti_watchDursum1$Color_Condition == "blue_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test100 <-list("red_baseline, blue_intervention", t_test10)
  
  t_test11 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "red_baseline" | acti_watchDursum1$Color_Condition == "white_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test110 <-list("red_baseline, white_baseline", t_test11)
  
  t_test12 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "blue_baseline" | acti_watchDursum1$Color_Condition == "red_intervention",]), var.equal = TRUE, paired =  TRUE) 
  t_test120 <-list("white_baseline, blue_baseline", t_test12)
  
  t_test13 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "blue_baseline" | acti_watchDursum1$Color_Condition == "white_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test130 <-list("red_intervention, blue_intervention", t_test13)
  
  t_test14 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "white_baseline" | acti_watchDursum1$Color_Condition == "red_intervention",]), var.equal = TRUE, paired = TRUE) 
  t_test140 <-list("red_intervention, white_intervention", t_test14)
  
  t_test15 <- t.test(Sleep_Time ~ Color_Condition, data = returnComplete2(acti_watchDursum1[acti_watchDursum1$Color_Condition == "white_baseline" | acti_watchDursum1$Color_Condition == "blue_intervention",]), var.equal = TRUE, paired =  TRUE) 
  t_test150 <-list("white_intervention, blue_intervention", t_test15)
  
  
  
  
  t_testList <- list(t_test10, t_test20, t_test30, t_test40, t_test50, t_test60,  t_test70,  t_test80,  t_test90)
  
  
  
  exportMeansPval2(Sleep_Time_conditionColor, "Completed-Subjects-Sleep_Time-ColorXCondition", t_testList)  
  
  exportMeansPval(Sleep_Time_conditionColor, "Completed-Subjects--Sleep_Time-ColorXCondition", t_test3)  
  
  
  


  source('~/GitHub/NIOSH-Nurses/normalizeActi-watch.R', echo=TRUE)
  
  acti_watch2 <- NormalizeAcitwatch(acti_watch2)
  
  
  colnames(acti_watch2)[colnames(acti_watch2)=="light"] <- "Color"

  #acti_watch2$Shift <- as.factor(ifelse(as.numeric(acti_watch2$subject) > 140, "Night", "Day"))
  
  acti_watch2$subject <- as.factor(acti_watch2$subject )
  #acti_watch2$Color <- as.factor(acti_watch2$Color )
  acti_watch2$Duration <- as.numeric(acti_watch2$Duration )
  acti_watch2$`Onset Latency` <- as.numeric(acti_watch2$`Onset Latency` )
  acti_watch2$Efficiency <- as.numeric(acti_watch2$Efficiency )
  acti_watch2$WASO <- as.numeric(acti_watch2$WASO )
  acti_watch2$`Wake Time` <- as.numeric(acti_watch2$`Wake Time` )
  acti_watch2$`%Wake` <- as.numeric(acti_watch2$`%Wake`  )
  acti_watch2$`Sleep Time` <- as.numeric(acti_watch2$`Sleep Time`)
  acti_watch2$`%Sleep` <- as.numeric(acti_watch2$`%Sleep`)
  colnames(acti_watch2)[5] <- "Onset_Latency"
  colnames(acti_watch2)[8] <- "Wake_Time"
  colnames(acti_watch2)[9] <- "Wake_percent"
  colnames(acti_watch2)[10] <- "Sleep_Time"
  colnames(acti_watch2)[11] <- "Sleep_percent"
  
  
  duration_model <- lme(Duration ~ Shift*Color, random = ~1|subject/Color,
                        data=acti_watch2)

  anova(duration_model)
  
  
  durationNorm_conditionShift <- summarySE(data =acti_watch2,  measurevar = "Sleep_Time", groupvars = c( "Shift"))
  
  t0test <- t.test(Duration ~ Shift, data = acti_watch2,  var.equal = TRUE)
  
  exportMeansPval(durationNorm_conditionShift, "All-Subjects-Norm-Sleep-Duration-Shift", t0test)  
  
  
  
  
  sleepTime_model <- lme(Sleep_Time ~ Shift*Color, random = ~1|subject/Color,
                         data=acti_watch2)

  
  anova(sleepTime_model)
  
  
  Sleep_TimeNorm_conditionShift <- summarySE(data =acti_watch2,  measurevar = "Sleep_Time", groupvars = c( "Shift"))
  
  t0test <- t.test(Sleep_Time ~ Shift, data = acti_watch2,  var.equal = TRUE)
  
  exportMeansPval(Sleep_TimeNorm_conditionShift, "All-Subjects-Norm-Sleep_Time-Shift", t0test)  
  
  
  
  
  

