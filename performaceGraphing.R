



library(readxl)


PVT <- read_excel("//root/public/roohac/nurses_study/completed-nurses/full_data_sets/processedtime2/Nurses_Study_Summary_Tables_2018_02_21_15_23_37_Complete.xlsx", 
                  sheet = "PVT")

GNG <- read_excel("//root/public/roohac/nurses_study/completed-nurses/full_data_sets/processedtime2/Nurses_Study_Summary_Tables_2018_02_21_15_23_37_Complete.xlsx", 
                  sheet = "GNG")

OB <- read_excel("//root/public/roohac/nurses_study/completed-nurses/full_data_sets/processedtime2/Nurses_Study_Summary_Tables_2018_02_21_15_23_37_Complete.xlsx", 
                 sheet = "OB")

  
  library(nlme)
  library(lsmeans)
  library(ggplot2)
  source('~/GitHub/NIOSH-Nurses/SummarySE2.R', echo=TRUE) 
  library(Rmisc)
  library(MuMIn)
  library(ggsignif)
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
  GNG$Condition <- ifelse(GNG$Condition == "r", "Red", 
                          ifelse(GNG$Condition == "b", "Blue", "White" ))
  GNG$Period <- ifelse(GNG$Period == "b","Baseline", "Intervention")
  GNG$Condition_period <- ifelse(GNG$Condition_period == "b_b", "Blue baseline", 
                                 ifelse(GNG$Condition_period == "b_i", "Blue intervention", 
                                        ifelse(GNG$Condition_period == "r_b", "Red baseline", 
                                               ifelse(GNG$Condition_period == "r_i", "Red intervention", 
                                                      ifelse(GNG$Condition_period == "w_b", "White baseline", "White intervention" )))))
  GNG$Condition_period <- factor(GNG$Condition_period, levels = c("Red baseline", "Red intervention", "Blue baseline", "Blue intervention", "White baseline", "White intervention"   ))
  
  
  #GNG2 <- subset(GNG, ValidPairedBin == "TRUE" & ValidTest == "TRUE" & !is.na(TimeBin))
  #GNG2 <- subset(GNG,ValidTest == "TRUE" & !is.na(TimeBin))
  GNG2 <- subset(GNG,!is.na(TimeBin) & !is.na(avgResponseTimeCorrect) & !is.na(avgResponseTime) & HasMaxTrials == "TRUE")
  
  
  ###GNG hit
  
  gng_hit_subs1 <- summarySE(data = GNG2, measurevar="HitRate", groupvars=c("SubjectID", "Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  gng_hit_subs2 <- summarySE(GNG2, measurevar="HitRate", groupvars=c("SubjectID", "Condition_period",  "Shift", "Period", "Condition"))
  
  
  gng_hit1 <- summarySE(gng_hit_subs1, measurevar="HitRate", groupvars=c("Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  gng_hit2 <- summarySE(gng_hit_subs2, measurevar="HitRate", groupvars=c("Condition_period", "Shift", "Condition", "Period"))
  
  
  ggplot(gng_hit1, aes(x=Period, y=HitRate, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=HitRate-se, ymax=HitRate+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(Shift ~Condition*TimeBin) + #facet_grid(.~Condition*Period) +
    coord_cartesian(ylim=c(19,21))+
    geom_text(aes(label = round(HitRate, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
    
  
  ggplot(gng_hit2, aes(x=Period, y=HitRate, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=HitRate-se, ymax=HitRate+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(.~Shift* Condition) +
    coord_cartesian(ylim=c(19,21))+
    geom_text(aes(label = round(HitRate, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
    
 
  ##GNG false postive
  
  
  gng_fp_subs1 <- summarySE(GNG2, measurevar="FalsePositive", groupvars=c("SubjectID", "Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  gng_fp_subs2 <- summarySE(GNG2, measurevar="FalsePositive", groupvars=c("SubjectID", "Condition_period",  "Shift", "Period", "Condition"))

  
  gng_fp1 <- summarySE(gng_fp_subs1, measurevar="FalsePositive", groupvars=c("Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  gng_fp2 <- summarySE(gng_fp_subs2, measurevar="FalsePositive", groupvars=c("Condition_period", "Shift", "Condition", "Period"))
  
  #df1 <- graph_table("FalsePositve", GNG2, 1)
  
  
  ggplot(gng_fp1, aes(x=Period, y=FalsePositive, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=FalsePositive-se, ymax=FalsePositive+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(Shift ~Condition*TimeBin) + #facet_grid(.~Condition*Period) +
    #coord_cartesian(ylim=c(20,21))+
    geom_text(aes(label = round(FalsePositive, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  ggplot(gng_fp2, aes(x=Period, y=FalsePositive, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=FalsePositive-se, ymax=FalsePositive+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(.~Shift* Condition) +
    #coord_cartesian(ylim=c(20,21))+
    geom_text(aes(label = round(FalsePositive, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  
  
  ##GNG mean of mean responsetime
  
  
  gng_rt_subs1 <- summarySE(GNG2, measurevar="avgResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  gng_rt_subs2 <- summarySE(GNG2, measurevar="avgResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "Shift", "Period", "Condition"))
  
  
  gng_rt1 <- summarySE(gng_rt_subs1, measurevar="avgResponseTimeCorrect", groupvars=c("Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  gng_rt2 <- summarySE(gng_rt_subs2, measurevar="avgResponseTimeCorrect", groupvars=c("Condition_period", "Shift", "Condition", "Period"))
  
  
  ggplot(gng_rt1, aes(x=Period, y=avgResponseTimeCorrect, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=avgResponseTimeCorrect-se, ymax=avgResponseTimeCorrect+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(Shift ~Condition*TimeBin) + #facet_grid(.~Condition*Period) +
    coord_cartesian(ylim=c(.4,.7))+
    geom_text(aes(label = round(avgResponseTimeCorrect, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  ggplot(gng_rt2, aes(x=Period, y=avgResponseTimeCorrect, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=avgResponseTimeCorrect-se, ymax=avgResponseTimeCorrect+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(.~Shift* Condition) +
    coord_cartesian(ylim=c(.4,.7))+
    geom_text(aes(label = round(avgResponseTimeCorrect, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
 
  
  if(FALSE){
    ##GNG median of mean responsetime
    
    
    gng_rt_subs1 <- summarySE2(GNG2, measurevar="avgResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "TimeBin", "Shift", "Period", "Condition"))
    gng_rt_subs2 <- summarySE2(GNG2, measurevar="avgResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "Shift", "Period", "Condition"))
    
    
    gng_rt1 <- summarySE2(gng_rt_subs1, measurevar="avgResponseTimeCorrect median", groupvars=c("Condition_period", "TimeBin", "Shift", "Period", "Condition"))
    gng_rt2 <- summarySE2(gng_rt_subs2, measurevar="avgResponseTimeCorrect median", groupvars=c("Condition_period", "Shift", "Condition", "Period"))
    
    
    ggplot(gng_rt1, aes(x=Period, y=`avgResponseTimeCorrect median mean`, fill = Condition_period)) + 
      geom_bar(position=position_dodge(), stat="identity") +
      geom_errorbar(aes(ymin=`avgResponseTimeCorrect median mean`-se, ymax=`avgResponseTimeCorrect median mean`+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) + facet_grid(Shift ~Condition*TimeBin) + #facet_grid(.~Condition*Period) +
      coord_cartesian(ylim=c(.4,.7))+
      geom_text(aes(label = round(`avgResponseTimeCorrect median mean`, digits = 3), vjust=2))+
      scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    
    
    ggplot(gng_rt2, aes(x=Period, y=`avgResponseTimeCorrect median mean`, fill = Condition_period)) + 
      geom_bar(position=position_dodge(), stat="identity") +
      geom_errorbar(aes(ymin=`avgResponseTimeCorrect median mean`-se, ymax=`avgResponseTimeCorrect median mean`+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) + facet_grid(.~Shift* Condition) +
      coord_cartesian(ylim=c(.4,.7))+
      geom_text(aes(label = round(`avgResponseTimeCorrect median mean`, digits = 3), vjust=2))+
      scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    
    
    ##GNG mean median responsetime
    
    
    gng_rt_subs1 <- summarySE(GNG2, measurevar="medResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "TimeBin", "Shift", "Period", "Condition"))
    gng_rt_subs2 <- summarySE(GNG2, measurevar="medResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "Shift", "Period", "Condition"))
    
    
    gng_rt1 <- summarySE(gng_rt_subs1, measurevar="medResponseTimeCorrect", groupvars=c("Condition_period", "TimeBin", "Shift", "Period", "Condition"))
    gng_rt2 <- summarySE(gng_rt_subs2, measurevar="medResponseTimeCorrect", groupvars=c("Condition_period", "Shift", "Condition", "Period"))
    
    
    ggplot(gng_rt1, aes(x=Period, y=medResponseTimeCorrect, fill = Condition_period)) + 
      geom_bar(position=position_dodge(), stat="identity") +
      geom_errorbar(aes(ymin=medResponseTimeCorrect-se, ymax=medResponseTimeCorrect+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) + facet_grid(Shift ~Condition*TimeBin) + #facet_grid(.~Condition*Period) +
      coord_cartesian(ylim=c(.4,.7))+
      geom_text(aes(label = round(medResponseTimeCorrect, digits = 3), vjust=2))+
      scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    
    
    ggplot(gng_rt2, aes(x=Period, y=medResponseTimeCorrect, fill = Condition_period)) + 
      geom_bar(position=position_dodge(), stat="identity") +
      geom_errorbar(aes(ymin=medResponseTimeCorrect-se, ymax=medResponseTimeCorrect+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) + facet_grid(.~Shift* Condition) +
      coord_cartesian(ylim=c(.4,.7))+
      geom_text(aes(label = round(medResponseTimeCorrect, digits = 3), vjust=2))+
      scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    
  }

  
  ##GNG median of median responsetime
  
  
  gng_rt_subs1 <- summarySE2(GNG2, measurevar="medResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  gng_rt_subs2 <- summarySE2(GNG2, measurevar="medResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "Shift", "Period", "Condition"))
  
  
  gng_rt1 <- summarySE2(gng_rt_subs1, measurevar="medResponseTimeCorrect median", groupvars=c("Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  gng_rt2 <- summarySE2(gng_rt_subs2, measurevar="medResponseTimeCorrect median", groupvars=c("Condition_period", "Shift", "Condition", "Period"))
  
  
  ggplot(gng_rt1, aes(x=Period, y=`medResponseTimeCorrect median median`, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=`medResponseTimeCorrect median median`-se, ymax=`medResponseTimeCorrect median median`+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(Shift ~Condition*TimeBin) + #facet_grid(.~Condition*Period) +
    coord_cartesian(ylim=c(.4,.7))+
    geom_text(aes(label = round(`medResponseTimeCorrect median median`, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  ggplot(gng_rt2, aes(x=Period, y=`medResponseTimeCorrect median median`, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=`medResponseTimeCorrect median median`-se, ymax=`medResponseTimeCorrect median median`+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(.~Shift* Condition) +
    coord_cartesian(ylim=c(.4,.7))+
    geom_text(aes(label = round(`medResponseTimeCorrect median median`, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  
  
  
  
  
   ###Significance
  
  gng_rt_subs3 <- summarySE(GNG2, measurevar="avgResponseTimeCorrect", groupvars=c("SubjectID", "TimeBin"))

  
  gng_rt3 <- summarySE(gng_rt_subs3, measurevar="avgResponseTimeCorrect", groupvars=c("TimeBin"))
  
  ggplot(gng_rt3, aes(x=TimeBin, y=avgResponseTimeCorrect)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=avgResponseTimeCorrect-se, ymax=avgResponseTimeCorrect+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + 
    coord_cartesian(ylim=c(.4,.7))+
    geom_text(aes(label = round(avgResponseTimeCorrect, digits = 3), vjust=2))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
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
  
  OB$Condition <- ifelse(OB$Condition == "r", "Red", 
                          ifelse(OB$Condition == "b", "Blue", "White" ))
  OB$Period <- ifelse(OB$Period == "b","Baseline", "Intervention")
  OB$Condition_period <- ifelse(OB$Condition_period == "b_b", "Blue baseline", 
                                 ifelse(OB$Condition_period == "b_i", "Blue intervention", 
                                        ifelse(OB$Condition_period == "r_b", "Red baseline", 
                                               ifelse(OB$Condition_period == "r_i", "Red intervention", 
                                                      ifelse(OB$Condition_period == "w_b", "White baseline", "White intervention" )))))
  OB$Condition_period <- factor(OB$Condition_period, levels = c("Red baseline", "Red intervention", "Blue baseline", "Blue intervention", "White baseline", "White intervention"   ))
  
  
  
  #OB2 <- subset(OB, ValidPairedBin == "TRUE" & isValid == "TRUE" & !is.na(TimeBin))
  #OB2 <- subset(OB,isValid == "TRUE" & !is.na(TimeBin))
  OB2 <- subset(OB,!is.na(TimeBin) & !is.na(avgResponseTimeCorrect) & !is.na(avgResponseTime) & HasMaxTrials == "TRUE" )
  
  OB_half <-subset(OB2, HalfAnswered == "TRUE" )
  OB_normal <-subset(OB2, HalfAnswered == "FALSE" )
  
  ##OB
  
  OB_percentCorrect_subs1 <- summarySE(OB_normal, measurevar="PercentCorrect", groupvars=c("SubjectID", "Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  OB_percentCorrect_subs2 <- summarySE(OB_normal, measurevar="PercentCorrect", groupvars=c("SubjectID", "Condition_period", "Shift", "Period", "Condition"))
  
  
  OB_percentCorrect1 <- summarySE(OB_percentCorrect_subs1, measurevar="PercentCorrect", groupvars=c("Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  OB_percentCorrect2 <- summarySE(OB_percentCorrect_subs2, measurevar="PercentCorrect", groupvars=c("Condition_period", "Shift", "Condition", "Period"))
  
  
  ggplot(OB_percentCorrect1, aes(x=Period, y=PercentCorrect, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=PercentCorrect-se, ymax=PercentCorrect+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(Shift ~Condition*TimeBin) + #facet_grid(.~Condition*Period) +
    coord_cartesian(ylim=c(.8,1))+
    geom_text(aes(label = round(PercentCorrect, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  ggplot(OB_percentCorrect2, aes(x=Period, y=PercentCorrect, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=PercentCorrect-se, ymax=PercentCorrect+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(.~Shift* Condition) +
    coord_cartesian(ylim=c(.8,1))+
    geom_text(aes(label = round(PercentCorrect, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  ##OB means of means rt
  
  OB_avgResponseTimeCorrect_subs1 <- summarySE(OB_normal, measurevar="avgResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  OB_avgResponseTimeCorrect_subs2 <- summarySE(OB_normal, measurevar="avgResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "Shift", "Period", "Condition"))
  OB_avgResponseTimeCorrect_subs3 <- summarySE(OB_normal, measurevar="avgResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "Period", "Condition"))
  
  OB_avgResponseTimeCorrect1 <- summarySE(OB_avgResponseTimeCorrect_subs1, measurevar="avgResponseTimeCorrect", groupvars=c("Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  OB_avgResponseTimeCorrect2 <- summarySE(OB_avgResponseTimeCorrect_subs2, measurevar="avgResponseTimeCorrect", groupvars=c("Condition_period", "Shift", "Condition", "Period"))
  OB_avgResponseTimeCorrect3 <- summarySE(OB_avgResponseTimeCorrect_subs3, measurevar="avgResponseTimeCorrect", groupvars=c("Condition_period", "Condition", "Period"))
  
 # OB_avgResponseTimeCorrect3 <- summarySE(real_OB_data, measurevar="avgResponseTimeCorrect", groupvars=c("Condition_period", "Condition", "Period"))
  
  ggplot(OB_avgResponseTimeCorrect1, aes(x=Period, y=avgResponseTimeCorrect, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=avgResponseTimeCorrect-se, ymax=avgResponseTimeCorrect+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(Shift ~Condition*TimeBin) + #facet_grid(.~Condition*Period) +
    coord_cartesian(ylim=c(.5,1))+
    geom_text(aes(label = round(avgResponseTimeCorrect, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


  
  
  ggplot(OB_avgResponseTimeCorrect2, aes(x=Period, y=avgResponseTimeCorrect, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=avgResponseTimeCorrect-se, ymax=avgResponseTimeCorrect+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(.~Shift* Condition) +
    coord_cartesian(ylim=c(.5,1))+
    geom_text(aes(label = round(avgResponseTimeCorrect, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  ggplot(OB_avgResponseTimeCorrect3, aes(x=Condition_period, y=avgResponseTimeCorrect, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=avgResponseTimeCorrect-se, ymax=avgResponseTimeCorrect+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + # facet_grid(.~ Condition) +
    coord_cartesian(ylim=c(.5,1))+
    geom_text(aes(label = round(avgResponseTimeCorrect, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    geom_signif(comparisons = list(c("Red baseline", "Red intervention")), annotations=".003")

  ##OB medians of medians
  
  
  OB_medResponseTimeCorrect_subs1 <- summarySE2(OB_normal, measurevar="medResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  OB_medResponseTimeCorrect_subs2 <- summarySE2(OB_normal, measurevar="medResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "Shift", "Period", "Condition"))
  OB_medResponseTimeCorrect_subs3 <- summarySE2(OB_normal, measurevar="medResponseTimeCorrect", groupvars=c("SubjectID", "Condition_period", "Period", "Condition"))
  
  OB_medResponseTimeCorrect1 <- summarySE2(OB_medResponseTimeCorrect_subs1, measurevar="medResponseTimeCorrect median", groupvars=c("Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  OB_medResponseTimeCorrect2 <- summarySE2(OB_medResponseTimeCorrect_subs2, measurevar="medResponseTimeCorrect median", groupvars=c("Condition_period", "Shift", "Condition", "Period"))
  OB_medResponseTimeCorrect3 <- summarySE2(OB_medResponseTimeCorrect_subs3, measurevar="medResponseTimeCorrect median", groupvars=c("Condition_period", "Condition", "Period"))
  
  
  ggplot(OB_medResponseTimeCorrect1, aes(x=Period, y=`medResponseTimeCorrect median median`, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=`medResponseTimeCorrect median median`-se, ymax=`medResponseTimeCorrect median median`+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(Shift ~Condition*TimeBin) + #facet_grid(.~Condition*Period) +
    coord_cartesian(ylim=c(.5,1))+
    geom_text(aes(label = round(`medResponseTimeCorrect median median`, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  
  ggplot(OB_medResponseTimeCorrect2, aes(x=Period, y=`medResponseTimeCorrect median median`, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=`medResponseTimeCorrect median median`-se, ymax=`medResponseTimeCorrect median median`+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(.~Shift* Condition) +
    coord_cartesian(ylim=c(.5,1))+
    geom_text(aes(label = round(`medResponseTimeCorrect median median`, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  
  sumDataOBrt <- aggregate(medResponseTimeCorrect ~ SubjectID + Condition_period + Condition, data =   OB_normal[!is.na(OB_normal$medResponseTimeCorrect),], FUN = median)
  
  redOBrt <- sumDataOBrt[sumDataOBrt$Condition == "Red",]
  redOBrt2 <-  redOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
  
  blueOBrt <- sumDataOBrt[sumDataOBrt$Condition == "Blue",]
  blueOBrt2 <-  blueOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
  
  whiteOBrt <- sumDataOBrt[sumDataOBrt$Condition == "White",]
  whiteOBrt2 <-  whiteOBrt %>% group_by(SubjectID) %>% filter(n()>1) 
  
  wilcox.test(medResponseTimeCorrect ~ Condition_period, data = redOBrt2, paired = TRUE) 
  wilcox.test(medResponseTimeCorrect ~ Condition_period, data = blueOBrt2, paired = TRUE) 
  wilcox.test(medResponseTimeCorrect ~ Condition_period, data = whiteOBrt2, paired = TRUE) 
  
  
  
  ggplot(OB_medResponseTimeCorrect3, aes(x=Condition_period, y=`medResponseTimeCorrect median median`, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=`medResponseTimeCorrect median median`-se, ymax=`medResponseTimeCorrect median median`+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + # facet_grid(.~ Condition) +
    coord_cartesian(ylim=c(.5,1))+
    geom_text(aes(label = round(`medResponseTimeCorrect median median`, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    geom_signif(comparisons = list(c("White baseline", "White intervention")), annotations=".047")+
    geom_signif(comparisons = list(c("Red baseline", "Red intervention")), annotations=".038")

  
  
  
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
  
  PVT$Condition <- ifelse(PVT$Condition == "r", "Red", 
                         ifelse(PVT$Condition == "b", "Blue", "White" ))
  PVT$Period <- ifelse(PVT$Period == "b","Baseline", "Intervention")
  PVT$Condition_period <- ifelse(PVT$Condition_period == "b_b", "Blue baseline", 
                                ifelse(PVT$Condition_period == "b_i", "Blue intervention", 
                                       ifelse(PVT$Condition_period == "r_b", "Red baseline", 
                                              ifelse(PVT$Condition_period == "r_i", "Red intervention", 
                                                     ifelse(PVT$Condition_period == "w_b", "White baseline", "White intervention" )))))
  PVT$Condition_period <- factor(PVT$Condition_period, levels = c("Red baseline", "Red intervention", "Blue baseline", "Blue intervention", "White baseline", "White intervention"   ))
  
  
  
  
  #PVT2 <- subset(PVT, ValidPairedBin == "TRUE" & ValidTest == "TRUE" & !is.na(TimeBin))
  #PVT2 <- subset(PVT,ValidTest == "TRUE" & !is.na(TimeBin))
  PVT2 <- subset(PVT, !is.na(TimeBin) & !is.na(avgResponseTime) & HasMaxTrials == "TRUE")
  
  PVT_percentCorrect_subs1 <- summarySE(PVT2, measurevar="accuracy", groupvars=c("SubjectID", "Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  PVT_percentCorrect_subs2 <- summarySE(PVT2, measurevar="accuracy", groupvars=c("SubjectID", "Condition_period", "Shift", "Period", "Condition"))
  PVT_percentCorrect_subs3 <- summarySE(PVT2, measurevar="accuracy", groupvars=c("SubjectID", "Condition_period",  "Period", "Condition"))
  
  PVT_percentCorrect1 <- summarySE(PVT_percentCorrect_subs1, measurevar="accuracy", groupvars=c("Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  PVT_percentCorrect2 <- summarySE(PVT_percentCorrect_subs2, measurevar="accuracy", groupvars=c("Condition_period", "Shift", "Condition", "Period"))
  PVT_percentCorrect3 <- summarySE(PVT_percentCorrect_subs2, measurevar="accuracy", groupvars=c("Condition_period", "Condition", "Period"))
  
  
  ggplot(PVT_percentCorrect1, aes(x=Period, y=accuracy, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=accuracy-se, ymax=accuracy+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(Shift ~Condition*TimeBin) + #facet_grid(.~Condition*Period) +
    coord_cartesian(ylim=c(.8,1))+
    geom_text(aes(label = round(accuracy, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  ggplot(PVT_percentCorrect2, aes(x=Period, y=accuracy, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=accuracy-se, ymax=accuracy+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(.~Shift* Condition) +
    coord_cartesian(ylim=c(.8,1))+
    geom_text(aes(label = round(accuracy, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  ggplot(PVT_percentCorrect3, aes(x=Condition_period, y=accuracy, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=accuracy-se, ymax=accuracy+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    coord_cartesian(ylim=c(.8,1))+
    geom_text(aes(label = round(accuracy, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    geom_signif(comparisons = list(c("Red baseline", "Red intervention")), annotations=".0134", y_position = .985)
    
  ###Median of median
  
  PVT_percentCorrect_subs1 <- summarySE2(PVT2, measurevar="accuracy", groupvars=c("SubjectID", "Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  PVT_percentCorrect_subs2 <- summarySE2(PVT2, measurevar="accuracy", groupvars=c("SubjectID", "Condition_period", "Shift", "Period", "Condition"))
  PVT_percentCorrect_subs3 <- summarySE2(PVT2, measurevar="accuracy", groupvars=c("SubjectID", "Condition_period",  "Period", "Condition"))
  
  PVT_percentCorrect1 <- summarySE2(PVT_percentCorrect_subs1, measurevar="accuracy median", groupvars=c("Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  PVT_percentCorrect2 <- summarySE2(PVT_percentCorrect_subs2, measurevar="accuracy median", groupvars=c("Condition_period", "Shift", "Condition", "Period"))
  PVT_percentCorrect3 <- summarySE2(PVT_percentCorrect_subs3, measurevar="accuracy median", groupvars=c("Condition_period", "Condition", "Period"))
  
  
  ggplot(PVT_percentCorrect1, aes(x=Period, y=`accuracy median median`, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=`accuracy median median`-se, ymax=`accuracy median median`+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(Shift ~Condition*TimeBin) + #facet_grid(.~Condition*Period) +
    coord_cartesian(ylim=c(.8,1))+
    geom_text(aes(label = round(`accuracy median median`, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  ggplot(PVT_percentCorrect2, aes(x=Period, y=`accuracy median median`, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=`accuracy median median`-se, ymax=`accuracy median median`+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(.~Shift* Condition) +
    coord_cartesian(ylim=c(.8,1))+
    geom_text(aes(label = round(`accuracy median median`, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  ggplot(PVT_percentCorrect3, aes(x=Condition_period, y=`accuracy median median`, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=`accuracy median median`-se, ymax=`accuracy median median`+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    coord_cartesian(ylim=c(.8,1))+
    geom_text(aes(label = round(`accuracy median median`, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    geom_signif(comparisons = list(c("Red baseline", "Red intervention")), annotations=".004", y_position = .985)
  
    sumData <- aggregate(accuracy ~ SubjectID + Condition_period + Condition, data =  PVT2, FUN = median)
  
    red <- sumData[sumData$Condition == "Red",]
    red2 <-  red %>% group_by(SubjectID) %>% filter(n()>1) 
  
    blue <- sumData[sumData$Condition == "Blue",]
    blue2 <-  blue %>% group_by(SubjectID) %>% filter(n()>1) 
  
    white <- sumData[sumData$Condition == "White",]
    white2 <-  white %>% group_by(SubjectID) %>% filter(n()>1) 
  

    wilcox.test(accuracy ~ Condition_period, data = red2, paired = TRUE) 
    wilcox.test(accuracy ~ Condition_period, data = blue2, paired = TRUE) 
    wilcox.test(accuracy ~ Condition_period, data = white2, paired = TRUE) 
  
  
  
  ##Mean of mean
  
  PVT_avgResponseTime_subs1 <- summarySE(PVT2, measurevar="avgResponseTime", groupvars=c("SubjectID", "Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  PVT_avgResponseTime_subs2 <- summarySE(PVT2, measurevar="avgResponseTime", groupvars=c("SubjectID", "Condition_period", "Shift", "Period", "Condition"))
  
  PVT_avgResponseTime1 <- summarySE(PVT_avgResponseTime_subs1, measurevar="avgResponseTime", groupvars=c("Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  PVT_avgResponseTime2 <- summarySE(PVT_avgResponseTime_subs2, measurevar="avgResponseTime", groupvars=c("Condition_period", "Shift", "Condition", "Period"))
  
  
  ggplot(PVT_avgResponseTime1, aes(x=Period, y=avgResponseTime, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=avgResponseTime-se, ymax=avgResponseTime+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(Shift ~Condition*TimeBin) + #facet_grid(.~Condition*Period) +
    coord_cartesian(ylim=c(.2,.6))+
    geom_text(aes(label = round(avgResponseTime, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  ggplot(PVT_avgResponseTime2, aes(x=Period, y=avgResponseTime, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=avgResponseTime-se, ymax=avgResponseTime+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(.~Shift* Condition) +
    coord_cartesian(ylim=c(.2,.6))+
    geom_text(aes(label = round(avgResponseTime, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  

  
  #Median of median
  PVT_medResponseTime_subs1 <- summarySE2(PVT2, measurevar="medResponseTime", groupvars=c("SubjectID", "Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  PVT_medResponseTime_subs2 <- summarySE2(PVT2, measurevar="medResponseTime", groupvars=c("SubjectID", "Condition_period", "Shift", "Period", "Condition"))
  
  PVT_medResponseTime1 <- summarySE2(PVT_medResponseTime_subs1, measurevar="medResponseTime median", groupvars=c("Condition_period", "TimeBin", "Shift", "Period", "Condition"))
  PVT_medResponseTime2 <- summarySE2(PVT_medResponseTime_subs2, measurevar="medResponseTime median", groupvars=c("Condition_period", "Shift", "Condition", "Period"))
  
  
  ggplot(PVT_medResponseTime1, aes(x=Period, y=`medResponseTime median median`, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=`medResponseTime median median`-se, ymax=`medResponseTime median median`+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(Shift ~Condition*TimeBin) + #facet_grid(.~Condition*Period) +
    coord_cartesian(ylim=c(.2,.6))+
    geom_text(aes(label = round(`medResponseTime median median`, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
  
  ggplot(PVT_medResponseTime2, aes(x=Period, y=`medResponseTime median median`, fill = Condition_period)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=`medResponseTime median median`-se, ymax=`medResponseTime median median`+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) + facet_grid(.~Shift* Condition) +
    coord_cartesian(ylim=c(.2,.6))+
    geom_text(aes(label = round(`medResponseTime median median`, digits = 3), vjust=2))+
    scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
  