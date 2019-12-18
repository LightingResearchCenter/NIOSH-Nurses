library(nlme)


PVT <- read_excel("//root/public/roohac/NIOSH-nurses-study/completed-nurses/full_data_sets/processedtime2/Nurses_Study_Summary_Tables_2018_08_04_16_59_15_complete.xls", 
                  sheet = "PVT")


colnames(PVT)[colnames(PVT)=="Condition"] <- "Color"
colnames(PVT)[colnames(PVT)=="Period"] <- "Condition"


PVT$Color_Condition <- paste(PVT$Color, PVT$Condition, sep = "_")


PVT$SubjectID <- as.factor(PVT$SubjectID )
PVT$Shift <- as.factor(PVT$Shift )
PVT$Color <- as.factor(PVT$Color )
PVT$Condition <- as.factor(PVT$Condition )
PVT$Color_Condition <- as.factor(PVT$Color_Condition )

PVT$TimeBin <- as.numeric(PVT$TimeBin)

PVT$avgResponseTime <- as.numeric(PVT$avgResponseTime )
PVT$accuracy <- PVT$nCorrect/PVT$nTrials



#PVT2 <- subset(PVT, ValidPairedBin == "TRUE" & ValidTest == "TRUE" & !is.na(TimeBin))
#PVT2 <- subset(PVT,ValidTest == "TRUE" & !is.na(TimeBin))
PVT2 <- subset(PVT,  !is.na(TimeBin) & !is.na(avgResponseTime) & HasMaxTrials == "TRUE" & Color_Condition != "NA_NA" & ValidPairedBin == "TRUE")

###Plot
ggplot(PVT2, aes(x = TimeBin, y = avgResponseTime, group = Condition, linetype = Condition, fill = Color))+
  #geom_point() + 
  geom_smooth()+
  facet_grid(Color~Shift)+
  scale_fill_manual(values=c( "deepskyblue4", "red4",   "gray80")) 
  


PVT_rt_model0 <- lme(avgResponseTime ~ Shift * Color * Condition  *  TimeBin, random = ~1|SubjectID,
                    data=PVT2)

PVT_rt_model1 <- lme(avgResponseTime ~ Shift * Color * Condition  *  TimeBin, random = ~ 1+ TimeBin|SubjectID, 
                    data=PVT2)

anova(PVT_rt_model0, PVT_rt_model1)

PVT_rt_model2 <- lme(avgResponseTime ~ Shift * Color * Condition  *  TimeBin, random = ~TimeBin|SubjectID/Color/Condition, 
                     data=PVT2)

anova(PVT_rt_model1, PVT_rt_model2)

