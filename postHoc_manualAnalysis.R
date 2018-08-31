



#Hit rate condition
GNG$Condition_period <- paste(GNG$Condition, GNG$Period, sep = "_")

GNG2 <- subset(GNG,!is.na(TimeBin) & !is.na(avgResponseTimeCorrect) & !is.na(avgResponseTime) & HasMaxTrials == "TRUE" & Condition_period != "NA_NA")

hitRateData <- aggregate(HitRate ~ Condition + SubjectID, data = GNG2, FUN = mean)

table(hitRateData$Condition)
table(hitRateData$SubjectID)


cropData <- function(testData, compare1, compare2, DV){
  
  croppedData1 <- subset(testData, Condition == compare1 | Condition == compare2)
  subNum <- data.frame(table(croppedData1$SubjectID))
  completeSumNUm <- subNum[subNum$Freq ==2,]
  
  subjectList <- unique(completeSumNUm$Var1)
  
  croppedData2 <- croppedData1[croppedData1$SubjectID %in% subjectList,]
  return(croppedData2)
}

tTest_r_w <- cropData(hitRateData, "r", "w", "Condition")

t.test(HitRate ~ Condition, data = tTest_r_w, paired = TRUE)


tTest_r_b <- cropData(hitRateData, "r", "b", "Condition")

t.test(HitRate ~ Condition, data = tTest_r_b, paired = TRUE)

tTest_w_b <- cropData(hitRateData, "w", "b", "Condition")

t.test(HitRate ~ Condition, data = tTest_w_b, paired = TRUE)







#####
OB$Condition_period <- paste(OB$Condition, OB$Period, sep = "_")


OB$SubjectID <- as.factor(OB$SubjectID )
OB$Shift <- as.factor(OB$Shift )
OB$Condition <- as.factor(OB$Condition )
OB$Period <- as.factor(OB$Period )
OB$Condition_period <- as.factor(OB$Condition_period )

OB2 <- subset(OB,!is.na(TimeBin) & !is.na(avgResponseTimeCorrect) & !is.na(avgResponseTime) & HasMaxTrials == "TRUE" & Condition_period != "NA_NA" )

OB_half <-subset(OB2, HalfAnswered == "TRUE" )
OB_normal <-subset(OB2, HalfAnswered == "FALSE" )

OB_rt_model1 <- lme(avgResponseTimeCorrect ~ Condition * Period *  TimeBin * Shift,random = ~1|SubjectID/Condition/Period,
                   data=OB_normal[!is.na(OB_normal$avgResponseTimeCorrect) & OB_normal$SubjectID != 155 & OB_normal$SubjectID != 160 & OB_normal$SubjectID != 101,])
anova(OB_rt_model1)

comparisonRed <- aggregate(avgResponseTimeCorrect~ Period + SubjectID, data =subset(OB_normal[!is.na(OB_normal$avgResponseTimeCorrect),], Condition == "r"), FUN = mean)
comparisonBlue <- aggregate(avgResponseTimeCorrect~ Period + SubjectID, data =subset(OB_normal[!is.na(OB_normal$avgResponseTimeCorrect),], Condition == "b"), FUN = mean)

t.test(avgResponseTimeCorrect~ Period, data = comparisonRed[comparisonRed$SubjectID != 155 & comparisonRed$SubjectID != 160 & 
                                                              comparisonRed$SubjectID != 101 & comparisonRed$SubjectID != 100 &  
                                                              comparisonRed$SubjectID != 123 & comparisonRed$SubjectID != 134 & comparisonRed$SubjectID != 157,], paired = TRUE)


OB_rt_model2 <- lme(avgResponseTimeCorrect ~ Shift * Condition_period *  TimeBin,random = ~1|SubjectID/Condition/Period/TimeBin,
                   data=OB_normal[!is.na(OB_normal$avgResponseTimeCorrect),])
anova(OB_rt_model2)