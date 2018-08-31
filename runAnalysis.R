library(readxl)
source('~/GitHub/NIOSH-Nurses/performanceAnalysisFunctions.R', echo=TRUE)
source('~/GitHub/NIOSH-Nurses/IVISAnalysisFunctions.R', echo=TRUE)
source('~/GitHub/NIOSH-Nurses/actiwatchAnalysisFunctions.R', echo=TRUE)

PVT <- read_excel("//root/public/roohac/NIOSH-nurses-study/completed-nurses/full_data_sets/processedtime2/Nurses_Study_Summary_Tables_2018_08_04_16_59_15_complete.xls", 
                  sheet = "PVT")

GNG <-  read_excel("//root/public/roohac/NIOSH-nurses-study/completed-nurses/full_data_sets/processedtime2/Nurses_Study_Summary_Tables_2018_08_04_16_59_15_complete.xls", 
                                                                     sheet = "GNG")

OB <- read_excel("//root/public/roohac/NIOSH-nurses-study/completed-nurses/full_data_sets/processedtime2/Nurses_Study_Summary_Tables_2018_08_04_16_59_15_complete.xls", 
                  sheet = "OB")
#acti-watchSummary_2-12-18.
acti_watch <- read_excel("//root/projects/NIOSH_RedLightForShiftWorkers/Actiware data/Sleep-analysis-data_8-17-18.xlsx")

source('~/GitHub/NIOSH-Nurses/normalizeActi-watch.R', echo=TRUE)
acti_watch_norm <- NormalizeAcitwatch(acti_watch)

ISIV <- read_excel("//root/projects/NIOSH_RedLightForShiftWorkers/Actiware data/NIOSH_Nurses_Actiwatch_BaselineIntervention_ISIV.xlsx")
ISIV <- read_excel("//root/projects/NIOSH_RedLightForShiftWorkers/Actiware data/NIOSH_Nurses_Actiwatch_BaselineIntervention_ISIV.xlsx")



performance_outputList <- Output_lme_performance_nurses_study(PVT, OB, GNG, FALSE, TRUE)


actiwatch_outputList <- Output_lme_actiWatch_nurses_study(acti_watch, TRUE, TRUE)

actiwatchNorm_outputList <- Output_lme_actiWatchNorm_nurses_study(acti_watch_norm, FALSE, TRUE)

IVIS_outputList <- Output_lme_ISIV_nurses_study(ISIV, TRUE)


performance_outputList

