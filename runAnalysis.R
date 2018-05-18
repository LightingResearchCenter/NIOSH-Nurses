library(readxl)
source('~/GitHub/NIOSH-Nurses/performanceAnalysisFunctions.R', echo=TRUE)
source('~/GitHub/NIOSH-Nurses/IVISAnalysisFunctions.R', echo=TRUE)
source('~/GitHub/NIOSH-Nurses/actiwatchAnalysisFunctions.R', echo=TRUE)

PVT <- read_excel("//root/public/roohac/nurses_study/completed-nurses/full_data_sets/processedtime2/Nurses_Study_Summary_Tables_2018_02_21_15_23_37_Complete.xlsx", 
                  sheet = "PVT")

GNG <- read_excel("//root/public/roohac/nurses_study/completed-nurses/full_data_sets/processedtime2/Nurses_Study_Summary_Tables_2018_02_21_15_23_37_Complete.xlsx", 
                                                              sheet = "GNG")

OB <- read_excel("//root/public/roohac/nurses_study/completed-nurses/full_data_sets/processedtime2/Nurses_Study_Summary_Tables_2018_02_21_15_23_37_Complete.xlsx", 
                  sheet = "OB")

acti_watch <- read_excel("//root/projects/NIOSH_RedLightForShiftWorkers/Actiware data/acti-watchSummary_2-12-18.xlsx")

ISIV <- read_excel("//root/projects/NIOSH_RedLightForShiftWorkers/Actiware data/NIOSH_Nurses_Actiwatch_BaselineIntervention_ISIV.xlsx")



performance_outputList <- Output_lme_performance_nurses_study(PVT, OB, GNG, TRUE)


actiwatch_outputList <- Output_lme_actiWatch_nurses_study(acti_watch, TRUE)


IVIS_outputList <- Output_lme_ISIV_nurses_study(ISIV, TRUE)


performance_outputList

