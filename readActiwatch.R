


library(readxl)


readActiwatch <- function(){
  library(readxl)
  
  dir <- "//root/projects/NIOSH_RedLightForShiftWorkers/Actiware data/Actiwatch Data"
  fileList <- list.files(dir)[grep(".xlsx", list.files(dir))]
  allsubAD <- data.frame()
  for(i in 1:length(fileList)){
    
    #add for loop to iterate through each subject
    print(fileList[i])
    sheets <- excel_sheets(path = paste(dir, fileList[i], sep = "/"))
    validSheets <- sheets[grep("calc", sheets)]
    print(validSheets)
    ## iterat through each sheet
  }}
    
    subject_100_nurses_study <- read_excel("//root/projects/NIOSH_RedLightForShiftWorkers/Actiware data/Actiwatch Data/subject 100 nurses study.xlsx", 
                                           sheet = "100 red calc")
    
    subject_100_nurses_study <- subset(subject_100_nurses_study, `Interval Type` == "avg" )
    
    summaryData <- subject_100_nurses_study
    
    summaryData$period <- "NA"
    summaryData$period[1] <- "baseline"
    summaryData$period[2] <- "intervention"
    summaryData$light <- "red"
    
    summaryData$subject <- "100"
    summaryData$`Interval Type` <- NULL
    summaryData$`Interval#` <- NULL
    summaryData$`Start Date` <- NULL
    summaryData$`Start Time` <- NULL
    summaryData$`End Date` <- NULL
    summaryData$`End Time` <- NULL
    
    summaryData <- summaryData[ , c(11, 10, 9, 1:8) ]
    
  }
}




