NormalizeAcitwatch <- function(acti_watch){
  acti_watch$period_start <- NULL
  acti_watch$period_end <- NULL
  
  
  library(reshape2)
  colnames(acti_watch)
  acti_watch2 <- melt(acti_watch, id = c( "subject", "light" ,"period" ,"Shift"  ))
  
  library(tidyr)
  acti_watch3 <- spread(acti_watch2, key = period, value = value)
  acti_watch3$Norm <- acti_watch3$intervention/acti_watch3$baseline
  acti_watch3$baseline <- NULL
  acti_watch3$intervention <- NULL
  acti_watch4 <- subset(acti_watch3, is.finite(Norm) & !is.na(is.finite(Norm)))
  
  acti_watch5 <- spread(acti_watch4, key = variable, value = Norm)
  
  return(acti_watch5)
}