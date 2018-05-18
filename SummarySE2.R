summarySE2 <- function (data = NULL, measurevar, groupvars = NULL, na.rm = FALSE, 
                       conf.interval = 0.95, .drop = TRUE) 
{
  library("plyr")
  length2 <- function(x, na.rm = FALSE) {
    if (na.rm) 
      sum(!is.na(x))
    else length(x)
  }
  datac <- ddply(data, groupvars, .drop = .drop, .fun = function(xx, 
                                                                 col, na.rm) {
    c(N = length2(xx[, col], na.rm = na.rm),
      mean = mean(xx[, col], na.rm = na.rm), 
      median = median(xx[, col], na.rm = na.rm), 
      sd = sd(xx[, col], na.rm = na.rm))
  }, measurevar, na.rm)
  datac <- plyr::rename(datac, c(mean = paste(measurevar, "mean", sep = " ")))
  datac <- plyr::rename(datac, c(median = paste(measurevar, "median", sep = " ")))
                                              
  datac$se <- datac$sd/sqrt(datac$N)
  ciMult <- qt(conf.interval/2 + 0.5, datac$N - 1)
  datac$ci <- datac$se * ciMult
  return(datac)
}
