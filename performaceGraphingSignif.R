
  library(nlme)
  library(lsmeans)
  library(ggplot2)
  library(Rmisc)
  library(MuMIn)

  library(ggsignif)
  load("//root/projects/NIOSH_RedLightForShiftWorkers/performance/performanceModels.RData")
  
  ###IVIS
  IVIS_outputList[[2]][[2]][[2]]
  
  df <- IVIS_outputList[[2]][[2]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Shift", "IS", FALSE, .2, .7)
  
  
  
  
  ####"GNG rt - timebin"
  performance_outputList[[3]][[1]][[1]]
  performance_outputList[[3]][[2]]
  
  name <- performance_outputList[[3]][[2]][[1]][[1]]
  df <- performance_outputList[[3]][[2]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "TimeBin", "GNG mean response time", FALSE, .35, .7)
  
  
  
  
  
  
  
  
  ####"OB accuracy - timebin"
  performance_outputList[[4]][[1]][[1]]
  performance_outputList[[4]][[2]]
  
  name <- performance_outputList[[4]][[2]][[1]][[1]]
  df <- performance_outputList[[4]][[2]][[1]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "TimeBin", "OB mean accuracy", FALSE, .7, 1.1)
  
  
  
  ####"OB accuracy - timebin*shift"
  performance_outputList[[4]][[1]][[1]]
  performance_outputList[[4]][[2]]
  
  name <- performance_outputList[[4]][[2]][[2]][[1]]
  df <- performance_outputList[[4]][[2]][[2]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  lsmeans$new_var <- paste(lsmeans$lsmeans.TimeBin, lsmeans$lsmeans.Shift, sep = "_")
  
  ggplot(lsmeans, aes_string(x = colnames(lsmeans)[1], y = colnames(lsmeans)[3]))+
    geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                  width=.2,                    
                  position=position_dodge(.9))+
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    facet_grid(.~lsmeans.Shift)+
    coord_cartesian(ylim=c(.7,1))+
    labs(x="TmeBin" , y = "Mean OB accuracy") +
    geom_text(aes(label = round(lsmeans.lsmean, digits = 3), vjust=2)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
   geom_signif(comparisons = list(c(1, 2)), annotations=".000",  y_position = .965)+
   geom_signif(comparisons = list(c(1, 3)), annotations=".000",  y_position = .985) +
    ggtitle("Levent: PLease delete the sig between the nighttime. Thanks")
  
  
  
  ####"OB response_time - condition period"
  performance_outputList[[7]][[1]][[1]]
  performance_outputList[[7]][[2]]
  
  name <-performance_outputList[[7]][[2]][[1]][[2]][[1]]
  df <- performance_outputList[[7]][[2]][[1]][[2]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  
    ###On performance Graphing
  
  
  
  ####"OB response time - timebin"
  performance_outputList[[7]][[1]][[1]]
  performance_outputList[[7]][[2]][[1]][[1]][[1]]
  
  name <- performance_outputList[[7]][[2]][[1]][[1]][[1]]
  df <- performance_outputList[[7]][[2]][[1]][[1]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "TimeBin", "OB response time", FALSE, .3, 1.1)
  
  
  ####"PVT correct - timebin"
  performance_outputList[[8]][[1]][[1]]
  performance_outputList[[8]][[2]][[1]][[1]][[1]]
  
  name <- performance_outputList[[8]][[2]][[1]][[1]][[1]]
  df <- performance_outputList[[8]][[2]][[1]][[1]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "TimeBin", "PVT Correct", FALSE, .8, 1)
  
  
  ####"PVT response time - timebin"
  performance_outputList[[9]][[1]][[1]]
  performance_outputList[[9]][[2]][[1]][[1]][[1]]
  
  name <- performance_outputList[[9]][[2]][[1]][[1]][[1]]
  df <- performance_outputList[[9]][[2]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "TimeBin", "PVT response time", FALSE, .2, .6)

  