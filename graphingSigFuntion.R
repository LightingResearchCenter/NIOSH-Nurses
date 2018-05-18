
add_plotSigs <- function(means, comparisons, x_str, y_str, include_fill, x_lim, y_lim){
  
  
  

  


  
  if(x_str == "Condition period"){
    
    means$lsmeans.Condition_period <- ifelse(means$lsmeans.Condition_period == "b_b", "Blue baseline", 
                                             ifelse(means$lsmeans.Condition_period == "b_i", "Blue intervention", 
                                                    ifelse(means$lsmeans.Condition_period == "r_b", "Red baseline", 
                                                           ifelse(means$lsmeans.Condition_period == "r_i", "Red intervention", 
                                                                  ifelse(means$lsmeans.Condition_period == "w_b", "White baseline", "White intervention" )))))
    means$lsmeans.Condition_period <- factor(means$lsmeans.Condition_period, levels = c("Red baseline", "Red intervention", "Blue baseline", "Blue intervention", "White baseline", "White intervention"   ))
    
    

  }
  
  
  if(include_fill ){
    gg <- ggplot(means, aes_string(x = colnames(means)[1], y = colnames(means)[2], fill = colnames(means)[1]))+
      geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
      geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                    width=.2,                    
                    position=position_dodge(.9))+
      theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
      theme(legend.title=element_blank()) +
      coord_cartesian(ylim=c(x_lim,y_lim))+
      labs(x=x_str , y = y_str) +
      geom_text(aes(label = round(lsmeans.lsmean, digits = 3), vjust=2)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    
      
    
    if(colnames(means)[1] == lsmeans.Condition_period ){
      
      #means$means <- factor(means$means, levels = c("Red Light" , "Blue Light", "Dim light" ))
      
      gg <- gg +  scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90")) 

      
    }
    if(colnames(means)[1] == "lsmeans.condition2"){
      gg <- gg +  scale_fill_manual(values=c( "gray100",  "gray40")) 
      
    }
  }else{
    gg <- ggplot(means, aes_string(x = colnames(means)[1], y = colnames(means)[2]))+
      geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
      geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                    width=.2,                    
                    position=position_dodge(.9))+
      theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
      theme(legend.title=element_blank()) +
      coord_cartesian(ylim=c(x_lim,y_lim))+
      labs(x=x_str , y = y_str) +
      geom_text(aes(label = round(lsmeans.lsmean, digits = 3), vjust=2)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    
    
    
  }
  
  
  
  
  sig_start <- max(means$lsmeans.lsmean) 
  sig_inter <- max(means$lsmeans.SE) + (.5*max(means$lsmeans.SE)) 
  
  sig_nifComp <- comparisons[comparisons$contrasts.p.value < .06,]
  if(length(sig_nifComp$contrasts.contrast) > 0){
    for(i in 1:length(sig_nifComp$contrasts.contrast)){
      comparison_group <- strsplit(as.character(sig_nifComp$contrasts.contrast[i]), " - ")[[1]]
      
      gg <- gg + geom_signif(comparisons = list(c(comparison_group[1], comparison_group[2])), annotations=substr(as.character(sprintf("%.3f", round(sig_nifComp$contrasts.p.value[i], digits = 3))), 2, 5), y_position = sig_start + (i*sig_inter))
      
    }
  }
  
  
  return(gg)
}  



add_plotSigs2 <- function(means, comparisons, x_str, y_str){
  
  
  gg <- ggplot(means, aes_string(x = colnames(means)[1], y = colnames(means)[3]))+
    geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                  width=.2,                    
                  position=position_dodge(.9))+
    #scale_fill_manual(values=c("gray100", "gray80", "gray60", "gray40")) +
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    #coord_cartesian(ylim=c(.5,2))+
    labs(x=x_str , y = y_str) 
  
  
  sig_start <- max(means$lsmeans.lsmean) 
  sig_inter <- max(means$lsmeans.SE) + (.5*max(means$lsmeans.SE)) 
  
  sig_nifComp <- comparisons[comparisons$contrasts.p.value < .05,]
  if(length(sig_nifComp$contrasts.contrast) > 0){
    for(i in 1:length(sig_nifComp$contrasts.contrast)){
      comparison_group <- strsplit(as.character(sig_nifComp$contrasts.contrast[i]), " - ")[[1]]
      
      gg <- gg + geom_signif(comparisons = list(c(comparison_group[1], comparison_group[2])), annotations=substr(as.character(sprintf("%.3f", round(sig_nifComp$contrasts.p.value[i], digits = 3))), 2, 5), y_position = sig_start + (i*sig_inter))
      
    }
  }
  
  
  return(gg)
}  

####"GNG hit rate"
outputList[[1]][[1]][[1]]
outputList[[1]][[2]]

  name <- outputList[[1]][[2]][[1]][[1]]
  df <- outputList[[1]][[2]][[1]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "Mean hit rate", TRUE)
  
  
  name <- outputList[[1]][[2]][[2]][[1]]
  df <- outputList[[1]][[2]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Test period", "Mean GNG hit rate", FALSE)
  




#"GNG false_positve"
outputList[[2]][[1]][[1]]
outputList[[2]][[2]]

 name <- outputList[[2]][[2]][[1]][[1]][[1]]
 df <- outputList[[2]][[2]][[1]][[1]][[2]]
 
 
 lsmeans <- data.frame(summary(df)[1])
 lsmeansComparisons <- data.frame(summary(df)[2])
 add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "Mean false positve rate", TRUE)
 
 
 name <- outputList[[2]][[2]][[1]][[2]][[1]]
 df <- outputList[[2]][[2]][[1]][[2]][[2]]
 
 lsmeans <- data.frame(summary(df)[1])
 lsmeansComparisons <- data.frame(summary(df)[2])
 add_plotSigs(lsmeans, lsmeansComparisons, "Test period", "Mean GNG false positve rate", FALSE)
 
 
 
 name <- outputList[[2]][[2]][[2]][[1]]
 df <- outputList[[2]][[2]][[2]][[2]]
 
 lsmeans <- data.frame(summary(df)[1])
 lsmeansComparisons <- data.frame(summary(df)[2])
 add_plotSigs(lsmeans, lsmeansComparisons, "Session", "Mean GNG false positve rate", FALSE)
 
 
 
#GNG response time
outputList[[3]][[1]][[1]]
outputList[[3]][[2]]


  name <- outputList[[3]][[2]][[1]][[1]]
  df <- outputList[[3]][[2]][[1]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "Mean GNG response time (s)", TRUE)
  

  name <- outputList[[3]][[2]][[2]][[1]]
  df <- outputList[[3]][[2]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Light", "Mean GNG response time (s)", TRUE)
  
  



#outputList[[4]][[1]][[1]]
#outputList[[4]][[2]]

outputList[[5]][[1]][[1]]
outputList[[5]][[2]]

  name <- outputList[[5]][[2]][[1]]
  df <- outputList[[5]][[2]][[2]]
  

  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "GNG bottom 10% response time (s)", TRUE)
  
  
  
outputList[[6]][[1]][[1]]
outputList[[6]][[2]]
  


  name <- outputList[[6]][[2]][[1]][[1]]
  df <- outputList[[6]][[2]][[1]][[2]]
  
  

  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "GNG top 10% response time (s)", TRUE)
  

  name <- outputList[[6]][[2]][[2]][[1]]
  df <- outputList[[6]][[2]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Test period", "GNG top 10% response time (s)", FALSE)
  



outputList[[7]][[1]][[1]]
#outputList[[7]][[2]]

outputList[[8]][[1]][[1]]
#outputList[[8]][[2]]






outputList[[9]][[1]][[1]]
outputList[[9]][[2]]


  name <- outputList[[9]][[2]][[1]]
  df <- outputList[[9]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Session", "1-back mean accuracy", FALSE)
  

outputList[[10]][[1]][[1]]
outputList[[10]][[2]]


outputList[[11]][[1]][[1]]
outputList[[11]][[2]]

  name <- outputList[[11]][[2]][[1]]
  df <- outputList[[11]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Session", "1-back mean accuracy", FALSE)





outputList[[12]][[1]][[1]]
outputList[[12]][[2]]


  name <- outputList[[12]][[2]][[1]][[1]][[1]]
  df <- outputList[[12]][[2]][[1]][[1]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Light", "1-back respone time",  TRUE)
  

  name <- outputList[[12]][[2]][[1]][[2]][[1]]
  df <- outputList[[12]][[2]][[1]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Test period", "1-back respone time", FALSE)
  
  
  
  name <- outputList[[12]][[2]][[2]][[1]]
  df <- outputList[[12]][[2]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Session", "1-back respone time", FALSE)
  







outputList[[13]][[1]][[1]]
#outputList[[13]][[2]]





outputList[[14]][[1]][[1]]
outputList[[14]][[2]]

  
  name <- outputList[[14]][[2]][[1]][[1]]
  df <- outputList[[14]][[2]][[1]][[2]]

  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Session", "2-back mean accuracy", FALSE)
  
  
  name <- outputList[[14]][[2]][[2]][[1]]
  df <- outputList[[14]][[2]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  #add_plotSigs2(lsmeans, lsmeansComparisons, "Light:Test period", "2-back mean accuracy")
  ########******* ###********** fix

  gg <- ggplot(lsmeans, aes(x = lsmeans.condition2, y = lsmeans.lsmean, fill = lsmeans.light))+
    geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                  width=.2,                    
                  position=position_dodge(.9))+
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    scale_fill_manual(values=c("red4", "deepskyblue4",  "gray80" )) +
    labs(x="Caffeine" , y = "2-back mean accuracy") +
    geom_signif(comparisons = list(c("Caffe:Blue", "Place:Blue")), annotations=".017" )
  
  
outputList[[15]][[1]][[1]]
outputList[[15]][[2]]


  name <- outputList[[15]][[2]][[1]]
  df <- outputList[[15]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Session", "2-back correct matches", FALSE)
  

outputList[[16]][[1]][[1]]
outputList[[16]][[2]]
  
  name <- outputList[[16]][[2]][[1]][[1]]
  df <- outputList[[16]][[2]][[1]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Session", "2-back correct no-matches", FALSE)
  
  
  name <- outputList[[16]][[2]][[2]][[1]]
  df <- outputList[[16]][[2]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs2(lsmeans, lsmeansComparisons, "Caffeine:light", "2-back correct matches")
  ########******* ###********** fix
  
  gg <- ggplot(lsmeans, aes(x = lsmeans.condition2, y = lsmeans.lsmean, fill = lsmeans.light))+
    geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    geom_errorbar(aes(ymin=lsmeans.lsmean-lsmeans.SE, ymax=lsmeans.lsmean+lsmeans.SE),
                  width=.2,                    
                  position=position_dodge(.9))+
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    scale_fill_manual(values=c("red4", "deepskyblue4",  "gray80" )) +
    labs(x="Caffeine" , y = "2-back mean correct no-matches") +
    geom_signif(comparisons = list(c("Caffe:Blue", "Place:Blue")), annotations=".017" )

outputList[[17]][[1]][[1]]
outputList[[17]][[2]]



  name <- outputList[[17]][[2]][[1]][[1]]
  df <- outputList[[17]][[2]][[1]][[2]]
  
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "2-back response time",  TRUE)
  
  
  name <- outputList[[17]][[2]][[2]][[1]]
  df <- outputList[[17]][[2]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Test period", "2-back response time", FALSE)
  




outputList[[18]][[1]][[1]]
#outputList[[18]][[2]]


outputList[[19]][[1]][[1]]
outputList[[19]][[2]]
  
  name <- outputList[[19]][[2]][[1]]
  df <- outputList[[19]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Session", "MOB 1-back mean accuracy", FALSE)
  



outputList[[20]][[1]][[1]]
outputList[[20]][[2]]

  name <- outputList[[20]][[2]][[1]]
  df <- outputList[[20]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Session", "MOB 1-back mean correct matches", FALSE)
  



outputList[[21]][[1]][[1]]
outputList[[21]][[2]]

  name <- outputList[[21]][[2]][[1]]
  df <- outputList[[21]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Session", "MOB 1-back mean correct no-matches", FALSE)
  



outputList[[22]][[1]][[1]]
outputList[[22]][[2]]

  name <- outputList[[22]][[2]][[1]][[1]][[1]]
  df <- outputList[[22]][[2]][[1]][[1]][[2]]
  
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Caffeine", "MOB 1-back response time",  TRUE)
  
  
  name <- outputList[[2]][[2]][[1]][[2]][[1]]
  df <- outputList[[2]][[2]][[1]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Test period", "MOB 1-back response time", FALSE)
  
  
  
  name <- outputList[[2]][[2]][[2]][[1]]
  df <- outputList[[2]][[2]][[2]][[2]]
  
  lsmeans <- data.frame(summary(df)[1])
  lsmeansComparisons <- data.frame(summary(df)[2])
  add_plotSigs(lsmeans, lsmeansComparisons, "Session", "MOB 1-back response time", FALSE)
  




outputList[[23]][[1]][[1]]
#length(outputList[[23]][[2]])



  

