library(ReporteRs)
library(magrittr)




if(FALSE){
  
  ###transform summary to data.frame
  
  ###Function to import a model
  df1 <- data.frame(anova(hitrate_model))
  ###take df1 into a table and output into word
  df2 <- summary(lsmeans(hitrate_model, pairwise~ session, adjust="tukey", data = hit_rate))
  df2 <- lsmeans(hitrate_model, pairwise~ session, adjust="tukey", data = hit_rate)
  
  ###The fitst data frame will be used for graphing
  df3<- data.frame(df[1])
  ###this data frame will be used for finding significane differences
  df4<- data.frame(df[2])
  
  performance_outputList[[1]][[1]][[1]]
  performance_outputList[[1]][[1]][[2]]
  performance_outputList[[1]][[1]][[3]]
  
  performance_outputList[[1]][[2]][[2]][[1]]
}
RUN <- FALSE
if(RUN){
  outputList <- actiwatchNorm_outputList
  wordTableGenerator(outputList, FALSE)
  
  for(i in 1:length(outputList)){
    print(outputList[[i]][[1]][[1]])
    print(length(outputList[[i]][[2]]))
    #print(length(outputList[[i]][[2]]))
  }
  
}



wordTableGenerator <- function(outputList, post_hoc){
  library(ReporteRs)
  library(magrittr)
  doc = docx()

  doc = addParagraph( doc, "A FlexTable example", stylename = "TitleDoc" )
  # add a section title
  doc = addTitle( doc, "How to turn a data.frame into a Word table", level = 1 )
  
  for(i in 1:length(outputList)){
    
    if(!(post_hoc)){
      currTitle <- outputList[[i]][[1]]
      
      currDF <- outputList[[i]][[3]]
      
      
      colnames(currDF)[1] <- "df"
      colnames(currDF)[2] <- "Error"
      colnames(currDF)[3] <- "F"
      colnames(currDF)[4] <- "p"
      currDF <- currDF[2:length(currDF$p),]
      
      currDF$p <-  substr(as.character(sprintf("%.3f", round(currDF$p, digits = 3))), 2, 5)
      
      currDF$F <-  as.character(round(currDF$F, digits = 3))
      
      doc = addParagraph( doc, currTitle, stylename = "DocDefaults" )
      
      MyFTable = FlexTable( data = currDF, add.rownames = TRUE )
      
      doc = addFlexTable(doc, MyFTable)
      
      # write the doc
      
      dir <- "//root/projects/NIOSH_RedLightForShiftWorkers/Performance_data/"
      filename <- paste0(dir,format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "NursesPerformance-No-PostHoc.docx")
      

    }else{
      
      currTitle <- outputList[[i]][[1]][[1]]
      
      currDF <- outputList[[i]][[1]][[3]]
      
      
      colnames(currDF)[1] <- "df"
      colnames(currDF)[2] <- "Error"
      colnames(currDF)[3] <- "F"
      colnames(currDF)[4] <- "p"
      currDF <- currDF[2:length(currDF$p),]
      
      currDF$p <-  substr(as.character(sprintf("%.3f", round(currDF$p, digits = 3))), 2, 5)
      
      currDF$F <-  as.character(round(currDF$F, digits = 3))
      
      doc = addParagraph( doc, currTitle, stylename = "DocDefaults" )
      
      MyFTable = FlexTable( data = currDF, add.rownames = TRUE )
      
      doc = addFlexTable(doc, MyFTable)
      
      # write the doc
      dir <- "//root/projects/NIOSH_RedLightForShiftWorkers/Performance_data/"
      filename <- paste0(dir,format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "NursesPerformance-No-PostHoc.docx")
      
    }
    
    

  }  
  writeDoc( doc, file = filename )
  
  # open the Word doc
 # browseURL(filename)
}


