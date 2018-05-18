

library(nlme)
library(lsmeans)
library(ggplot2)
library(Rmisc)
library(MuMIn)

acti_watch <- read_excel("//root/projects/NIOSH_RedLightForShiftWorkers/Actiware data/acti-watchSummary_2-12-18.xlsx")

ISIV <- read_excel("//root/projects/NIOSH_RedLightForShiftWorkers/Actiware data/NIOSH_Nurses_Actiwatch_BaselineIntervention_ISIV.xlsx")

ISIV$Condition_period <- paste(ISIV$light, ISIV$period, sep = " ")
ISIV$Shift <- as.factor(ifelse(as.numeric(ISIV$subject) > 140, "Night", "Day"))
ISIV$Condition_period <- factor(ISIV$Condition_period, levels = c("red baseline", "red intervention", "blue baseline", "blue intervention", "white baseline", "white intervention"   ))


IV_sum <- summarySE(ISIV, measurevar="IV", groupvars=c("Condition_period", "Shift", "light", "period"))

ggplot(IV_sum, aes(x=period, y=IV, fill = Condition_period)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=IV-se, ymax=IV+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + facet_grid(.~Shift* light) +
  #coord_cartesian(ylim=c(19,21))+
  geom_text(aes(label = round(IV, digits = 3), vjust=2))+
  scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


IS_sum <- summarySE(ISIV, measurevar="IS", groupvars=c("Condition_period", "Shift", "light", "period"))

ggplot(IS_sum, aes(x=period, y=IS, fill = Condition_period)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=IS-se, ymax=IS+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + facet_grid(.~Shift* light) +
  #coord_cartesian(ylim=c(19,21))+
  geom_text(aes(label = round(IS, digits = 3), vjust=2))+
  scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

t.test(IS ~ period, data = ISIV[ISIV$light == "red" & ISIV$Shift == "Night",], paired = TRUE)



ggplot(IS_sum[IS_sum$Shift == "Night",], aes(x=Condition_period, y=IS, fill = Condition_period)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=IS-se, ymax=IS+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  #coord_cartesian(ylim=c(19,21))+
  geom_text(aes(label = round(IS, digits = 3), vjust=2))+
  scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_signif(comparisons = list(c("red baseline", "red intervention")), annotations=".044")+
  labs( x = "Condition period for Night shift workers") 


###Actiwear graphs

acti_watch$Condition_period <- paste(acti_watch$light, acti_watch$period, sep = " ")
acti_watch$Shift <- as.factor(ifelse(as.numeric(acti_watch$subject) > 140, "Night", "Day"))
acti_watch$Condition_period <- factor(acti_watch$Condition_period, levels = c("red baseline", "red intervention", "blue baseline", "blue intervention", "white baseline", "white intervention"   ))

acti_watch$Duration <- as.numeric(acti_watch$Duration )
acti_watch$`Onset Latency` <- as.numeric(acti_watch$`Onset Latency` )
acti_watch$Efficiency <- as.numeric(acti_watch$Efficiency )
acti_watch$WASO <- as.numeric(acti_watch$WASO )
acti_watch$`Wake Time` <- as.numeric(acti_watch$`Wake Time` )
acti_watch$`%Wake` <- as.numeric(acti_watch$`%Wake`  )
acti_watch$`Sleep Time` <- as.numeric(acti_watch$`Sleep Time`)
acti_watch$`%Sleep` <- as.numeric(acti_watch$`%Sleep`)
colnames(acti_watch)[8] <- "Onset_Latency"
colnames(acti_watch)[11] <- "Wake_Time"
colnames(acti_watch)[12] <- "Wake_percent"
colnames(acti_watch)[13] <- "Sleep_Time"
colnames(acti_watch)[14] <- "Sleep_percent"

Duration_sum <- summarySE(acti_watch, measurevar="Duration", groupvars=c("Condition_period", "Shift", "light", "period"))

ggplot(Duration_sum, aes(x=period, y=Duration, fill = Condition_period)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Duration-se, ymax=Duration+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + facet_grid(.~Shift* light) +
  #coord_cartesian(ylim=c(19,21))+
  geom_text(aes(label = round(Duration, digits = 3), vjust=2))+
  scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

Onset_Latency_sum <- summarySE(acti_watch, measurevar="Onset_Latency", groupvars=c("Condition_period", "Shift", "light", "period"))

ggplot(Onset_Latency_sum, aes(x=period, y=Onset_Latency, fill = Condition_period)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Onset_Latency-se, ymax=Onset_Latency+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + facet_grid(.~Shift* light) +
  #coord_cartesian(ylim=c(19,21))+
  geom_text(aes(label = round(Onset_Latency, digits = 3), vjust=2))+
  scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs( y = "Onset Latency") 

Wake_Time_sum <- summarySE(acti_watch, measurevar="Wake_Time", groupvars=c("Condition_period", "Shift", "light", "period"))

ggplot(Wake_Time_sum, aes(x=period, y=Wake_Time, fill = Condition_period)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Wake_Time-se, ymax=Wake_Time+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + facet_grid(.~Shift* light) +
  #coord_cartesian(ylim=c(19,21))+
  geom_text(aes(label = round(Wake_Time, digits = 3), vjust=2))+
  scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs( y = "Wake time") 

Wake_percent_sum <- summarySE(acti_watch, measurevar="Wake_percent", groupvars=c("Condition_period", "Shift", "light", "period"))

ggplot(Wake_percent_sum, aes(x=period, y=Wake_percent, fill = Condition_period)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Wake_percent-se, ymax=Wake_percent+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + facet_grid(.~Shift* light) +
  #coord_cartesian(ylim=c(19,21))+
  geom_text(aes(label = round(Wake_percent, digits = 3), vjust=2))+
  scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs( y = "Wake percent") 

Sleep_Time_sum <- summarySE(acti_watch, measurevar="Sleep_Time", groupvars=c("Condition_period", "Shift", "light", "period"))

ggplot(Sleep_Time_sum, aes(x=period, y=Sleep_Time, fill = Condition_period)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Sleep_Time-se, ymax=Sleep_Time+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + facet_grid(.~Shift* light) +
  #coord_cartesian(ylim=c(19,21))+
  geom_text(aes(label = round(Sleep_Time, digits = 3), vjust=2))+
  scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs( y = "Sleep time") 

Sleep_percent_sum <- summarySE(acti_watch, measurevar="Sleep_percent", groupvars=c("Condition_period", "Shift", "light", "period"))

ggplot(Sleep_percent_sum, aes(x=period, y=Sleep_percent, fill = Condition_period)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Sleep_percent-se, ymax=Sleep_percent+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + facet_grid(.~Shift* light) +
  coord_cartesian(ylim=c(90,100))+
  geom_text(aes(label = round(Sleep_percent, digits = 3), vjust=2))+
  scale_fill_manual(values=c( "red4", "red1", "deepskyblue4", "deepskyblue2", "gray80", "gray90"))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs( y = "Sleep percent") 


