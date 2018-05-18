library(readxl)

Phasor_summary <- read_excel("//root/projects/NIOSH_RedLightForShiftWorkers/daysimeter_data/tables/2018-02-22_1645 Phasor summary.xlsx",  sheet = "cropped_2018-02-21_1019")





Phasor_summary$Condition_period <- paste(Phasor_summary$color, Phasor_summary$protocol, sep = "_")

Phasor_summary$id <- as.factor(Phasor_summary$id)
Phasor_summary$shift <- as.factor(Phasor_summary$shift)
Phasor_summary$Condition_period <- as.factor(Phasor_summary$Condition_period)
Phasor_summary$protocol <- as.factor(Phasor_summary$protocol)

Phasor_summary$phasor_magnitude <- as.numeric(Phasor_summary$phasor_magnitude)
Phasor_summary$phasor_angle <- as.numeric(Phasor_summary$phasor_angle)

Phasor_summary2 <- subset(Phasor_summary, !is.na(phasor_magnitude) & !is.na(phasor_angle) & protocol != "unknown" & color != "unknown" & shift != "unknown")

Phasor_summary3 <-  Phasor_summary2 %>% group_by(id) %>% filter(n()>5) 

PhasorAngle_model <- lme(phasor_angle ~ Condition_period*shift , random =~1|id/color/protocol,
                      data=Phasor_summary3)




PhasorMag_model <- lme(phasor_magnitude ~ Condition_period*shift , random = ~1|id/color/protocol,
                         data=Phasor_summary2)


