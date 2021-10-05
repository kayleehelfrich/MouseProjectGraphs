rm(list=ls())
library("ggpubr")
setwd("~/KayleeStuff/Smith_Lab/Data/Mouse_Validation_Data/Maternal_pSMAD_SMAD")

File <- "MatLiv_pSMAD_SMAD_TP_FinalValues.csv"
data <- read.csv(File, header=TRUE)
data_frame <- data.frame(data)

CON <- data_frame[is.element(data_frame$Group, "CON"),]
ALC <- data_frame[is.element(data_frame$Group, "ALC"),]

sets <- data.frame(CON$pSMAD.SMAD.TP)
sets$ALC <- ALC$pSMAD.SMAD.TP
colnames(sets)[1] <- "CON" #renaming the first column

Groups <- c(sets$CON, sets$ALC)
tech_repl <- 8 #number of biological replicates
set_nums <- 1 #number of technical plate replicates
reps <- tech_repl*set_nums #reps should match the number of rows
groups <- c(rep("CON", reps), rep("ALC", reps))

join <- data.frame(Groups, groups)
#Checking for the assumption of equal variance
bartlett.test(Groups, groups)
#checking for assumption of normality
shapiro.test(data_frame$pSMAD.SMAD.TP)
hist(data_frame$pSMAD.SMAD.TP)
qqnorm(data_frame$pSMAD.SMAD.TP)
qqline(data_frame$pSMAD.SMAD.TP)

#student's t-test for normal data
t.test(sets$CON, sets$ALC)

#Wilcoxon rank-sum test for non-normal data
wilcox.test(sets$CON, sets$ALC, alternative = "two.sided", exact = FALSE)

#Outlier Test
summary(CON$pSTAT3.STAT3)
summary(ALC$pSTAT3.STAT3)