rm(list=ls())
library("ggpubr")
setwd("~/KayleeStuff/Smith_Lab/Data/Mouse_Validation_Data/Hematology")
File <- "AIN93G_Maternal_Mouse_Hematology_CLEAN.csv"

data <- read.csv(File, header=TRUE)
data_frame <- data.frame(data)

MD <- data_frame[is.element(data_frame$Treatment, "MD"),]
PAE <- data_frame[is.element(data_frame$Treatment, "PAE"),]

sets <- data.frame(MD$ZnPP)
sets$PAE <- PAE$ZnPP
colnames(sets)[1] <- "MD" #renaming the first column

treatments <- c(sets$MD, sets$PAE)
tech_repl <- 10 #number of biological replicates
set_nums <- 1 #number of technical plate replicates
reps <- tech_repl*set_nums #reps should match the number of rows
groups <- c(rep("MD", reps), rep("PAE", reps))

join <- data.frame(treatments, groups)
#Checking for the assumption of equal variance
bartlett.test(treatments, groups)
#checking for assumption of normality
shapiro.test(data_frame$ZnPP)
hist(data_frame$ZnPP)
qqnorm(data_frame$ZnPP)
qqline(data_frame$ZnPP)

#student's t-test for normal data
t.test(sets$MD, sets$PAE)

#Wilcoxon rank-sum test for non-normal data
wilcox.test(sets$MD, sets$PAE, alternative = "two.sided", exact = FALSE)

#Outlier Test
summary(MD$pSTAT3.STAT3)
summary(PAE$pSTAT3.STAT3)