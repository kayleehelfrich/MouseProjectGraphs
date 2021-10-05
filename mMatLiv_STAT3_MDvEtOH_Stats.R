rm(list=ls())
library("ggpubr")
setwd("~/KayleeStuff/Smith_Lab/Data/Mouse_Validation_Data/Maternal_pSTAT_STAT")
File <- "Final Combined Data_STAT-TP.csv"

data <- read.csv(File, header=TRUE)
data_frame <- data.frame(data)

MD <- data_frame[is.element(data_frame$Treatment, "MD"),]
PAE <- data_frame[is.element(data_frame$Treatment, "PAE"),]

sets <- data.frame(MD$STAT3.TP)
sets$PAE <- PAE$STAT3.TP
colnames(sets)[1] <- "MD" #renaming the first column

treatments <- c(sets$MD, sets$PAE)
tech_repl <- 8 #number of biological replicates
set_nums <- 1 #number of technical plate replicates
reps <- tech_repl*set_nums #reps should match the number of rows
groups <- c(rep("MD", reps), rep("PAE", reps))

join <- data.frame(treatments, groups)
#Checking for the assumption of equal variance
bartlett.test(treatments, groups)
#checking for assumption of normality
shapiro.test(data_frame$STAT3.TP)
hist(data_frame$STAT3.TP)
qqnorm(data_frame$STAT3.TP)
qqline(data_frame$STAT3.TP)

#student's t-test for normal data
t.test(sets$MD, sets$PAE)

#Wilcoxon rank-sum test for non-normal data
wilcox.test(sets$MD, sets$PAE, alternative = "two.sided", exact = FALSE)

#Outlier Test
summary(MD$pSTAT3.STAT3)
summary(PAE$pSTAT3.STAT3)

library(ggplot2)
order <- c("MD", "PAE")
p <- ggplot(merge, aes(x=groups, y=treatments)) + 
  geom_boxplot(outlier.shape=NA) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0)) + 
  theme(plot.title = element_text(hjust = 0.5, size=18), 
        axis.title.x = element_blank(), #gets rid of x-axis label
        panel.grid.major = element_blank(), #gets rid of major gridlines
        panel.grid.minor = element_blank(), #gets rid of minor gridlines
        panel.background = element_blank(), #turns background white instead of gray
        axis.line = element_line(colour = "black"), 
        legend.position="none", #gets rid of legend
        axis.text=element_text(size=18, color = "black"), #sets size of x and y axis labels
        axis.title=element_text(size=14,face="bold")) +
  scale_x_discrete (limits = order)

p+labs(title=("Fetal Liver Hepcidin Expression: Ct Values"), y = ("Fetal Hepc/Gapdh dCt")) +
  scale_fill_manual(values = c("#3399cc", "#0000FF"))
