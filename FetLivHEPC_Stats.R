rm(list=ls())
library("ggpubr")
setwd("C:/Users/kaylee/Documents/Kaylee_Stuff/Smith_Lab/Data/Mouse_Validation_Data/Fetal_Hepcidin")
PCR_file <- "FetLivHEPC_Ctcalcs2_Combined.csv"

data <- read.csv(PCR_file, header=TRUE)
data_frame <- data.frame(data)

MD <- data_frame[is.element(data_frame$Biological.Sets, "MD"),]
EtOH <- data_frame[is.element(data_frame$Biological.Sets, "EtOH"),]

sets <- data.frame(MD$Cq.Averages)
sets$EtOH <- EtOH$Cq.Averages
colnames(sets)[1] <- "MD" #renaming the first column

treatments <- c(sets$MD, sets$EtOH)
tech_repl <- 9 #number of biological replicates
set_nums <- 2 #number of technical plate replicates
reps <- tech_repl*set_nums #reps should match the number of rows
groups <- c(rep("MD", reps), rep("EtOH", reps))

join <- data.frame(treatments, groups)
#Checking for the assumption of equal variance
bartlett.test(treatments, groups)
#checking for assumption of normality
shapiro.test(data_frame$Cq.Averages)
hist(data_frame$Cq.Averages)
qqnorm(data_frame$Cq.Averages)
qqline(data_frame$Cq.Averages)

#student's t-test for normal data
t.test(sets$MD, sets$EtOH)

#Wilcoxon rank-sum test for non-normal data
wilcox.test(sets$MD, sets$EtOH, alternative = "two.sided", exact = FALSE)

#Outlier Test
summary(MD$Cq.Averages)
summary(EtOH$Cq.Averages)

library(ggplot2)
order <- c("MD", "EtOH")
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
