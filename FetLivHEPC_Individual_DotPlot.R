rm(list=ls())
setwd("C:/Users/kaylee/Documents/Kaylee_Stuff/Smith_Lab/Data/FetLiv_Cytokines/FetLiv_IL10")
Alldata_file <- "FetLivIL10_Ctcalcs3_Combined.csv"
library("ggplot2")

data <- read.csv(Alldata_file, header=TRUE)
df <- data.frame(data)

#subset data from total data frame into data frames for individual treatments
dfIS_0 <- subset(df, Biological.Sets == "IS-0", select = c("Experiment", "Cq.Averages", "Sample.IDs", "Lower.Err", "Upper.Err"))
dfIS_5 <- subset(df, Biological.Sets == "IS-5", select = c("Experiment", "Cq.Averages", "Sample.IDs", "Lower.Err", "Upper.Err"))
dfID_0 <- subset(df, Biological.Sets == "ID-0", select = c("Experiment", "Cq.Averages", "Sample.IDs", "Lower.Err", "Upper.Err"))
dfID_5 <- subset(df, Biological.Sets == "ID-5", select = c("Experiment", "Cq.Averages", "Sample.IDs", "Lower.Err", "Upper.Err"))
dfIF_0 <- subset(df, Biological.Sets == "IF-0", select = c("Experiment", "Cq.Averages", "Sample.IDs", "Lower.Err", "Upper.Err"))
dfIF_5 <- subset(df, Biological.Sets == "IF-5", select = c("Experiment", "Cq.Averages", "Sample.IDs", "Lower.Err", "Upper.Err"))

#Change the Sample ID's into characters
dfIS_0$Sample.IDs <- as.character(dfIS_0$Sample.IDs) 
dfIS_5$Sample.IDs <- as.character(dfIS_5$Sample.IDs)
dfID_0$Sample.IDs <- as.character(dfID_0$Sample.IDs)
dfID_5$Sample.IDs <- as.character(dfID_5$Sample.IDs)
dfIF_0$Sample.IDs <- as.character(dfIF_0$Sample.IDs)
dfIF_5$Sample.IDs <- as.character(dfIF_5$Sample.IDs)

#Change the Experiment #'s into characters
dfIS_0$Experiment <- as.character(dfIS_0$Experiment)
dfIS_5$Experiment <- as.character(dfIS_5$Experiment)
dfID_0$Experiment <- as.character(dfID_0$Experiment)
dfID_5$Experiment <- as.character(dfID_5$Experiment)
dfIF_0$Experiment <- as.character(dfIF_0$Experiment)
dfIF_5$Experiment <- as.character(dfIF_5$Experiment)

#par(mfrow=c(2,1))

#Create the plot for IS-0
png("IS0 Fetal Liver IL10 Figure 3.png", units="in", width=7, height=7, res=600)
plotIS_0 <- ggplot(dfIS_0, aes(x = Sample.IDs, y = Cq.Averages, 
                        ymin = Lower.Err, 
                        ymax = Upper.Err)) +
   geom_pointrange(aes(color = Experiment)) +
  coord_cartesian(ylim = c(3, 7)) +
  ggtitle("IS-0 Fetal Liver IL-10") +
  xlab("Fetus") + 
  ylab("Average Cq") +
  theme(panel.background = element_blank(), 
        legend.position="right", 
        plot.title = element_text(hjust = 0.5, size=16), 
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"), 
        panel.border = element_blank(), axis.line.x = element_line(), axis.line.y = element_line())
print(plotIS_0)
dev.off() 

#Create the plot for IS-5
png("IS5 Fetal Liver IL-10 Figure 3.png", units="in", width=7, height=7, res=600)
plotIS_5 <- ggplot(dfIS_5, aes(x = Sample.IDs, y = Cq.Averages, 
                               ymin = Lower.Err, 
                               ymax = Upper.Err)) +
  geom_pointrange(aes(color = Experiment)) +
  coord_cartesian(ylim = c(3, 7)) +
  ggtitle("IS-5 Fetal Liver IL-10") +
  xlab("Fetus") + 
  ylab("Average Cq") +
  theme(panel.background = element_blank(), 
        legend.position="right", 
        plot.title = element_text(hjust = 0.5, size=16), 
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"), 
        panel.border = element_blank(), axis.line.x = element_line(), axis.line.y = element_line())
print(plotIS_5)
dev.off() 

#Create the plot for ID-0
png("ID0 Fetal Liver IL-10 Figure 3.png", units="in", width=7, height=7, res=600)
plotID_0 <- ggplot(dfID_0, aes(x = Sample.IDs, y = Cq.Averages, 
                               ymin = Lower.Err, 
                               ymax = Upper.Err)) +
  geom_pointrange(aes(color = Experiment)) +
  coord_cartesian(ylim = c(3, 7)) +
  ggtitle("ID-0 Fetal Liver IL-10") +
  xlab("Fetus") + 
  ylab("Average Cq") +
  theme(panel.background = element_blank(), 
        legend.position="right", 
        plot.title = element_text(hjust = 0.5, size=16), 
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"), 
        panel.border = element_blank(), axis.line.x = element_line(), axis.line.y = element_line())
print(plotID_0)
dev.off() 

#Create the plot for ID-5
png("ID5 Fetal Liver IL-10 Figure 3.png", units="in", width=7, height=7, res=600)
plotID_5 <- ggplot(dfID_5, aes(x = Sample.IDs, y = Cq.Averages, 
                               ymin = Lower.Err, 
                               ymax = Upper.Err)) +
  geom_pointrange(aes(color = Experiment)) +
  coord_cartesian(ylim = c(3, 7)) +
  ggtitle("ID-5 Fetal Liver IL-10") +
  xlab("Fetus") + 
  ylab("Average Cq") +
  theme(panel.background = element_blank(), 
        legend.position="right", 
        plot.title = element_text(hjust = 0.5, size=16), 
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"), 
        panel.border = element_blank(), axis.line.x = element_line(), axis.line.y = element_line())
print(plotID_5)
dev.off() 

#Create the plot for IF-0
png("IF0 Fetal Liver IL-10 Figure 3.png", units="in", width=7, height=7, res=600)
plotIF_0 <- ggplot(dfIF_0, aes(x = Sample.IDs, y = Cq.Averages, 
                               ymin = Lower.Err, 
                               ymax = Upper.Err)) +
  geom_pointrange(aes(color = Experiment)) +
  coord_cartesian(ylim = c(3, 7)) +
  ggtitle("IF-0 Fetal Liver IL-10") +
  xlab("Fetus") + 
  ylab("Average Cq") +
  theme(panel.background = element_blank(), 
        legend.position="right", 
        plot.title = element_text(hjust = 0.5, size=16), 
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"), 
        panel.border = element_blank(), axis.line.x = element_line(), axis.line.y = element_line())
print(plotIF_0)
dev.off() 

#Create the plot for IF-5
png("IF5 Fetal Liver IL-10 Figure 3.png", units="in", width=7, height=7, res=600)
plotIF_5 <- ggplot(dfIF_5, aes(x = Sample.IDs, y = Cq.Averages, 
                               ymin = Lower.Err, 
                               ymax = Upper.Err)) +
  geom_pointrange(aes(color = Experiment)) +
  coord_cartesian(ylim = c(3, 7)) +
  ggtitle("IF-5 Fetal Liver IL-10") +
  xlab("Fetus") + 
  ylab("Average Cq") +
  theme(panel.background = element_blank(), 
        legend.position="right", 
        plot.title = element_text(hjust = 0.5, size=16), 
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"), 
        panel.border = element_blank(), axis.line.x = element_line(), axis.line.y = element_line())
print(plotIF_5)
dev.off() 

