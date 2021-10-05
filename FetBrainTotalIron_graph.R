rm(list=ls())
library(ggplot2)
setwd("C:/Users/kaylee/Documents/Kaylee_Stuff/Smith_Lab/Data/Fetal_Brain_Mineral_Analysis")
Fe_file <- "mFetBrain_TotalIron_ForGraph.csv"

#reads a file in a table format and creates a data frame from it
data <- read.csv(Fe_file, header=TRUE) 
data_frame <- data.frame(data)

#initialize a high resolution graph
png("FetBrain_TotalIron_Bargraph.png", units="in", width=7, height=7, res=600) 

sets <- c("MD", "Ethanol")
p<- ggplot(data_frame, aes(x=Biological.Sets, y=Average, fill=sets)) + 
  geom_bar(stat="identity", color="black", #this "color" sets the outline color
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Lower.error, ymax=Upper.error), width=.2,
                position=position_dodge(.9)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,0.2)) + #to make the graph sit on the bottom without the space and to set the max y-axis limit
  # ylim(0,120) +
  theme(plot.title = element_text(hjust = 0.5, size=18), 
        axis.title.x = element_blank(), #gets rid of x-axis label
        panel.grid.major = element_blank(), #gets rid of major gridlines
        panel.grid.minor = element_blank(), #gets rid of minor gridlines
        panel.background = element_blank(), #turns background white instead of gray
        axis.line = element_line(colour = "black"), 
        legend.position="none", #gets rid of legend
        axis.text=element_text(size=18, color = "black"), #sets size of x and y axis labels
        axis.title=element_text(size=20,face="bold")) + scale_x_discrete (limits = sets)
print(p)

# Finished bar plot
p+labs(title=("Fetal Brain Total Iron"), y = ("Total iron normalized to sample dry weight (mg/g)")) +
  scale_fill_manual(values = c("#3399cc", "#0000FF"))

#release PNG file graph into directory
dev.off() 
