rm(list=ls())
library(ggplot2)
library(extrafont)
loadfonts(device = "win")
setwd("~/KayleeStuff/Smith_Lab/Data/Mouse_Validation_Data/Fetal_pSTAT_STAT")


### Reads a file in a table format and creates a data frame from it. This figures will be in the first panel.
datafile <- "FetLiv_pSTAT_STAT3_TP_FinalValues.csv"
data1 <- read.csv(datafile, header=TRUE) 
dataframe_Original <- data.frame(data1)

#STAT

## Create a dot plot overlaid on a bar chart
# Initialize an individual high resolution graph
png("mFetLiv_STAT3-TP_MDvPAE_DotPlot_B&W.png", units="in", width=4, height=7, res=600)

# Sets order of samples for future steps
order <- c("CON", "ALC")

plot <- ggplot(data= dataframe_Original, aes(x=Group, y=STAT3.Average), fill=order) + #creates original graph
  geom_bar(stat="identity", color="black", aes(fill = Group),
           position=position_dodge()) +
  scale_fill_manual(values=c("CON" = "grey100", "ALC" = "grey51")) +
  geom_errorbar(aes(ymin=STAT3.Lower, ymax=STAT3.Upper), width=.2,
                position=position_dodge(2)) + 
  geom_point(aes(x=Group, y=STAT3.TP),
             size = 2, shape = 19, position = position_jitter(width=.15, height=0)) + #adds individual points
  ggtitle("STAT3") +
  scale_y_continuous(limits = c(0,3), expand = c(0,0)) + #this sets the minimum and maximum values shown on the y-axis
  ylab(expression("STAT3/Total Protein Relative to CON")) + #adds y-axis label
  scale_x_discrete (limits = order) + #rearranges the x-axis in correct order
  theme(plot.title = element_text(hjust = 0.5, size=28, family = "Times New Roman"), 
        axis.title.x = element_blank(), #gets rid of x-axis label
        axis.title.y = element_text(size = 24, family = "Times New Roman"),
        panel.grid.major = element_blank(), #gets rid of major gridlines
        panel.grid.minor = element_blank(), #gets rid of minor gridlines
        panel.background = element_blank(), #turns background white instead of gray
        axis.line = element_line(colour = "black"), 
        legend.position="none", #gets rid of legend
        axis.text=element_text(size=26, color = "black", family = "Times New Roman"), #sets size of x and y axis labels
        axis.text.x=element_text(family = "Times New Roman"),
        axis.title=element_text(size=26,face="bold", family = "Times New Roman"))

print(plot)
dev.off() 

# pSTAT

## Create a dot plot overlaid on a bar chart
# Initialize an individual high resolution graph
png("mFetLiv_pSTAT3-TP_MDvPAE_DotPlot_B&W.png", units="in", width=4, height=7, res=600)

# Sets order of samples for future steps
order <- c("CON", "ALC")

plot <- ggplot(data= dataframe_Original, aes(x=Group, y=pSTAT3.Average), fill=order) + #creates original graph
  geom_bar(stat="identity", color="black", aes(fill = Group),
           position=position_dodge()) +
  scale_fill_manual(values=c("CON" = "grey100", "ALC" = "grey51")) +
  geom_errorbar(aes(ymin=pSTAT3.Lower, ymax=pSTAT3.Upper), width=.2,
                position=position_dodge(2)) + 
  geom_point(aes(x=Group, y=pSTAT3.TP),
             size = 2, shape = 19, position = position_jitter(width=.15, height=0)) + #adds individual points
  ggtitle("pSTAT3") +
  scale_y_continuous(limits = c(0,5), expand = c(0,0)) + #this sets the minimum and maximum values shown on the y-axis
  ylab(expression("pSTAT3/Total Protein Relative to CON")) + #adds y-axis label
  scale_x_discrete (limits = order) + #rearranges the x-axis in correct order
  theme(plot.title = element_text(hjust = 0.5, size=28, family = "Times New Roman"), 
        axis.title.x = element_blank(), #gets rid of x-axis label
        axis.title.y = element_text(size = 24, family = "Times New Roman"),
        panel.grid.major = element_blank(), #gets rid of major gridlines
        panel.grid.minor = element_blank(), #gets rid of minor gridlines
        panel.background = element_blank(), #turns background white instead of gray
        axis.line = element_line(colour = "black"), 
        legend.position="none", #gets rid of legend
        axis.text=element_text(size=26, color = "black", family = "Times New Roman"), #sets size of x and y axis labels
        axis.text.x=element_text(family = "Times New Roman"),
        axis.title=element_text(size=26,face="bold", family = "Times New Roman"))

print(plot)
dev.off() 

# pSTAT/STAT

## Create a dot plot overlaid on a bar chart
# Initialize an individual high resolution graph
png("mFetLiv_pSTAT-STAT-TP_MDvPAE_DotPlot_B&W.png", units="in", width=4, height=7, res=600)

# Sets order of samples for future steps
order <- c("CON", "ALC")

plot <- ggplot(data= dataframe_Original, aes(x=Group, y=pSTAT3.STAT3.Average), fill=order) + #creates original graph
  geom_bar(stat="identity", color="black", aes(fill = Group),
           position=position_dodge()) +
  scale_fill_manual(values=c("CON" = "grey100", "ALC" = "grey51")) +
  geom_errorbar(aes(ymin=pSTAT3.STAT3.Lower, ymax=pSTAT3.STAT3.Upper), width=.2,
                position=position_dodge(2)) + 
  geom_point(aes(x=Group, y=pSTAT3.STAT3.TP),
             size = 2, shape = 19, position = position_jitter(width=.15, height=0)) + #adds individual points
  ggtitle("pSTAT3/STAT3") +
  scale_y_continuous(limits = c(0,8), expand = c(0,0)) + #this sets the minimum and maximum values shown on the y-axis
  ylab(expression("pSTAT3/STAT3/Total Protein, Relative to CON")) + #adds y-axis label
  scale_x_discrete (limits = order) + #rearranges the x-axis in correct order
  theme(plot.title = element_text(hjust = 0.5, size=24, family = "Times New Roman"), 
        axis.title.x = element_blank(), #gets rid of x-axis label
        axis.title.y = element_text(size = 20, family = "Times New Roman"),
        panel.grid.major = element_blank(), #gets rid of major gridlines
        panel.grid.minor = element_blank(), #gets rid of minor gridlines
        panel.background = element_blank(), #turns background white instead of gray
        axis.line = element_line(colour = "black"), 
        legend.position="none", #gets rid of legend
        axis.text=element_text(size=26, color = "black", family = "Times New Roman"), #sets size of x and y axis labels
        axis.text.x=element_text(family = "Times New Roman"),
        axis.title=element_text(size=26,face="bold", family = "Times New Roman"))

print(plot)
dev.off() 
