rm(list=ls())
library(ggplot2)
library(extrafont)
loadfonts(device = "win")
setwd("~/KayleeStuff/Smith_Lab/Data/Mouse_Validation_Data/Fetal_pSMAD_SMAD")


### Reads a file in a table format and creates a data frame from it. This figures will be in the first panel.
datafile <- "FetLiv_pSMAD_SMAD_TP_FinalValues.csv"
data1 <- read.csv(datafile, header=TRUE) 
dataframe_Original <- data.frame(data1)

#SMAD

## Create a dot plot overlaid on a bar chart
# Initialize an individual high resolution graph
png("mFetLiv_SMAD-TP_MDvPAE_DotPlot_B&W.png", units="in", width=4, height=7, res=600)

# Sets order of samples for future steps
order <- c("CON", "ALC")

plot <- ggplot(data= dataframe_Original, aes(x=Group, y=SMAD.Average), fill=order) + #creates original graph
  geom_bar(stat="identity", color="black", aes(fill = Group),
           position=position_dodge()) +
  scale_fill_manual(values=c("CON" = "grey100", "ALC" = "grey51")) +
  geom_errorbar(aes(ymin=SMAD.Lower, ymax=SMAD.Upper), width=.2,
                position=position_dodge(2)) + 
  geom_point(aes(x=Group, y=SMAD.TP),
             size = 2, shape = 19, position = position_jitter(width=.15, height=0)) + #adds individual points
  ggtitle("SMAD5") +
  scale_y_continuous(limits = c(0,2), expand = c(0,0)) + #this sets the minimum and maximum values shown on the y-axis
  ylab(expression("SMAD5/Total Protein Relative to CON")) + #adds y-axis label
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

# pSMAD

## Create a dot plot overlaid on a bar chart
# Initialize an individual high resolution graph
png("mFetLiv_pSMAD-TP_MDvPAE_DotPlot_B&W.png", units="in", width=4, height=7, res=600)

# Sets order of samples for future steps
order <- c("CON", "ALC")

plot <- ggplot(data= dataframe_Original, aes(x=Group, y=pSMAD.Average), fill=order) + #creates original graph
  geom_bar(stat="identity", color="black", aes(fill = Group),
           position=position_dodge()) +
  scale_fill_manual(values=c("CON" = "grey100", "ALC" = "grey51")) +
  geom_errorbar(aes(ymin=pSMAD.Lower, ymax=pSMAD.Upper), width=.2,
                position=position_dodge(2)) + 
  geom_point(aes(x=Group, y=pSMAD.TP),
             size = 2, shape = 19, position = position_jitter(width=.15, height=0)) + #adds individual points
  ggtitle("pSMAD1/5") +
  scale_y_continuous(limits = c(0,2), expand = c(0,0)) + #this sets the minimum and maximum values shown on the y-axis
  ylab(expression("pSMAD1/5/Total Protein Relative to CON")) + #adds y-axis label
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

# pSMAD/SMAD

## Create a dot plot overlaid on a bar chart
# Initialize an individual high resolution graph
png("mFetLiv_pSMAD-SMAD-TP_MDvPAE_DotPlot_B&W.png", units="in", width=4, height=7, res=600)

# Sets order of samples for future steps
order <- c("CON", "ALC")

plot <- ggplot(data= dataframe_Original, aes(x=Group, y=pSMAD.SMAD.Average), fill=order) + #creates original graph
  geom_bar(stat="identity", color="black", aes(fill = Group),
           position=position_dodge()) +
  scale_fill_manual(values=c("CON" = "grey100", "ALC" = "grey51")) +
  geom_errorbar(aes(ymin=pSMAD.SMAD.Lower, ymax=pSMAD.SMAD.Upper), width=.2,
                position=position_dodge(2)) + 
  geom_point(aes(x=Group, y=pSMAD.SMAD.TP),
             size = 2, shape = 19, position = position_jitter(width=.15, height=0)) + #adds individual points
  ggtitle("pSMAD1/5/SMAD5") +
  scale_y_continuous(limits = c(0,3), expand = c(0,0)) + #this sets the minimum and maximum values shown on the y-axis
  ylab(expression("pSMAD1/5/SMAD5/Total Protein, Relative to CON")) + #adds y-axis label
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
