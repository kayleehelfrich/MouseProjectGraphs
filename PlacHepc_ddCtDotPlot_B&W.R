#Change Cq File Biological Set Names to IS-MD, IS-PAE, etc. before beginning
#Average together any Cq values from same sample (leaving only biological replicates, no technical replicates)
#Copy and paste correct upper and lower bounds, and means to numbers (ex. all IS-0 get same upper, lower, and mean)
#Delete standard error for each sample
rm(list=ls())
library(ggplot2)
library(extrafont)
loadfonts(device = "win")
setwd("~/KayleeStuff/Smith_Lab/Data/Mouse_Validation_Data/Placenta_Hepcidin")

#reads a file in a table format and creates a data frame from it
datafile <- "PlacHepc_Ctcalcs_file1_For_ddCtPlot.csv"
data1 <- read.csv(datafile, header=TRUE) 
dataframe_Original <- data.frame(data1)

#write in the calibrator sample mean from the average of your Ct calculations for your calibrator sample
IS_MD_mean <-  3.56201900111111

#this function is the calculation for the new Ct's that have been compared to IS-0
Function1 <- function(Ct) {
  FUN <- ((2^-(Ct - IS_MD_mean)))
  return(FUN)
}

#for each sample, this subtracts the calibrator sample mean, and calculates the 2^-ddCt, and adjusts lower, upper, and mean
Adj_Ct <- sapply(dataframe_Original$Cq.Averages, match.fun("Function1"), USE.NAMES = TRUE)

#convert values from above into individual dataframes
dataframe_AdjCtCalcs <- data.frame(Adj_Ct)

#creates one final dataframe
dataframe_AdjCtCalcs$Biological.Sets<-dataframe_Original$Biological.Sets #adds the Biological.Sets labels back to the dataframe
dataframe_AdjCtCalcs$Sample.IDs<-dataframe_Original$Sample.IDs #adds the sample IDs back to the dataframe
dataframe_AdjCtCalcs$lower<-dataframe_Original$lower #adds in adjusted lower error
dataframe_AdjCtCalcs$upper<-dataframe_Original$upper #adds in adjusted upper error
dataframe_AdjCtCalcs$mean<-dataframe_Original$mean #adds in adjusted mean
dataframe_AdjCtCalcs <- dataframe_AdjCtCalcs[,c(2,3,1,4,5,6)] #rearranges the columns so Biological.Sets is first

#initialize a high resolution graph
png("Placental_Hepcidin_ddCtDotPlot_B&W.png", units="in", width=5, height=7, res=600)

sets <- c("CON", "ALC")

p<- ggplot(dataframe_AdjCtCalcs, aes(x=Biological.Sets, y=mean)) + 
  geom_bar(stat="identity", color="black", aes(fill = Biological.Sets),
           position=position_dodge()) +
  scale_fill_manual(values=c("CON" = "grey100", "ALC" = "grey51")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) + 
  geom_point(aes(x=Biological.Sets, y=Adj_Ct),
             size = 2, shape = 19, position = position_jitter(width=.15, height=0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,2)) +
  ggtitle("Placental Hepcidin") +
  ylab(expression("Hepcidin expression relative to CON")) + 
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
        axis.title=element_text(size=26,face="bold", family = "Times New Roman")) + 
  scale_x_discrete (limits = sets)

print(p)
dev.off()
