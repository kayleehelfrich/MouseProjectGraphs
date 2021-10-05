#Change Cq File Biological Set Names to IS-MD, IS-PAE, etc. before beginning
#Average together any Cq values from same sample (leaving only biological replicates, no technical replicates)
#Copy and paste correct upper and lower bounds, and means to numbers (ex. all IS-0 get same upper, lower, and mean)
#Delete standard error for each sample
rm(list=ls())
library(ggplot2)
setwd("C:/Users/kaylee/Documents/Kaylee_Stuff/Smith_Lab/Data/Mouse_Validation_Data/Maternal_Hepcidin_4.5gDose")

#reads a file in a table format and creates a data frame from it
datafile <- "MatLivHEPC_4.5gkgDose_Ctcalcs_file1_For_ddCtDotPlot.csv"
data1 <- read.csv(datafile, header=TRUE) 
dataframe_Original <- data.frame(data1)

#write in the calibrator sample mean from the average of your Ct calculations for your calibrator sample
IS_MD_mean <-  1.7885419275

#this function is the calculation for the new Ct's that have been compared to IS-0
Function1 <- function(Ct) {
  FUN <- ((2^-(Ct - IS_MD_mean))*100)
  return(FUN)
}
#this function adjusts the lower, upper, and mean values
Function2 <- function(value) {
  FUN <- (value*100)
  return(FUN)
}
#for each sample, this subtracts the calibrator sample mean, and calculates the 2^-ddCt, and adjusts lower, upper, and mean
Adj_Ct <- sapply(dataframe_Original$Cq.Averages, match.fun("Function1"), USE.NAMES = TRUE)
Adj_lower <- sapply(dataframe_Original$lower, match.fun("Function2"), USE.NAMES = TRUE)
Adj_upper <- sapply(dataframe_Original$upper, match.fun("Function2"), USE.NAMES = TRUE)
Adj_mean <- sapply(dataframe_Original$mean, match.fun("Function2"), USE.NAMES = TRUE)

#convert values from above into individual dataframes
dataframe_AdjCtCalcs <- data.frame(Adj_Ct)
dataframe_AdjLower <- data.frame(Adj_lower)
dataframe_AdjUpper <- data.frame(Adj_upper)
dataframe_AdjMean <- data.frame(Adj_mean)

#creates one final dataframe
dataframe_AdjCtCalcs$Biological.Sets<-dataframe_Original$Biological.Sets #adds the Biological.Sets labels back to the dataframe
dataframe_AdjCtCalcs$Sample.IDs<-dataframe_Original$Sample.IDs #adds the sample IDs back to the dataframe
dataframe_AdjCtCalcs$lower<-dataframe_AdjLower$Adj_lower #adds in adjusted lower error
dataframe_AdjCtCalcs$upper<-dataframe_AdjUpper$Adj_upper #adds in adjusted upper error
dataframe_AdjCtCalcs$mean<-dataframe_AdjMean$Adj_mean #adds in adjusted mean
dataframe_AdjCtCalcs <- dataframe_AdjCtCalcs[,c(2,3,1,4,5,6)] #rearranges the columns so Biological.Sets is first

#initialize a high resolution graph
png("MatLiv_HEPC_ddCtDotPlot_4.5gkgDose.png", units="in", width=7, height=7, res=1000)

#sets order of samples for future steps
order <- c("MD", "EtOH")
cols <- c("MD" = "navyblue", "EtOH" = "forestgreen")

p <- ggplot(data= dataframe_AdjCtCalcs, aes(x=Biological.Sets, y=Adj_Ct, color = Biological.Sets), fill=order) + #creates original graph
  geom_point(size = 2, shape = 19, position = position_jitter(width=.15, height=0.1)) + #adds individual points
  ggtitle("Maternal Liver Hepcidin") + #adds title
  xlab("Treatment") + #adds x-axis label
  ylab("Maternal Liver Hepc/Gapdh expression relative to MD") + #adds y-axis label
  geom_errorbar(aes(ymin = lower, ymax = upper), col = "black", width =  .1) + #adjusts errorbars
  geom_point(aes(x = Biological.Sets, y = mean), size = 10, shape = 95, col = "red") + #adjusts mean line
  scale_x_discrete (limits = order) + #rearranges the x-axis in correct order
  scale_color_manual(values = cols) + #supposedly alters the colors of the points
  scale_y_continuous(expand = c(0,0), limits = c(0,400)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18), #sets size and position of graph title
    axis.title.x = element_text(size = 16), #axis.title.x = element_blank() if you want to get rid of x-axis label
    panel.grid.major = element_blank(), #gets rid of major gridlines
    panel.grid.minor = element_blank(), #gets rid of minor gridlines
    panel.background = element_blank(), #turns background white instead of gray
    axis.line = element_line(colour = "black"), #turns axes to black
    legend.position="none", #gets rid of legend
    axis.text=element_text(size=18, color = "black"), #sets size of x and y axis labels
    axis.title = element_text(size=18, color = "black")
                                    )
print(p)

#release PNG file graph into directory
dev.off() 
