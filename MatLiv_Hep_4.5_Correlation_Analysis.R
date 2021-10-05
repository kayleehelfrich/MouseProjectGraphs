rm(list=ls())
library(ggplot2)
library(ggpubr)
setwd("C:/Users/kaylee/Documents/Kaylee_Stuff/Smith_Lab/Data/Mouse_Validation_Data/Maternal_Hepcidin_4.5gDose")
Data_file <- "CollectionTimes_HepcIL6Expression_Mouse_GD13.5-17.5_4.5g_MD.csv"

#reads a file in a table format and creates a data frame from it
data <- read.csv(Data_file, header=TRUE) 
data_frame <- data.frame(data)

#initialize a high resolution graph
png("MatLiv_IL6_HEPC_Correlation_4.5gDose_ddCtValue_MD.png", units="in", width=7, height=7, res=600) 

#Scatter plot with correlation analyses
sp <- ggscatter(data_frame, x = "IL6.Cq.Averages.Adj", y = "HEPC.Cq.Averages.Adj", #creates graph
                color = "Biological.Sets", 
                size = 3, #sets shape based on pre-set variables
                palette = c("black", "gray"), #sets color of points
                add = "reg.line", #adds a regression line
                add.params = list(color = "red"), #adds color of regression line
                title = "Correlation of Maternal Liver IL-6 and Hepcidin in MD Only",  #sets main graph title
                xlab = "ddCt of IL-6", ylab = "ddCt of Hepcidin", #sets x-axis and y-axis labels
                legend = "right", 
                ellipse = FALSE) + #when true, adds an ellipse around the points
    stat_cor(method = "pearson", #uses Pearson correlation to analyze correlation
              label.x.npc = 0.45, label.y.npc = 0.85, size = 6) #moves correlation numbers and label to desired location and size
print(sp)

#release PNG file graph into directory
dev.off() 

#adjust p values to account for multiple hypothesis testing
pvals <- c(0.19, 0.43, 0.54, 0.35)
p.adjust(pvals, method="BH")