rm(list=ls())
setwd("C:/Users/kaylee/Documents/Kaylee_Stuff/Smith_Lab/Data/Fetal_Brain_Mineral_Analysis")
Data_file <- "Fetal_Mouse_Brain_Data_DryWeight.csv"

data <- read.csv(Data_file, header=TRUE) 
data_frame <- data.frame(data)

#separate out data into MD and EtOH to view in separate tables
MD <- data_frame[is.element(data_frame$Treatment, "MD"),]
EtOH <- data_frame[is.element(data_frame$Treatment, "EtOH"),]

#Create a table of means of MD data
MDAvg <- colMeans(MD[,c(3:12)]) 
MD_table <- data.frame(MDAvg)

#Create a table of means of EtOH data
EtOHAvg <- colMeans(EtOH[,c(3:12)])
EtOH_table <- data.frame(EtOHAvg)

#Get standard error for Fe groups
sd(MD$Fe)
sd(EtOH$Fe)

#Calculating p-values and storing them to a table
pvalues <- c()
t.test(MD$Ca, EtOH$Ca, alternative = "two.sided")
pvalues <- c(pvalues, t.test(MD$Ca, EtOH$Ca)$'p.value')
t.test(MD$Cu, EtOH$Cu, alternative = "two.sided")
pvalues <- c(pvalues, t.test(MD$Cu, EtOH$Cu)$'p.value')
t.test(MD$Fe, EtOH$Fe, alternative = "two.sided")
pvalues <- c(pvalues, t.test(MD$Fe, EtOH$Fe)$'p.value')
t.test(MD$K, EtOH$K, alternative = "two.sided")
pvalues <- c(pvalues, t.test(MD$K, EtOH$K)$'p.value')
t.test(MD$Mg, EtOH$Mg, alternative = "two.sided")
pvalues <- c(pvalues, t.test(MD$Mg, EtOH$Mg)$'p.value')
t.test(MD$Na, EtOH$Na, alternative = "two.sided")
pvalues <- c(pvalues, t.test(MD$Na, EtOH$Na)$'p.value')
t.test(MD$Ni, EtOH$Ni, alternative = "two.sided")
pvalues <- c(pvalues, t.test(MD$Ni, EtOH$Ni)$'p.value')
t.test(MD$P, EtOH$P, alternative = "two.sided")
pvalues <- c(pvalues, t.test(MD$P, EtOH$P)$'p.value')
t.test(MD$Si, EtOH$Si, alternative = "two.sided")
pvalues <- c(pvalues, t.test(MD$Si, EtOH$Si)$'p.value')
t.test(MD$Zn, EtOH$Zn, alternative = "two.sided")
pvalues <- c(pvalues, t.test(MD$Zn, EtOH$Zn)$'p.value')

p_values <- data.frame(pvalues)
row.names(p_values) <- c("Ca", "Cu", "Fe", "K", "Mg", "Na", "Ni", "P", "Si", "Zn")

#Adjust p-values
p.adjust(pvalues, method="BH")
