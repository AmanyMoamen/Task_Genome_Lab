# Set R working Directory
getwd()
setwd("C:/Users/HP/Desktop/Genomics Data Analysis Lab Allometry Dataset")
# Read Dataset
Allometry <- read.csv("G1_Allometry.csv")
# View Dataset on R environment
View(Allometry)
# Get Data Dimensions
dim(Allometry)
# Deal with CSV file for analysis
Allometry2 <- read.csv("G1_Allometry.csv",header = F)
Allometry2
View(Allometry2)
Allometry3 <- read.csv("G1_Allometry.csv",col.names = c("Column1","Column2","Column3",
                                                        "Column4","Column5"))
Allometry3
View(Allometry3)
# Dataset Sense
nrow(Allometry)
ncol(Allometry)
colnames(Allometry)
# Data Indexing & Slicing
head(Allometry)
tail(Allometry)
head(Allometry,17)
tail(Allometry,17)
Allometry4 <- head(Allometry[,-c(1,3)])
Allometry4
View(Allometry4)
Per.type <- Allometry[2,4]
Per.type
Per.type <- Allometry[3,"leafarea"]
Per.type
Rowinfo <- Allometry[2,]
Rowinfo
Colinfo <- Allometry[,4]
Colinfo
Colinfo <- head(Allometry[,1])
Colinfo
Colinfo <- head(Allometry[,"height"])
Colinfo
Colinfo <- Allometry["height"]
Colinfo
Colinfo <- head(Allometry["height"])
Colinfo
Setinfo <- Allometry[c(1:10),c("species","diameter","height")]
Setinfo
Allometry5 <- Allometry[Allometry$species=="PSME",]
Allometry5
Allometry6 <- Allometry[Allometry$species=="PIPO",]
Allometry6
Allometry7 <- Allometry[Allometry$species=="PIMO",]
Allometry7
Allometry8 <- Allometry[Allometry$diameter<60,c(1,2,3)]
Allometry8
Allometry9 <- Allometry[Allometry$species=="PIPO"& Allometry$diameter<60,c(1,2,3)]
Allometry9
Allometry10 <- Allometry[Allometry$species=="PIPO"& Allometry$diameter<60,-c(1,2,3)]
Allometry10
# Data Sorting
Allometry11 <- Allometry[order(Allometry$diameter),]
Allometry11
Allometry12 <- Allometry[order(-Allometry$diameter),]
Allometry12
Allometry13 <- Allometry[order(Allometry$diameter,Allometry$leafarea),]
Allometry13
# Data Re-coding Columns
Allometry$newspecies[Allometry$species=="PSME"]='1'
Allometry$newspecies[Allometry$species=="PIPO"]='2'
Allometry$newspecies[Allometry$species=="PIMO"]='3'
Allometry
View(Allometry)
# Data Computing
Allometry$computing <- Allometry$leafarea - Allometry$branchmass
Allometry
# Delete a column
Allometry$computing <- NULL
Allometry
# To get all locations of NA
complete.cases(Allometry)
# Get all rows contain missing data 
Allometry[!complete.cases(Allometry),]
# The data structure to know the incorrect types for variables.
str(Allometry)
# The data summary to know the incorrect types for variables
summary(Allometry)
# Cleaning Data
Allometry$diameter <- gsub("\\.","",Allometry$diameter)
View(Allometry)
Allometry$diameter <- as.numeric(Allometry$diameter)
Allometry$height <- gsub(",","",Allometry$height)
View(Allometry)
Allometry$height <- gsub("\\â???o","",Allometry$height)
View(Allometry)
Allometry$height <- gsub("\\â???","",Allometry$height)
View(Allometry)
Allometry$height <- gsub("\\.","",Allometry$height)
View(Allometry)
Allometry$height <- as.numeric(Allometry$height)
Allometry$leafarea <- gsub("\\.","",Allometry$leafarea)
View(Allometry)
Allometry$leafarea <- as.numeric(Allometry$leafarea)
Allometry$branchmass <- gsub("\\.","",Allometry$branchmass)
View(Allometry)
Allometry$branchmass <- as.numeric(Allometry$branchmass)
Allometry$newspecies <- as.numeric(Allometry$newspecies)
Allometry$computing <- Allometry$height + Allometry$leafarea
Allometry
summary(Allometry)
# Data Manipulation
#  Get all row with missing data for specific variable
is.na(Allometry)
any(is.na(Allometry$height))
Allometry14 <- Allometry[is.na(Allometry$height),]
Allometry14
Allometry15 <- Allometry[!is.na(Allometry$height),]
Allometry15
# Get and fill rows of a variable based on NA in another variable
Allometry16 <- Allometry[is.na(Allometry$height) & Allometry$species=="PSME",]
Allometry16
Allometry[is.na(Allometry$height) & Allometry$species=="PSME",'height'] = "4689724578"
Allometry
Allometry[is.na(Allometry$height) & Allometry$species=="PIMO",'height'] = "7645789075"
Allometry
median(Allometry[,'leafarea'])
median(Allometry[,'height'],na.rm = T)
med <- median(Allometry[Allometry$species=="PIPO",'height'],na.rm = T)
Allometry[is.na(Allometry$height) & Allometry$species=="PIPO",'height'] <- med
Allometry
Allometry[is.na(Allometry$computing) & Allometry$species=="PSME",'computing'] = "3457899755"
Allometry
Allometry[is.na(Allometry$computing) & Allometry$species=="PIMO",'computing'] = "4465908657"
Allometry
med2 <- median(Allometry[Allometry$species=="PIPO",'computing'],na.rm = T)
Allometry[is.na(Allometry$computing) & Allometry$species=="PIPO",'computing'] <- med2
Allometry
# Data is ready for visualization
install.packages("tidyverse")
library(ggplot2)
# Display the effect of the height on diameter(co_relation) using scatter plot,name the figure
draw1 <- ggplot(Allometry,aes(x=height,y=diameter))
draw1 + geom_point() + ggtitle("The co_relation between the height and diameter")
# Display the effect of height on diameter colored by the groups of newspecies using scatter plot
draw2<-ggplot(Allometry , aes(height , diameter))
draw2 + geom_point(aes(color=newspecies)) +stat_smooth(se=FALSE)  
# The distribution of leafarea using histogram,name the figure and rename the x,y
draw3<-ggplot(Allometry , aes(leafarea))
draw3
draw3 + geom_histogram(binwidth = 7)
draw3 + geom_histogram(fill = "green")
draw3 + geom_histogram() + ggtitle("leafarea distribution")
draw3 + geom_histogram() + labs(x="leafarea" , y="Frequency")
# The distribution of branchmass using histogram,name the figure and rename the x,y
draw4<-ggplot(Allometry , aes(branchmass))
draw4
draw4 + geom_histogram(binwidth = 7)
draw4 + geom_histogram(fill = "green")
draw4 + geom_histogram() + ggtitle("branchmass distribution")
draw4 + geom_histogram() + labs(x="branchmass" , y="Frequency")
# Summarize the newspecies 1,2,3 to species and newspecies groups using Bar chart
draw5<-ggplot(Allometry , aes(x=newspecies  ,fill= species))
draw5 +geom_bar()+labs(y="species count" ,title="species rate")
draw5 +geom_bar() +theme_light()+facet_wrap(~newspecies)