library("dplyr")
library(ggplot2)

setwd("C:\\Users\\shaun\\OneDrive\\Desktop\\Academics\\Sem-5\\Foundation of Data Analytics 3505\\J-comp\\Datasets\\")
# Accidents due to vehicular defects
vehicular_defect = read.csv("Vehicular_defects.csv")
vehicular_defect = vehicular_defect[1:36,2:32]
vehicular_defect = vehicular_defect %>% arrange(State..UT)
vehicular_defect = vehicular_defect[-c(9,6,10,8),]
View(vehicular_defect)
sum(is.na(vehicular_defect))
dim(vehicular_defect)

colnames(vehicular_defect)

rownames(vehicular_defect) <- vehicular_defect$State..UT

states = seq(1:32)
vehicular_defect$Total_2014 = 0
vehicular_defect$Total_2016 = 0
total_nums1 = seq(2,16,3)
total_nums2 = seq(17,32,3)

for (i in states)
{
  vehicular_defect$Total_2014[i] = sum(vehicular_defect[i,total_nums1])
  vehicular_defect$Total_2016[i] = sum(vehicular_defect[i,total_nums2])
}

par(mar = c(9,4,4,4), bg="#CDCDCD")
barplot(vehicular_defect$Total_2014, main = "Total accidents in 2014 due to Vehicular defects" ,ylab="accidents",names.arg = vehicular_defect$State..UT,las=2,col = rainbow(150),ylim = c(0,25000))

barplot(vehicular_defect$Total_2016, main = "Total accidents in 2016 due to Vehicular defects" ,ylab="accidents",names.arg = vehicular_defect$State..UT,las=2,col = rainbow(80),ylim = c(0,45000))

labs = colnames(vehicular_defect)[total_nums1]
for (i in seq(3:34))
{
  par(mar = c(0,2,2,2), bg="#CDCDCD")
  v=as.numeric(as.vector(vehicular_defect[i,total_nums1]))
  if(sum(v)!=0)
  {
      piepercent<- round(100*v/sum(v), 1)
      pie(v,radius = 0.65,labels = piepercent, main=vehicular_defect[i,1],col = rainbow(6))
      legend("bottom", labs, cex = 0.7, fill = rainbow(6))
  }
}