# Bingxin Lu
# Assignment 1

# 2: See the command below for resetting the workspace.
rm(list=ls())

#3: See the command below for setting the working directory.
setwd()

#4: See the command below for reading the data.
realestate <- read.csv("datagovbldgrexus.csv",header = T)

#5: See the command below for providing summary.
summary(realestate)

#6: There are 9362 rows and 18 columns.
dim(realestate)

#7: See the command below for attaching.
attach(realestate)

#8: There are 11 unique region codes.
length(unique(Region.Code))

#9: See the command below for plotting histogram.
hist(Total.Parking.Spaces,xlab = "Number of Spots",ylab = "Frequency",main = "Parking Space Histogram")

#10: See the command below for computing how many buildings per state.
tapply(Property.Type,Bldg.State,FUN = function(x) length(x))

#11: See the command below for computing the average square footage per state.
tapply(Bldg.ANSI.Usable,Bldg.State, mean)

#12: There is no a correlation between Bldg ANSI Usable and Total Parking Spaces.
plot(Bldg.ANSI.Usable,Total.Parking.Spaces,col="blue")