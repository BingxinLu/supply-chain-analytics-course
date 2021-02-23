# Bingxin Lu
# Assignment 2

#2: See the command below for resetting the workspace.
rm(list=ls())

#3: See the command below for setting the working directory.
setwd()

#4: See the command below for reading the data.
purchaseorders = read.table("Lokad_PurchaseOrders.tsv", sep="\t", head=T)

#Preparing data
attach(purchaseorders)
dim(purchaseorders)

DelDate = as.Date(DeliveryDate, "%d-%m-%y")
PODate = as.Date(Date, "%d-%m-%y")

purchaseorders = cbind(purchaseorders, DelDate, PODate)

open_PO = purchaseorders[Received < Quantity,]
open_PO = ifelse(Received < Quantity, "Open PO", "Closed PO")
purchaseorders = cbind(purchaseorders, open_PO)

NetDollars = 	ifelse(Currency == "GBP", NetAmount * 1.30, 
                     ifelse(Currency == "EUR", NetAmount * 1.11,
                            NetAmount))

purchaseorders = cbind(purchaseorders, NetDollars)

PartialDeliveries = ifelse(Received > 0 & Received < Quantity, "Partial Delivery", "Not Partial")
purchaseorders = cbind(purchaseorders, PartialDeliveries)
head(purchaseorders)
dim(purchaseorders)

#5: See the command below for creating logical vector for almost filled open orders.
AlmostFilled = ifelse(Received >= Quantity * 0.9 & Received != Quantity, "Yes",
                      ifelse(PartialDeliveries == "Not Partial", "Not Partial","No"))
                      
purchaseorders = cbind(purchaseorders, AlmostFilled)
head(purchaseorders)
tail(purchaseorders)

#6a: See the command below for ploting, with green denoting partial delivered and red denoting not partial delivered.
subdat = purchaseorders[open_PO == "Open PO",]
dim(subdat)

plot(subdat$PODate,subdat$NetDollars, xlab = "PODate of open PO", ylab = "NetDollars of open PO",
     col = ifelse(subdat$PartialDeliveries == "Partial Delivery","green","red"))

#6b: See the command below for creating a panel of three locations, with green denoting partial delivered and red denoting not partial delivered.
par(mfrow = c(3,1))
with(subdat[subdat$Loc == "Chicago",],plot(PODate,NetDollars, main = "Chicago", xlab = "PODate of open PO",ylab = "NetDollars of open PO",
     col = ifelse(PartialDeliveries == "Partial Delivery","green","red")))
with(subdat[subdat$Loc == "Los Angeles",],plot(PODate,NetDollars, main= "Los Angeles", xlab = "PODate of open PO",ylab = "NetDollars of open PO",
     col = ifelse(PartialDeliveries == "Partial Delivery","green","red")))
with(subdat[subdat$Loc == "New-York",],plot(PODate,NetDollars,main = "New-York",xlab = "PODate of open PO", ylab = "NetDollars of open PO",
     col = ifelse(PartialDeliveries == "Partial Delivery","green","red")))

par(mfrow = c(1,1))

#6c: Logipro has the most partially delivered in terms of dollars.
YetReceived = ifelse(PartialDeliveries == "Partial Delivery", NetDollars/Quantity*(Quantity-Received), 0)
purchaseorders = cbind(purchaseorders,YetReceived)
tapply(YetReceived, Supplier, sum)

#6d: There are no qualitative difference in the unit costs to the different locations.
UnitCost = NetDollars/Quantity
hist(UnitCost)
purchaseorders = cbind(purchaseorders, UnitCost)

par(mfrow = c(3,1))
with(purchaseorders[purchaseorders$Loc == "Chicago",], hist(UnitCost,main = "Chicago"))
with(purchaseorders[purchaseorders$Loc == "Los Angeles",], hist(UnitCost,main = "Los Angeles"))
with(purchaseorders[purchaseorders$Loc == "New-York",], hist(UnitCost,main = "New-York"))

#7a: Because the answer will change every time, see the command below for calculating how many replications are required.
count_rep = 0
while (rnorm(1,0,1) < 3){
  count_rep = count_rep+1
}
print(count_rep) 

#7b: Because the answer will change every time, see the command below for calculating mean and standard deviation.
weeklydemand = matrix(0,nrow = 1000, ncol = 4)
A = rnorm(1000,30,5)
B = rnorm(1000,20,8)
C = rnorm(1000,40,12)

for (i in 1:1000){
  weeklydemand[i,1] = A[i]
  weeklydemand[i,2] = B[i]
  weeklydemand[i,3] = C[i]
  weeklydemand[i,4] = weeklydemand[i,1]+weeklydemand[i,2]+weeklydemand[i,3]
}
sum = weeklydemand[,4]
mean(sum)
sd(sum)
  