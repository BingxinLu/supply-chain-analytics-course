# Bingxin Lu
# Assignment 3

rm(list=ls())
setwd()

customerorders = read.table("Lokad_Orders.tsv", sep="\t", head=T)

attach(customerorders)
dim(customerorders)
head(customerorders)

PurchaseDate = as.Date(Date, "%d-%m-%y")
customerorders = cbind(customerorders, PurchaseDate)

#1: See the command below for creating unit cost data object and histogram.
UnitCost = NetAmount/Quantity
customerorders = cbind(customerorders, UnitCost)
hist(UnitCost,breaks = 100)

#2: See the command below for creating a dataset of all customer orders with unit costs below $100.
subdat = customerorders[UnitCost < 100,]
head(subdat)
dim(subdat)

#3: The R-square of the old model is 0.03291, the R-square of the model using subset created in #2 is 0.1095, so the fit of the model is improved.
lm_unit_cost_sub = lm(subdat$NetAmount ~ subdat$Quantity)
print(lm_unit_cost_sub)
summary(lm_unit_cost_sub)

#4: The model is not a appropriate model, because:
#   first, from the residuals vs fitted plot, we can see that there is a remaining pattern to residuals
#   second, from the normal Q-Q plot we can see that there are some outliers and the errors are not distributed normally.
#   third, from the scale-location plot, we can see that residuals increase gradually and they have relationship with the fitted values.
#   fourth, from the residuals vs leverage plot, we can see that some outliers have an undue influence on the model.
par(mfrow = c(2,2))
plot(lm_unit_cost_sub)

#5: See the command below for forcasting.
mo = strftime(PurchaseDate, "%m")
yr = strftime(PurchaseDate, "%Y")
date = paste(mo, "01", yr, sep="-")
date = as.Date(date, "%m-%d-%Y")
head(date)

agg_qty = aggregate(Quantity ~ date, FUN = sum)
head(agg_qty)
dim(agg_qty)

agg_NetAmt = aggregate(NetAmount ~ date, FUN = sum)
head(agg_NetAmt)
dim(agg_NetAmt)

customerorders_agg = cbind(agg_NetAmt, agg_qty)
library(forecast)

SES_NetAmt = ses(customerorders_agg$NetAmount, h=5, level=0.95)
summary(SES_NetAmt)
plot(SES_NetAmt)