# Bingxin Lu
# Assignment 4

rm(list=ls())
setwd()

#1: Because the answer will change every time, see the result "std.error" from "Bootstrap Statistics".
data(mtcars)
dim(mtcars)
head(mtcars)
attach(mtcars)

library(boot)

medianfunc = function(x,i){median(x[i])}
bootMedian = boot(mpg,medianfunc,100)
bootMedian

#2: See the command below.
rsq = function(data,i){
  d = data[i,]
  model = lm(d$disp~d$hp)
 return(summary(model)$r.squared)  
}
Rsboot = boot(mtcars,rsq,1000)
# Computing the average R Squared of the model.
mean(Rsboot$t)
# Creating a histogram of the different R-Squared values.
hist(Rsboot$t)
# Report the 95% Confidence Interval boundaries of R-Squared for this model.
quantile(Rsboot$t, probs = c(0.0275, 0.975))

#3a: Because the answer will change every time, see the command below for calculating average % of assembly orders that will have an issue. 
nreps = 10000
mean_comp = 200
sd_comp   = 30
instock = .95
stockouts = rep(NA, nreps)
notinstock = rep(NA, nreps)

for (rep in 1:nreps) {
  components = rnorm(1, mean_comp, sd_comp)
  components = round(components)
  stockouts[rep] = dbinom(components, components, instock)
  notinstock[rep] = 1-stockouts[rep]
}

mean(notinstock)

#3b: See the command below for calculate the interquartile range.
IQR(notinstock)

#3c: From the results we can see that, 
# the average % of assembly orders that will have an issue drops from about 0.999 to about 0.998,
# and the interquartile range increases from about 8e-05 to about 0.0011,
# so, a decrease in average number of components will decrease the average % of assembly orders that will have an issue and expand the IQR.

nreps = 10000
mean_comp_new = 150
sd_comp   = 30
instock = .95
stockouts_new = rep(NA, nreps)
notinstock_new = rep(NA, nreps)

for (rep in 1:nreps) {
  components = rnorm(1, mean_comp_new, sd_comp)
  components = round(components)
  stockouts_new[rep] = dbinom(components, components, instock)
  notinstock_new[rep] = 1-stockouts_new[rep]
}

mean(notinstock_new)
IQR(notinstock_new)

#3d: From the results we can see that, 
# the average % of assembly orders that will have an issue increases from about 0.9998 to about 0.9999,
# and the interquartile range decreases from about 8e-05 to about 5e-05,
# so, a decrease in average number of components will increase the average % of assembly orders that will have an issue and narrow the IQR.

nreps = 10000
mean_comp = 200
sd_comp_new   = 20
instock = .95
stockouts_new1 = rep(NA, nreps)
notinstock_new1 = rep(NA, nreps)

for (rep in 1:nreps) {
  components = rnorm(1, mean_comp, sd_comp_new)
  components = round(components)
  stockouts_new1[rep] = dbinom(components, components, instock)
  notinstock_new1[rep] = 1-stockouts_new1[rep]
}

mean(notinstock_new1)
IQR(notinstock_new1)