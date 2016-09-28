---
title: "Applied_Survival_Analysis_Exercises"
author: "Brigitte"
date: "September 12, 2016"
output: html_document
---
<!-- rmarkdown v1 -->
# Applied Survival Analysis Using R, D.R. Moore
## Exercises in book and exercise section



## Exercise 1

```r
library(asaur)
```

```
## Warning: package 'asaur' was built under R version 3.2.4
```

```r
head(gastricXelox)
```

```
##   timeWeeks delta
## 1         4     1
## 2         8     1
## 3         8     1
## 4         8     1
## 5         9     1
## 6        11     1
```

```r
table(gastricXelox$delta)
```

```
## 
##  0  1 
## 16 32
```

```r
death = length(which(gastricXelox$delta==1))
pwfollow = sum(gastricXelox$timeWeeks)
eventratepp = death/pwfollow
```

##Chapter 2
![plot of chunk }#](figure/}#-1.png)

## Modeling survival data
### Weibull distribution
h(t) = alpha*lambda*(lambda*t)^(alpha-1)
and cumulative  
H(t) = (lambda*t)^alpha  
Function pweibull calculates the pdf of the Weibull distribution.



```r
curve(pweibull(x,shape=1.5,scale=1/0.03,
               lower.tail = F),from=0,to=80,ylim=c(0,1))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

Nowe we want to plot the hazard function with this shape and scale. Define the function first as it is dweibull/pweibull and will be easier to read if defined in separate command.


```r
weibHaz = function(x,shape,scale) {
  dweibull(x,shape=shape,scale=scale)/pweibull(x,shape=shape,scale=scale,
                                               lower.tail=F)
}
curve(weibHaz(x,shape=1.5,scale=1/0.03),from=0,to=80,
      ylab ='Hazard', xlab ='Time' , col = 'red', main = 'Weibull Hazard dweibull / pweibull, i.e. PDF/CDF')
curve(weibHaz(x,shape=0.75,scale=1/0.03),from=0,to=80,
      ylab ='Hazard', xlab ='Time' , col = 'blue',add = T)

legend('top',c('alpha= 1.5, gam=0.03 ','al=0.75, gam=0.03 '),
       col = c('red','blue'),lty=c(1,1))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

Instead of dweibull and pweibull, you could also use the gamma hazard function with dgamma dna pgamma, or the log-normal, log-logistic or Pareto distribution, for example. 

### Calculating the Survival function from the Hazard function

Numerical solution: For example the very first plot above needs numerical solution. First compute the vector of differences, then find the cumulative hazard function using cumsum.


```r
tmnew = c(0,1/365,7/365,28/365,1:110)
tm.diff = diff(tmnew)
survMale = exp(-cumsum(hazMale*tm.diff)*365.24)
survFemale = exp(-cumsum(hazFemale*tm.diff)*365.24)
sum(survMale*tm.diff)
```

```
## [1] 74.72665
```

```r
sum(survFemale*tm.diff)
```

```
## [1] 79.82184
```


## Exercises page 24


```r
plot(log(survexp.us[,"female","2000"]) ~ tm,type="n",xlab="age",ylab="log hazard")
lines(log(survexp.us[,"male","1940"]) ~ tm)
lines(log(survexp.us[,"female","1940"]) ~ tm,col='red')
lines(log(survexp.us[,"male","2000"]) ~ tm,lty=2)
lines(log(survexp.us[,"female","2000"]) ~ tm,col='red',lty=2)
legend('top',c('male 1940','female 1940','male 2000','female 2000'),col = c('black','red','black','red'),lty=c(1,1,2,2)) # gives the legend  symbols
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
2.1 Hazard rates were much lower in 2000 for children.


```r
ages = list()
survMale = exp(-cumsum(survexp.us[,"male","1940"]*tm.diff)*365.24)
survFemale = exp(-cumsum(survexp.us[,"female","1940"]*tm.diff)*365.24)
ages$male40 = sum(survMale*tm.diff) # 1940 male
ages$female40 = sum(survFemale*tm.diff) #§940 female
survMale = exp(-cumsum(survexp.us[,"male","2000"]*tm.diff)*365.24)
survFemale = exp(-cumsum(survexp.us[,"female","2000"]*tm.diff)*365.24)
ages$male2000 = sum(survMale*tm.diff) # 2000 male
ages$female2000 = sum(survFemale*tm.diff) #2000 female

row.names <- names(ages)
col.names <- c("Mean Age")
cbind(as.data.frame(matrix(c(ages),nrow = 4, ncol = 1,
                           dimnames = list(row.names, col.names))))
```

```
##            Mean Age
## male40     61.12031
## female40    65.4049
## male2000   73.68863
## female2000 78.91715
```
2.2. The mean age is printed above. It is calculated from the cumulative hazard function, then the are under the respective survival curves by multilying tm.diff, the width of each rectange, with the heights of the rectangles survMale etc.



```r
plot(log(survexp.usr[,"male","white","2000"]) ~ tm,type="n",xlab="age",ylab="log hazard")
lines(log(survexp.usr[,"male","white","1940"]) ~ tm)
lines(log(survexp.usr[,"male","black","1940"]) ~ tm,col='red')
lines(log(survexp.usr[,"male","white","2000"]) ~ tm,lty=2)
lines(log(survexp.usr[,"male","black","2000"]) ~ tm,col='red',lty=2)
legend('top',c('white 1940','black 1940','white 2000','black 2000'),col = c('black','red','black','red'),lty=c(1,1,2,2)) # gives the legend  symbols
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

2.3. survexp.usr data. Plotted males for 1940 and 2000.


2.4. In exercise 1.1, it looks like 3 death occured, i.e. d=3. The sum of the survival times is 5+5+4+3+1, i.e. V = 18. 
l(lambda) = 3 log(lambda) - lambda * 18
and lambda is 0.166

```r
3/18 # lambda = d/V
```

```
## [1] 0.1666667
```

```r
3/(18^2) #var(lambda)
```

```
## [1] 0.009259259
```

For question 2.5, I had difficulties plotting the hazard function as a stepwise linear function. I found package eha which helps.

Name  | usage
------  | ------
Density  |  dpch(x, cuts, levels, log = FALSE)
Distribution  |  ppch(q, cuts, levels, lower.tail = TRUE, log.p = FALSE)
Quantile  | qpch(p, cuts, levels, lower.tail = TRUE, log.p = FALSE)
Hazard function  | hpch(x, cuts, levels, log = FALSE)
Cumulative hazard fun  | Hpch(x, cuts, levels, log.p = FALSE)
Random gen for pch distr  | rpch(n, cuts, levels)



```r
library(eha)
```

```
## Warning: package 'eha' was built under R version 3.2.5
```

```
## 
## Attaching package: 'eha'
```

```
## The following objects are masked _by_ '.GlobalEnv':
## 
##     qpch, rpch
```

```r
dimx = seq(0,10,0.1)
hazfunc = hpch(dimx, 5, c(0.07,0.14), log = FALSE) #corresponding hazMale on p.13
plot(hazfunc ~ dimx,type='n')
lines(hazfunc ~dimx)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
# Survival from hazard:
# 1. Differences
dimdiff = diff(dimx)
# 2. cumulative hazard function with cumsum, then survival by using S=exp(-integral(H))
survfunc = exp(-cumsum(hazfunc[1:length(hazfunc)-1]*dimdiff))
 # or with the 
# 3. Plot this
plot(survfunc ~ dimx[1:length(survfunc)],type='n',main='Survival function for stepwise linear hazard')
lines(survfunc ~ dimx[1:length(survfunc)])
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-2.png)

```r
# Mean survival time
sum(survfunc*dimdiff)
```

```
## [1] 6.737904
```

```r
# This would be the mean.
# Median survival time
qpch(0.5, 5, c(0.07,0.14))
```

```
## [1] 7.45103
```

```r
 # or
tt.linhaz = rpch(100, 5, c(0.07,0.14))
mean(tt.linhaz)
```

```
## [1] 8.634533
```

```r
median(tt.linhaz)
```

```
## [1] 7.461431
```


2.6 Log Normal

```r
ml=0 # 1, 2
dl = 0.25
normHaz <- function(x,meanlog,sdlog){
  dlnorm(x, meanlog, sdlog)/plnorm(x, meanlog, sdlog)
}
curve(normHaz(x,meanlog = ml, sdlog=dl),from=0,to=100,ylab='lognorm Hazard')
curve(normHaz(x,meanlog = 2, sdlog=dl),from=0,to=100,ylab='lognorm Hazard',add=T,col='red')
legend('top',c('meanlog=0','meanlog=2'),
       col = c('black','red'),lty=c(1,1))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

If a disease followed such a risk profile, the hazard would be verz high at young age and then being zero after around 18 years of age.

## Chapter 3
###Nonparametric Survival model: 
Kaplan-Meier (and Nelson-Altschuler)

```r
tt = c(7,6,6,5,2,4)
cens = c(0,1,0,0,1,1)
Surv(tt,cens)
```

```
## [1] 7+ 6  6+ 5+ 2  4
```

```r
result.km <- survfit(Surv(tt,cens) ~1, conf.type='log-log') # here add type ='fh' if you are interested in the Nelson-Altschuler
summary(result.km)
```

```
## Call: survfit(formula = Surv(tt, cens) ~ 1, conf.type = "log-log")
## 
##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
##     2      6       1    0.833   0.152       0.2731        0.975
##     4      5       1    0.667   0.192       0.1946        0.904
##     6      3       1    0.444   0.222       0.0662        0.785
```

```r
plot(result.km)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
Now with real data

```r
timeMonths = gastricXelox$timeWeeks*7/30.25
censored = gastricXelox$delta
gastric.km = survfit(Surv(timeMonths,censored)~1,conf.type='log-log')
plot(gastric.km,conf.int=T,main='PFS Progression Free Survival, GastricXelox')
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

To obtain median follow-up time, you first need to switch censored and non-censored 

```r
censored.follow = 1-censored
```
Reverse Kaplan-Meier

```r
survfit(Surv(timeMonths,censored.follow)~1)
```

```
## Call: survfit(formula = Surv(timeMonths, censored.follow) ~ 1)
## 
##       n  events  median 0.95LCL 0.95UCL 
##    48.0    16.0    27.8    21.1    50.2
```

## Smoothed hazard function Ch.3.4

```r
library(muhaz)
```

```
## Warning: package 'muhaz' was built under R version 3.2.5
```

```r
smoothy = 2.25 # the smoothing parameter b
plot(muhaz(tt,cens,max.time=8,bw.grid=smoothy,bw.method='global',b.cor='none'))
```

```
## Warning in muhaz(tt, cens, max.time = 8, bw.grid = smoothy, bw.method = "global", : maximum time > maximum Survival Time
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

```r
gastric.haz = pehaz(timeMonths,censored,width=1,max.time=20)
```

```
## 
## max.time= 20
## width= 1
## nbins= 20
```

```r
plot(gastric.haz,ylim=c(0,0.15))
gastric.smooth = muhaz(timeMonths,censored,bw.smooth=20,b.cor='left',max.time = 20)
lines(gastric.smooth)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-2.png)

```r
haz = gastric.smooth$haz.est
times = gastric.smooth$est.grid
surv = exp(-cumsum(haz[1:(length(haz)-1)]*diff(times)))
plot(surv~times[1:(length(times)-1)])
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-3.png)


## Left Truncation
Channing House Data, left truncated because subjects who die at older ages more likely to enter the center than patients who died young. Treat 'entry' as left truncation: 

```r
head(ChanningHouse)
```

```
##    sex entry exit time cens
## 1 Male   782  909  127    1
## 2 Male  1020 1128  108    1
## 3 Male   856  969  113    1
## 4 Male   915  957   42    1
## 5 Male   863  983  120    1
## 6 Male   906 1012  106    1
```

```r
ChanningHouse <- within(ChanningHouse,{
     entryYears <- entry/12
     exitYears <- exit/12})
ChanningMales <- ChanningHouse[ChanningHouse$sex == 'Male',]
# Estimate survival distribution 
# Kaplan-Meier
result.km <- survfit(Surv(entryYears,exitYears,cens,type='counting')
                     ~1, data=ChanningMales)
plot(result.km, xlim=c(63,101),conf.int=F)
# Nelson-Altschuler-Aalen
result.naa <- survfit(Surv(entryYears,exitYears,cens,type='counting')
                      ~1,type='fleming-harrington',data=ChanningMales)
lines(result.naa,col='blue',conf.int=F)
legend('top',legend=c('Kaplan-Meier','Nelson-A-A'), lty=1,col=c('black','blue'))
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)



## Exercises page 42
3.1 Fig. 3.2, median surv is 6 and 95% conf is 2 and NA

```r
survfit(Surv(tt,cens)~1,conf.type='log-log')
```

```
## Call: survfit(formula = Surv(tt, cens) ~ 1, conf.type = "log-log")
## 
##       n  events  median 0.95LCL 0.95UCL 
##       6       3       6       2      NA
```
3.2.

```r
gastric.km
```

```
## Call: survfit(formula = Surv(timeMonths, censored) ~ 1, conf.type = "log-log")
## 
##       n  events  median 0.95LCL 0.95UCL 
##   48.00   32.00   10.30    5.79   15.27
```

```r
# To get the results at different quartiles, do https://stat.ethz.ch/R-manual/R-devel/library/survival/html/quantile.survfit.html
quantile(gastric.km, probs = c(0.25, 0.5, 0.75), conf.int = TRUE)
```

```
## $quantile
##        25        50        75 
##  4.165289 10.297521        NA 
## 
## $lower
##        25        50        75 
##  2.545455  5.785124 14.809917 
## 
## $upper
##        25        50        75 
##  6.479339 15.272727        NA
```

First quartile =  (2.55,4.16,6.48)  (5%,bestestimate,95%)
Is there another way to extract the values from the object survfit than with the quantile function?

3.3

```r
par(mfrow=c(2,1))
# already done earlier:
#gastric.smooth = muhaz(timeMonths,censored,bw.smooth=20,b.cor='left',max.time = 20)
plot(gastric.smooth,ylim=c(0,0.15))
lines(muhaz(timeMonths,censored,bw.smooth=20,bw.grid=20,b.cor='left',max.time = 20),col='blue')
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)






# General R repetition


```r
x.vec = 1:10
y.vec = 3 + 2*x.vec + rnorm(10,mean=0,sd=2)
plot(y.vec ~ x.vec)

res.lm = lm(y.vec ~ x.vec)
abline(res.lm)
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png)
This creates a linear equation y = 3 + 2X + epsilon
And add a fitted line (linear regression)



