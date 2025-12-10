##_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
##
## Analysis code for: Why is prostate cancer overdiagnosis a problem with PSA testing
## and what can be done about it? (Brentnall et al, submitted 2025).
##
## Date: 9th December 2025
## Author: Adam Brentnall
##_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/

## Libraries
library("tidyverse")
library("readxl")

######################################################################################
## Load data
## Data extracted from Martin RM, Turner EL, Young GJ, Metcalfe C, Walsh EI, Lane JA, et al.
## Prostate-Specific Antigen Screening and 15-Year Prostate Cancer Mortality: A Secondary
## Analysis of the CAP Randomized Clinical Trial. JAMA. 2024;331:1460-70.
## Figure 2c, extracted using https://plotdigitizer.com/app

## control arm
mydta_control<-read_csv("plot-data-control-arm.csv")
mydta_control<-mydta_control[order(mydta_control$x),]

## screening arm
mydta_screen<-read_csv("plot-data-screening-arm.csv")
mydta_screen<-mydta_screen[order(mydta_screen$x),]

## interpolate points
lw1<-loess(y~x, mydta_screen, span=0.25)

lw2<-loess(y~x, mydta_control, span=0.25)

## visual 
plot(lw1$x, lw1$fitted, type="l")
points(lw1$x, lw1$y)
lines(lw2$x,lw2$fitted)
points(lw2$x, lw2$y)

## create data with same x-vals for both arms
myy_screen<-predict(lw1, newdata=data.frame(x=seq(0.5,15, by=0.1)))

myy_control<-predict(lw2, newdata=data.frame(x=seq(0.5,15, by=0.1)))

mynewdta<-data.frame(x=c(0,seq(0.5,15,by=0.1)), yscreen=c(0,myy_screen), ycontrol=c(0,myy_control))

######################################################################################
## Estimate excess / overdiagnosis using data

## cumulative hazard
mynewdta$hscreen <- -log(1-mynewdta$yscreen/100)
mynewdta$hcontrol <- -log(1-mynewdta$ycontrol/100)

## summary stat for net overdiagnosis based on reported statistics in paper
myse1<-(-0.0682+0.0706)/(2*1.96)

myse2<-(0.0721-0.0695)/(2*1.96)

myse3<-sqrt(myse1^2+myse2^2)

mypt<-0.0708 - 0.0694

myci<-c(mypt+1.96*myse3, mypt, mypt-1.96*myse3)*100

## Absolute risk difference and 95%CI at 15y
myci

## Baseline screen
Sbase<-2249/(113679+75707)

## Net overdiagnosis at 15y and 95%CI
(myci/100)/Sbase

## excess through time
mynewdta$oS<-(mynewdta$yscreen/100-mynewdta$ycontrol/100)/Sbase

## smoother to interpolate
lw3b<-loess(oS~x, mynewdta, span=0.2)

mynewdta$oSs<-lw3b$fitted

## set max excess to 1 at beginning
myidx<-max(seq(1,nrow(mynewdta))[mynewdta$oSs>1])

mynewdta$oSs2<-mynewdta$oSs

mynewdta$oSs2[1:myidx]<-1

### Estimate hazard using rate of change of cumulative hazard
## Use moving average of three points to estimate hazard from cumulative hazard to reduce noise
##
myplotdta<-data.frame(grad=c(NA,diff(mynewdta$hscreen) / diff(mynewdta$hcontrol)), x=mynewdta$x)

myplotdta$hscreens<-c(NA,sapply(2:(nrow(mynewdta)-1), function(idx) mean(mynewdta$hscreen[(idx-1):(idx+1)])),NA)

myplotdta$hcontrols<-c(NA,sapply(2:(nrow(mynewdta)-1), function(idx) mean(mynewdta$hcontrol[(idx-1):(idx+1)])),NA)

myplotdta$grad2<-c(NA,diff(myplotdta$hscreens)/diff(myplotdta$hcontrols))


myplotdta$hscreens2<-c(NA,NA,sapply(3:(nrow(mynewdta)-2), function(idx) mean(mynewdta$hscreen[(idx-2):(idx+2)])),NA, NA)

myplotdta$hcontrols2<-c(NA,NA, sapply(3:(nrow(mynewdta)-2), function(idx) mean(mynewdta$hcontrol[(idx-2):(idx+2)])),NA, NA)

myplotdta$grad3<-c(NA,diff(myplotdta$hscreens2)/diff(myplotdta$hcontrols2))

## plot for paper
myscale<-1

postscript(file="CAP-overdx-net.eps",horiz=FALSE,onefile=FALSE,width=14*myscale, height=10*myscale, pointsize=14)

par(mfrow=c(2,2))
## Plot (a): cumulative incidence
plot(mynewdta$x, mynewdta$yscreen, type="l", xlab="Time t (years)", ylab="Net risk, %", main="(a)")
lines(mynewdta$x, mynewdta$ycontrol, col=2)
grid()
legend("topleft", c("Screening", "Control"), col=c(1,2), lty=1)

## Plot (b): Difference in cumulative incidence (smoothed)
lw4<-loess(yscreen-ycontrol~x, mynewdta, span=0.2)
plot(mynewdta$x, lw4$fitted, type="l", xlab="Time t (years)", ylab="Difference in net risk (screen-control), %", main="(b)")
grid()

## Plot (c): Excess (smoothed)
plot(mynewdta$x, mynewdta$oSs2*100, type="l", xlab="Time t (years)", ylab="Net Overdiagnosis O*(t), %", ylim=c(0,100), main="(c)")
##lines(c(0,15), c(10,10),lty=2, col="gray")
grid()

## Plot (d): Compensatory drop, using estimated hazard ratio
mystart<-2.4
## Raw hazard ratio from moving average to estimate hazard separately in control and screening
plot(myplotdta$x, myplotdta$grad3, log="y", ylim=c(2/3,3/2), type="p", pch=".", xlab="Time t (years)", ylab="HR prostate cancer (screening/control)", main="(d)")
## smoothed hazard ratio
lwgrad<-loess(grad3~x, myplotdta[myplotdta$x>mystart,], span=0.4)
lines(lwgrad$x, lwgrad$fitted, col=1, lwd=2)
lines(myplotdta$x[myplotdta$x<=mystart], myplotdta$grad3[myplotdta$x<=mystart], lwd=1)
lines(c(0,20),c(1,1), lty=2, col="gray")
legend("topright", c("Raw estimate", "Smoothed estimate"), col=c(1,1), lty=c(3, 1), lwd=c(1,2))
grid()

dev.off()

####################################
##                                ##      
## Crude overdiagnosis estimates  ##
##                                ## 
####################################

## English all-cause mortality rates 
mymort<-readxl::read_xlsx("timeseries3yrqx19802021.xlsx", sheet="Eng Males qx", skip=4)

## Use period rates from 2021-23
mymort_ref<-data.frame(age=0:100, mort=mymort$`2021-2023`)

## Prostate cancer mortality rates
## Data from: #https://digital.nhs.uk/data-and-information/publications/statistical/compendium-mortality/current/mortality-from-prostate-cancer/mortality-from-prostate-cancer-crude-death-rate-by-age-group-3-year-average-m
## Rates -
## 1+	 01-04	 05-14	 15-34	 35-64	 65-74	 75+
mymort_prostate<-c(0.12,	0.05,	0.02,	4.82,	71.59,	373.84)/100000
mymort_ref$pc<-rep(mymort_prostate, c(5,10,20,30,10,26))

## Subtract rate of death from prostate cancer
mymort_ref$xpc<-mymort_ref$mort-mymort_ref$pc

## Simple model for rate of decline net overdiagnosis based on the plot
myrate<-(100-11.7)/12

myrate

## function t
fn.overdxc2<-function(h2, nl=15){
    
    S2<-c(1,exp(-cumsum(h2))) ## competing risks, survival

    S1<-c(1,1,1,1,1-seq(1,12)*(0.0736)) ## net survival

    h1<-diff(-log(S1)) ## net hazard
    
    myovdx<- 1 - cumsum( (h1/(h1+h2)) * (1-exp(-(h1+h2))) * S1[1:nl] * S2[1:nl])

    myovdx[nl]

}

## all-cause mortality assumption age 50-64
thisref1<-mymort_ref$mort[51:65]
## non prostate cancer mortality rate assumption age 50-64
thisref2<-mymort_ref$xpc[51:65]
## Probability alive by end 15y
1-prod(1-thisref1)
## Probability prostate cancer not diagnosed by end 15y
fn.overdxc2(thisref2)

## age 70y (to 85)
thisref1<-mymort_ref$mort[71:85]
thisref2<-mymort_ref$xpc[71:85]
1-prod(1-thisref1)
fn.overdxc2(thisref2)

## age 80y (to 95)
thisref1<-mymort_ref$mort[81:95]
thisref2<-mymort_ref$xpc[81:95]
1-prod(1-thisref1)
fn.overdxc2(thisref2)

