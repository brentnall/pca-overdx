## Projections of overdiagnosis change for scenario with increased PSA testing in men 50-69y and decreased testing in men 70y+.

####################################
## Libraries  ##
####################################

library("readxl")

####################################
## Data  ##
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

####################################
## Functions  ##
####################################

## Overdiagnosis estimate
fn.overdxc2<-function(inage, inadj, inadj2){

    nl<-15
    
    ## all-cause mortality assumption age 50-64
    hall<-mymort_ref$mort[(inage+1):(inage+15)]*inadj

    ## non prostate cancer mortality rate assumption age 50-64
    h2<-mymort_ref$xpc[(inage+1):(inage+15)]*inadj
   
    S2<-c(1,exp(-cumsum(h2))) ## competing risks, survival

    S1L<- c(1,1,1,1,1-seq(1,12)*1/12) ## net survival
    S1<-  c(1,1,1,1,1-seq(1,12)*(0.0736)) ## net survival
    S1U<- c(1,1,1,1,1-seq(1,12)*((100-26.7)/12)/100)

    h1L<- diff(-log(S1L)) ## net hazard
    h1L[nl]<-9999999
    h1<-  diff(-log(S1)) ## net hazard
    h1U<- diff(-log(S1U)) ## net hazard

    myovdxL<- 1 - cumsum( (h1L/(h1L+h2)) * (1-exp(-(h1L+h2))) * S1L[1:nl] * S2[1:nl])
    myovdx<-  1 - cumsum( (h1/ (h1 +h2)) * (1-exp(-(h1+h2)) ) * S1[1:nl]  * S2[1:nl])
    myovdxU<- 1 - cumsum( (h1U/(h1U+h2)) * (1-exp(-(h1U+h2))) * S1U[1:nl] * S2[1:nl])

    myout<-c(1-prod(1-hall), myovdx[nl], myovdxL[nl], myovdxU[nl])

    myout

}

####################################
## Analysis  ##
####################################

## estimate crude overdx by age
myout0<-sapply(50:85, function(inage) fn.overdxc2(inage, 1))

## number cancers 50-59, 60-69, 70-79, 80+, - from audit mulitplied up to pop
myage<-c(294,688,673,198)*4.47

##age mid points
myagemid<-c(55,65,75,85)

## base case
mybaseov<-myout0[2,myagemid-50]
## plausible range lower overdx
mylowerov<-myout0[3,myagemid-50]
## plausible range upper overdx
myupperov<-myout0[4,myagemid-50]
## for comparison - scharr model estimate
myscharrov<-c(0.372, 0.67, 0.9, 0.99)

mybaseov%*%myage
mylowerov%*%myage
myupperov%*%myage
myscharrov%*%myage

## plots in report

pdf("overdxscenarioa.pdf", width=6,height=6)

myincrease<-c(1,1.35,1.5,2,3) ## scenario for increased cancer detection in men 50-69y from organised screening, above current opportunistic screening 

mydecrease<-seq(1,0.2, by=-0.1) ## scenario for decreased cancer detection in men 70y+ from withdrawal of opportunistic screening 

## base case
myincreasenum<-sum(myage[1:2]*mybaseov[1:2])*myincrease

mydecreasenum<- sum(myage[3:4]*mybaseov[3:4])*mydecrease

myovbase<-mybaseov%*%myage

plot(1-mydecrease,myincreasenum[1]+mydecreasenum-myovbase,type="l", ylim=c(-2000,2000), xlab="Proportional decrease 70y+", ylab="Additional overdiagnosed prostate cancer / yr (n)", sub="Asymptomatic prostate cancer detection", main="", xlim=c(0,0.9))

abline(h=0, col="gray", lty=5)

sapply(2:4,function(idx) lines(1-mydecrease,myincreasenum[idx]+mydecreasenum-myovbase,type="l", col=heat.colors(6)[idx], lty=idx))

grid()

dev.off()

##Figure b - plausible lower range of overdx
pdf("overdxscenariob.pdf", width=6,height=6)

## lower overdx
myincreasenum<-sum(myage[1:2]*mylowerov[1:2])*myincrease

mydecreasenum<- sum(myage[3:4]*mylowerov[3:4])*mydecrease

## lower case
myovlow<-c(mylowerov%*%myage)

plot(1-mydecrease,myincreasenum[1]+mydecreasenum - myovlow, type="l", ylim=c(-2000,2000), xlim=c(0,0.9),xlab="Proportional decrease 70y+", ylab="Additional overdiagnosed prostate cancer / yr (n)", lty=1, main="", sub="Asymptomatic prostate cancer detection")

abline(h=0, col="gray", lty=5)

sapply(2:4,function(idx) lines(1-mydecrease,myincreasenum[idx]+mydecreasenum - myovlow,type="l", col=heat.colors(6)[idx], lty=idx))

grid()
#legend("topright", rev(c("50-69y as 2018", "50-69 double", "50-69 triple")), col=rev(c(1,heat.colors(7)[2:3])), lty=1, bty="n")

dev.off()

##Figure b - plausible upper range of overdx
pdf("overdxscenarioc.pdf", width=6,height=6)
## upper overdx

myincreasenum<-sum(myage[1:2]*myupperov[1:2])*myincrease

mydecreasenum<- sum(myage[3:4]*myupperov[3:4])*mydecrease

myovhi<-c(myupperov%*%myage)


plot(1-mydecrease,myincreasenum[1]+mydecreasenum - myovhi, type="l", ylim=c(-2000,2000), xlab="Proportional decrease 70y+", ylab="Additional overdiagnosed prostate cancer / yr (n)", lty=1, main="",sub="Asymptomatic prostate cancer detection", xlim=c(0,0.9))

abline(h=0, col="gray", lty=5)

sapply(2:4,function(idx) lines(1-mydecrease,myincreasenum[idx]+mydecreasenum-myovhi,type="l", col=heat.colors(6)[idx],lty=idx))

grid()

dev.off()



####### Consider frontier where overdx is unchanged
myincrease<-seq(1, 12, by=0.001)
mydecrease<-seq(1, 0.1, by=-0.001)

## base 
myincreasenum <- sum(myage[1:2]*mybaseov[1:2])*myincrease
mydecreasenum <- sum(myage[3:4]*mybaseov[3:4])*mydecrease
myfrontier<-  sapply(1:length(mydecreasenum),
                     function(tidx) which.min(abs((myincreasenum-myincreasenum[1]) - (mydecreasenum[1]-mydecreasenum[tidx])) ))

## lower 
myincreasenum_low <- sum(myage[1:2]*mylowerov[1:2])*myincrease
mydecreasenum_low <- sum(myage[3:4]*mylowerov[3:4])*mydecrease
myfrontier_low<-  sapply(1:length(mydecreasenum_low),
                     function(tidx) which.min(abs((myincreasenum_low-myincreasenum_low[1]) - (mydecreasenum_low[1]-mydecreasenum_low[tidx])) ))


## higher
myincreasenum_hi <- sum(myage[1:2]*myupperov[1:2])*myincrease
mydecreasenum_hi <- sum(myage[3:4]*myupperov[3:4])*mydecrease
myfrontier_hi<-  sapply(1:length(mydecreasenum_hi),
                     function(tidx) which.min(abs((myincreasenum_hi-myincreasenum_hi[1]) - (mydecreasenum_hi[1]-mydecreasenum_hi[tidx])) ))



pdf("overdxscenariod.pdf", width=6,height=6)

plot(1-mydecrease, myincrease[myfrontier], type="l", ylim=c(1,3), xlim=c(0,0.9),
     xlab="Proportional decrease 70y+", ylab="Proportional increase aged 50-69y", sub="Asymptomatic prostate cancer detection", main="", lwd=3)

abline(h=1.35, lty=5, col=heat.colors(6)[2], lwd=0.5)

abline(h=1.5, lty=5, col=heat.colors(6)[3], lwd=0.5)

abline(h=2, lty=5, col=heat.colors(6)[4], lwd=0.5)

lines(1-mydecrease, myincrease[myfrontier], type="l",lwd=2)

lines(1-mydecrease, myincrease[myfrontier_low], col="gray", lty=2, lwd=3)

lines(1-mydecrease, myincrease[myfrontier_hi], col="gray", lty=2, lwd=3)

grid()

dev.off()
