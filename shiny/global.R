library("shiny")
library("readxl")

####################################
##                                ##      
## Crude overdiagnosis estimates  ##
##                                ## 
####################################

## English all-cause mortality rates 
mymort<-readxl::read_xlsx("../timeseries3yrqx19802021.xlsx", sheet="Eng Males qx", skip=4)

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

fn.format<-function(ind, ndigit=2){                                                       format(round(ind,ndigit), nsmall=ndigit)
}


## function t
fn.overdxc2<-function(inage, inadj){

    nl<-15
    
    ## all-cause mortality assumption age 50-64
    hall<-mymort_ref$mort[(inage+1):(inage+15)]*inadj

    ## non prostate cancer mortality rate assumption age 50-64
    h2<-mymort_ref$xpc[(inage+1):(inage+15)]*inadj
   
    S2<-c(1,exp(-cumsum(h2))) ## competing risks, survival

    S1<-c(1,1,1,1,1-seq(1,12)*(0.0736)) ## net survival

    h1<-diff(-log(S1)) ## net hazard
    
    myovdx<- 1 - cumsum( (h1/(h1+h2)) * (1-exp(-(h1+h2))) * S1[1:nl] * S2[1:nl])


    myout<-c(1-prod(1-hall), myovdx[nl])

    myout

}


fn.format.overdx<-function(indeath, inoverdx){
    myout<-c(paste0(fn.format(indeath*100,1),"%"), 
             paste0(fn.format(inoverdx*100,1),"%") )

    myout

    }


