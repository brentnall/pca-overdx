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


fn.format.overdx<-function(indeath, inoverdx){
    myout<-c(paste0(fn.format(indeath*100,1),"%"), 
             paste0(fn.format(inoverdx*100,1),"%") )

    myout

    }

myout0<-sapply(50:85, function(inage) fn.overdxc2(inage, 1))

ui <- pageWithSidebar(

  # App title ----
  headerPanel("Prostate cancer overdiagnosis risk from PSA testing"),

  # Sidebar panel for inputs ----

  sidebarPanel(
      h1("About"),
      "This app estimates 15y mortality risk for men at different ages, and the risk of overdiagnosis if diagnosed with prostate cancer from a PSA test. This is based on data reported from a UK screening trial (CAP), and mortality rates in English men from 2021-23. It includes an option to adjust mortality rates for men who are healthier than average (eg. because they undertake regular exercise), or less healthy than average (eg. because they are lifelong heavy smokers).",

      h1("Input"),

          numericInput("inage", 
                        h3("Age"), 
                        value = 50),
           
          numericInput("inadj", 
                        h3("Competing risks relative adjustment (1.0 = population risk)"), 
                       value = 1.0,
                       step=0.05),
      actionButton("calc", "Update calculations with adjustment"),

      h1("User instructions"),
      h2("What should I enter in the age and competing risk adjustment boxes?"),
      "Enter the age at which a man is to be screened in the Age Box. If desired, adjust for a higher or lower death rate due to other causes than prostate cancer in the next box, and hit the button to update the calculations. Otherwise use 1.0 in the adjustment box to reflect the average English man. The results are shown in the other panel.",
      h2("How should I choose what to enter for competing hazards adjustment?"),
      "Use 1.00 for the average man. To explore what happens for cohorts at a higher or lower risk of death you can change this. For instance, enter 2.0 to double the risk, 0.5 to halve the risk. One reason for considering an adjustment less than 1.0 is that participants in clinical trials are known to have lower competing mortality risks than the general population. For example, 0.86 might be used to reflect the ERSPC trial participants (https://journals.sagepub.com/doi/abs/10.1258/096914104774061074); or 0.46 for UK Biobank males (https://pubmed.ncbi.nlm.nih.gov/28641372/). On the other hand, suppose one wishes to estimate overdiagnosis for heavy lifelong smokers, with a higher mortality risk than on average. The relative hazard after age 60y is likely at least approximately doubled (https://www.bmj.com/content/328/7455/1519), so one could use 2.0.",
      h2("Limitations"),
      "The calculations are based on a number of assumptions, which are discussed in the accompanying paper",   
      h3("License"),
      "GNU GPL v3. This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. Copyright 2025, Adam Brentnall"

  ),

  # Main panel for displaying outputs ----
  mainPanel(
      h1("Results"),
      plotOutput('plot1'),
      h3(textOutput("death")),
      h3(textOutput("death2")),
      plotOutput('plot2'),
      h3(textOutput("overdx")),
      h4(textOutput("overdxExpl")),
      h4(textOutput("overdxLU")),
      h3("Reason for overdiagnosis (with main estimate of total overdiagnosis)"),
      h4("1. Overdiagnosis due to detection of indolent disease: approximately 11.7%."),
      h4(textOutput("overdx1"))
         

  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {

    v <- reactiveValues(myout=myout0)

    observeEvent(input$calc, {

        validate(
            need(input$inage >=40 & input$inage <=85, "Error - Age must be between 40y and 85y"),
            need(input$inadj>0, "Error - Adjustment for life expectancy must be >0"))
        
        v$myout<-sapply(50:85, function(inage) fn.overdxc2(inage, input$inadj))
    })
    
        output$plot1 <- renderPlot({

        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        plot(50:85,v$myout[1,]*100, type="l", ylim=c(0,100), ylab="Probability die in 15 years (%)
", xlab="Age (years)",lwd=2)
        grid()
        points(input$inage, 100*v$myout[1,input$inage-49], lwd=10, col=2)
        
        })

        output$plot2 <- renderPlot({
        
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        plot(50:85,v$myout[2,]*100, type="l", ylim=c(0,100), ylab="Overdiagnosis probability in screen-detected prostate cancer (%)", xlab="Age (years)", lwd=2, col=4)
        lines(50:85,v$myout[3,]*100, type="l", lwd=1, col=4, lty=2)
        lines(50:85,v$myout[4,]*100, type="l", lwd=1, col=4, lty=2)
        grid()
        points(input$inage, 100*v$myout[2,input$inage-49], lwd=10, col=5)
        lines(rep(input$inage,2),c(100*v$myout[3,input$inage-49],100*v$myout[4,input$inage-49]), type="l", lwd=3, col=5, lty=3)
        legend("topleft", c("Best estimate", "Plausible range"), col=c(5,5), lty=c(NA,3), pch=c(1,NA), lwd=c(10,3))
        
    })
    
    output$death <- renderText({
        
        paste0("Probability die within 15y from age ", input$inage, ": ", paste0(fn.format(100*v$myout[1,input$inage-49],1),"%"))
        
    })

    output$overdx <- renderText({
        
           paste0("Total overdiagnosis approximately: ", fn.format(100*v$myout[2,input$inage-49],1), "%.")
            })

    output$overdxExpl <- renderText({
        
           paste0("This means that for every 1000 cancers detected at screening in men aged ", input$inage, " years, our best estimate is that approximately ", fn.format(1000*v$myout[2,input$inage-49],0), " would not be detected (without screening) in the next 15y")
            })

    
    output$overdxLU <- renderText({
        
        paste0("We acknowledge some uncertainty in this estimate, and find that overdiagnosis might be as low as ", fn.format(100*v$myout[3,input$inage-49],0), "%, or as high as ", fn.format(100*v$myout[4,input$inage-49],0), "%.")
     })

    output$overdx1 <- renderText({
        
        paste0("2. Overdiagosis due to death from other causes than prostate cancer: ", fn.format(100*v$myout[2,input$inage-49] - 11.7,1), "%")
     })

    output$overdxcommentary <- renderText({
        
        paste0("In this scenario the main reason for overdiagnosis is ", c("potential indolent prostate cancer", "death from other causes than prostate cancer.")[((100*v$myout[2,input$inage-49] - 11.7)>0) +1])
     })

    
    }


shinyApp(ui, server)
