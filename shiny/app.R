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

ui <- pageWithSidebar(

  # App title ----
  headerPanel("PSA Overdiagnosis calculator"),

  # Sidebar panel for inputs ----

  sidebarPanel(
      h1("About"),
      "This app estimates mortality risk in 15y, and overdiagnosis if diagnosed with prostate cancer from a PSA test, given age. This is based on data reported from a UK screening trial (CAP), and mortality rates in English men from 2021-23. It includes an option to account for men who are healthier than average (eg. because they do not smoke and undertake regular exercise), or less healthy than average.",
      h1("Limitations"),
      "The calculations are based on a number of assumptions, which are discussed in the accompanying paper",
      h1("How to use"),
      h2("What to enter"),
      "Enter the age at which a man is to be screened in the Age Box. Adjust for a higher or lower death rate due to other causes than prostate cancer in the next box. The calculations are automatically update on the other side",
      h2("How to choose what to enter for competing hazards adjustment?"),
      "Use 1.00 for the average man. Enter more than 1.00 to consider men with a higher risk of death in the next 15y than average (eg. because a smoker, or because already ill with a serious condiction). For instance, 2 would mean twice the risk, 3 would mean three times the risk, and so on. Enter less than 1.00 for lower-risk men than average (eg. participants in clinical trials are often healthier than average, for example to reflect participants in UK Biobank this would be approximately one half, or 0.5 in the box)",
      h1("Input"),

          numericInput("inage", 
                        h3("Age"), 
                        value = 50),

            
          numericInput("inadj", 
                        h3("Competing hazards relative adjustment (1=population, <1 = lower risk, >1 = higher risk)"), 
                       value = 1.0,
                       step=0.01)


  ),

  # Main panel for displaying outputs ----
  mainPanel(
      h1("Results"),
      plotOutput('plot1'),
      h3(textOutput("death")),
      h3(textOutput("death2")),
      plotOutput('plot2'),
      h3(textOutput("overdx"))

  )
)
# Define server logic to plot various variables against mpg ----
server <- function(input, output) {



   
    output$plot1 <- renderPlot({
        validate(
		need(input$inage >=40 & input$inage <=85, "Error - Age must be between 40y and 85y"),
		need(input$inadj>0, "Error - Adjustment for life expectancy must be >0"))

        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

        myadj<-input$inadj

    myout<<-sapply(50:85, function(inage) fn.overdxc2(inage, myadj))

    plot(50:85,myout[1,]*100, type="l", ylim=c(0,100), ylab="Probability die in 15y", xlab="Age",lwd=2)
    grid()
    points(input$inage, 100*myout[1,input$inage-49], lwd=10, col=2)
        

  })

    output$plot2 <- renderPlot({

        myadj<-input$inadj     

        plot(50:85,myout[2,]*100, type="l", ylim=c(0,100), ylab="Probability overdiagnosis, if diagnosed now", xlab="Age", lwd=2, col=4)
        grid()
        points(input$inage, 100*myout[2,input$inage-49], lwd=10, col=5)
        

  })

    output$death <- renderText({
	validate(
		need(input$inage >=40 & input$inage <=85, "Error - Age must be between 40y and 85y"),
		need(input$inadj>0, "Error - Adjustment for life expectancy must be >0"))

        ##        paste0("Probability die in 15y, from given age: ", paste0(fn.format(100*myout[1,input$inage-49],1),"%"))
        paste0("For every 1000 men aged ", input$inage, ", ", fn.format(1000*myout[1,input$inage-49],0), " are expected to die in the next 15y")
        
    })
    
    output$overdx <- renderText({
	validate(
		need(input$inage >=40 & input$inage <=85, "Error - Age must be between 40y and 85y"),
		need(input$inadj>0, "Error - Adjustment for life expectancy must be >0"))

        paste0("For every 1000 cancers detected at screening in men aged ", input$inage, " years, ", fn.format(1000*myout[2,input$inage-49],0), " would not be detected (without screening) in the next 15y")
  })
}

shinyApp(ui, server)
