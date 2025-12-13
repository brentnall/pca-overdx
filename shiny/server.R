
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


