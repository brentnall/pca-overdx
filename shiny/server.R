# Define server logic to plot various variables against mpg ----
server <- function(input, output) {

    output$plot1 <- renderPlot({
        validate(
		need(input$inage >=40 & input$inage <=85, "Error - Age must be between 40y and 85y"),
		need(input$inadj>0, "Error - Adjustment for life expectancy must be >0"))

        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

    myout<<-sapply(50:85, function(inage) fn.overdxc2(inage, input$inadj))

    plot(50:85,myout[1,]*100, type="l", ylim=c(0,100), ylab="Probability die in 15y", xlab="Age",lwd=2)
    grid()
    points(input$inage, 100*myout[1,input$inage-49], lwd=10, col=2)
        

  })

    output$plot2 <- renderPlot({

        validate(
            need(input$inage >=40 & input$inage <=85, "Error - Age must be between 40y and 85y"),
            need(input$inadj>0, "Error - Adjustment for life expectancy must be >0"))

        plot(50:85,myout[2,]*100, type="l", ylim=c(0,100), ylab="Probability overdiagnosis, if diagnosed now", xlab="Age", lwd=2, col=4)
        grid()
        points(input$inage, 100*myout[2,input$inage-49], lwd=10, col=5)
        

  })

    output$death <- renderText({
	validate(
		need(input$inage >=40 & input$inage <=85, "Error - Age must be between 40y and 85y"),
		need(input$inadj>0, "Error - Adjustment for life expectancy must be >0"))
        
        paste0("Probability die in 15y from age ", input$inage, ": ", paste0(fn.format(100*myout[1,input$inage-49],1),"%"))
##        paste0("For every 1000 men aged ", input$inage, ", ", fn.format(1000*myout[1,input$inage-49],0), " are expected to die in the next 15y")
        
    })
    
    output$overdx <- renderText({
	validate(
		need(input$inage >=40 & input$inage <=85, "Error - Age must be between 40y and 85y"),
		need(input$inadj>0, "Error - Adjustment for life expectancy must be >0"))

        paste0("For every 1000 cancers detected at screening in men aged ", input$inage, " years, ", fn.format(1000*myout[2,input$inage-49],0), " would not be detected (without screening) in the next 15y")
  })
}

