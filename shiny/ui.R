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

