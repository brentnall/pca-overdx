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


