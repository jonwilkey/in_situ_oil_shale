library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("In Situ v2 Results"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(

      selectInput("select1", label = "x-axis",
                  choices = list("Number of Wells" =             1,
                                 "NER" =                         2,
                                 "Well D&C Time" =               3,
                                 "Well Capital Cost" =           4,
                                 "Total Well Length" =           5,
                                 "Gas Fraction" =                6,
                                 "Wellhead Gas Price" =          7,
                                 "IRR" =                         8,
                                 "Oil Supply Price" =            9,
                                 "Total Oil Production" =       10,
                                 "Total Capital Investment" =   11,
                                 "Capital per Flowing Barrel" = 12),
                  selected = 1),

      selectInput("select2", label = "y-axis",
                  choices = list("Number of Wells" =             1,
                                 "NER" =                         2,
                                 "Well D&C Time" =               3,
                                 "Well Capital Cost" =           4,
                                 "Total Well Length" =           5,
                                 "Gas Fraction" =                6,
                                 "Wellhead Gas Price" =          7,
                                 "IRR" =                         8,
                                 "Oil Supply Price" =            9,
                                 "Total Oil Production" =       10,
                                 "Total Capital Investment" =   11,
                                 "Capital per Flowing Barrel" = 12),
                  selected = 2)),

    # Show a plot of the generated distribution
    mainPanel(plotOutput("plot"))
  )
))
