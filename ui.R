library(shiny)

# Define UI for random distribution application 
fluidPage(
    
  # Application title
  titlePanel("Tabsets"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", "Distribution type:",
                   c("Normal" = "norm",
                     "Student t" = "t",
                     # "Log-normal" = "lnorm",
                     "Uniform to Normal" = "U2N")),
      br(),
      conditionalPanel(condition = "input.dist=='norm'",
        radioButtons("sides", "Which side", inline=TRUE,
                   c("Left" = "left",
                     "Right" = "right",
                     "Both" = "both"))),
      conditionalPanel(condition = "input.dist=='norm'",
        sliderInput("p", "Probability:",
                                  value = 0.20,
                                    min = 0.00,
                                    max = 0.50,
                                   step = 0.01)),
      conditionalPanel(condition = "input.dist=='t'",
            sliderInput("t.df","DF",min=1, max=20,value=5, step=1)),
      conditionalPanel(condition = "input.dist=='U2N'",
            sliderInput("n","number of observations:",min=50, max=1000,value=200, step=50))
      ), 
      
      # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
        conditionalPanel(condition = "input.dist !='U2N'",
                          tabPanel("Plot", plotOutput("plot"))), 
        conditionalPanel(condition = "input.dist =='U2N'",
                          tabPanel("Plot", plotOutput("plotunif"),
                                           plotOutput("plotnorm"))), 
        tabPanel("Summary", verbatimTextOutput("summary"))#, 
        #tabPanel("Table", tableOutput("table"))
      )
    )
  )
)