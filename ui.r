



library(shiny)

# Define UI for slider demo application
shinyUI(fluidPage(
  
  
  #  Application title
  titlePanel("The economic value of environmental data:"),
#   fluidRow(  column(6,
#                     plotOutput("plot1"),
#                     plotOutput("plot2"))),

  # Sidebar with sliders that demonstrate various available
  # options
  sidebarLayout(
    sidebarPanel(
      # Simple integer interval
      sliderInput("rho", "rho:", 
                  min=0, max=10, value=4.5,  step= 0.1),#,
      
      #Decimal interval with step value
       sliderInput("sd_Disch", "sd Disch:", 
                   min = 0.2, max = 0.7, value = 0.4, step= 0.1),
      
      # Specification of range within an interval
      sliderInput("FM", "Fishing mortality:",
                   min = 0.02, max = 0.06, value = 0.025, step=0.005),
      fluidRow(  column(12,
                                             plotOutput("plot1"),
                                             plotOutput("plot2")))
#       
#       # Provide a custom currency format for value display, 
#       # with basic animation
#       sliderInput("format", "Custom Format:", 
#                   min = 0, max = 10000, value = 0, step = 2500,
#                   format="$#,##0", locale="us", animate=TRUE),
#       
#       # Animation with custom interval (in ms) to control speed,
#       # plus looping
#       sliderInput("animation", "Looping Animation:", 1, 2000, 1,
#                   step = 10, animate=
#                     animationOptions(interval=300, loop=TRUE))



    ),


    # Show a table summarizing the values entered

    mainPanel(
    h1(" A notional insurance scheme for the European anchovy",align = "center"),
    fluidRow(
      column(4, wellPanel(
        h5("Parameter", em("rho"),"determines the survival of individuals affected by freshwater discharges from the Guadalquivir River."),
        h5("Parameter", em("lambda"),"determines the survival of individuals affected by the wind."),
        h5("The combined effect of", em("rho"), "and", em("lambda"),"defines juvenile survival, which needs to be high enough for the popullation not to collapse. When a value for", em("rho"), "is chosen, the corresponding ", em("lambda"),"value is calculated."  ),
        h5(em("sd Disch"), "represents the standard deviation of freshwater discharges volume."),
        h5(em("F"), "represents the fishing mortality.")

      )),
      column(3,tableOutput("values")),
      column(3,wellPanel(h5(strong("HCR"), "is a harvest control rule that assumes the fishing mortality constant ignoring the effect of the environment in early survival while",strong("EHCR"),"is an environmentally-sensitive harvest control rule modifiying the fishing mortality according to wind frequency"))),
      column(5,tableOutput("values2"))),
      plotOutput("plot3")
    )
    
  )
))
