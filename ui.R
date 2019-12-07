#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("function.R")
source("library.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("sync ggpair"),

    # Show a plot of the generated distribution
    mainPanel(
        # plotOutput("scatter1",
        #            click = "plot_click",
        #            dblclick = "plot_dblclick",
        #            hover = "plot_hover",
        #            brush = "plot_brush"
        # ),
        # verbatimTextOutput("info"),
        # plotOutput("scatter2",
        #            click = "plot_click",
        #            dblclick = "plot_dblclick",
        #            hover = "plot_hover",
        #            brush = "plot_brush"
        # ),
        # 
        # plotlyOutput("scatter3"),
        # verbatimTextOutput("selection"),
        
        plotlyOutput("high", height = "800px", width = "800px"),
        plotlyOutput("high_rm", height = "800px", width = "800px"),
        verbatimTextOutput("high_selection")
    )
))
