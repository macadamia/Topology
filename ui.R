
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(markdown)

usePlotly <- T
if(usePlotly){
  library(plotly)
}

shinyUI(
  fluidPage(
  # Application title
  titlePanel("Topology Visualiser"),

  absolutePanel(
    top = 0, left = 0, right = 0,
    draggable = TRUE,
    wellPanel(
      if(usePlotly){
        plotlyOutput("topoPlotly", height = "800px", width = "100%")
      } else {
        plotOutput("topoPlot", height = "800px", width = "100%")
      }
    )
  ),

  absolutePanel(
    bottom = 20, width = 300, height=400,
    draggable = TRUE,
    wellPanel(
      HTML(markdownToHTML(fragment.only=TRUE, text=c(
        "Move this if it gets in the way"
      ))),
      selectInput("Variety", label = h3("Select Variety"),
                  choices = list("Calypso" , "Keitt", "1243" )),
      #this inputs gets rewitten when variety changes
      selectInput("treeNum", "Tree",
                  list('1','6','9','11','12','17','20','25','26','30','32','34','37','40','45'))
    ),
    style = "opacity: 0.92"
  ),
  absolutePanel(
    bottom=20, right= 20, width = 300,
    draggable = TRUE,
    wellPanel(
      HTML(markdownToHTML(fragment.only=TRUE, text=c(
        "Move this if it gets in the way"
      ))),
      plotOutput("legend")
    ),
    style = "opacity: 0.92"
    )
  )
)
