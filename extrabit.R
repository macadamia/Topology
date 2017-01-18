# Sidebar with a slider input for number of bins
sidebarLayout(
  sidebarPanel(

    selectInput("Variety", label = h3("Select Variety"),
                choices = list("Calypso" , "Keitt", "1243" )),
    hr(),
    #this inputs gets rewitten when variety changes
    selectInput("treeNum", "Tree",
                list('1','6','9','11','12','17','20','25','26','30','32','34','37','40','45'))

  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("topoPlot", width = "600px", height = "800px")
  )
)



library(markdown)

shinyUI(fluidPage(style="padding-top: 80px;",
                  h1("Absolutely-positioned panels"),
                  absolutePanel(
                    bottom = 20, right = 20, width = 300,
                    draggable = TRUE,
                    wellPanel(
                      HTML(markdownToHTML(fragment.only=TRUE, text=c(
                        "This is an absolutePanel that uses `bottom` and `right` attributes.

                        It also has `draggable = TRUE`, so you can drag it to move it around the page.

                        The slight transparency is due to `style = 'opacity: 0.92'`.

                        You can put anything in absolutePanel, including inputs and outputs:"
                      ))),
                      sliderInput("n", "", min=3, max=20, value=5),
                      plotOutput("plot2", height="200px")
                      ),
                    style = "opacity: 0.92"
                      ),
                  absolutePanel(
                    top = 0, left = 0, right = 0,
                    fixed = TRUE,
                    div(
                      style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE;",
                      HTML(markdownToHTML(fragment.only=TRUE, text=c(
                        "This absolutePanel is docked to the top of the screen
                        using `top`, `left`, and `right` attributes.

                        Because `fixed=TRUE`, it won't scroll with the page."
                      )))
                      )
                      ),
                  plotOutput("plot", height = "800px")
                      ))