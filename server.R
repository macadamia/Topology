
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(igraph)

source('helper.R')

shinyServer(function(input, output, session ) {

  observe({
    var <- input$Variety
    if(var == 'Calypso'){
      thisList <- list('1','6','9','11','12','17','20','25','26','30','32','34','37','40','45')
    }
    if(var == 'Keitt'){
      thisList <- list('2','4','7','13','14','16','19','23','24','28','33','35','38','41','42')
    }
    if(var == '1243'){
      thisList <- list('3','5','8','10','15','18','21','22','27','29','31','36','39','43','44')
    }
    updateSelectInput(session,"treeNum",label="Tree",choices=thisList)
  })

  output$topoPlot <- renderPlot({

    var <- input$Variety
    tn<- input$treeNum
    plantFile <- paste(var,tn,'GC_15_Detail.csv',sep='_')
    print(plantFile)
    csv <- read.table(plantFile,skip=2,sep=',',stringsAsFactors = F)
    header <- unlist(read.table(plantFile,skip=1,sep=',',nrows=1))
    names(csv) <- header
    cbPalette <- c('#E69F00','#009E73','#D55E00', '#0072B2','#56B4E9', '#F0E442','#CC79A7','#999999')

    #every GU is a vertex

    nVerts = dim(csv)[1]


    gu <- strsplit(csv$Gu_Code,'.',fixed=T)

    depth <- (max(nchar(csv$Gu_Code)) + 1 ) / 2  # accounts for . between numbers

    guCode <- matrix(NA,nrow=nVerts,ncol=depth)
    for(v in 1:nVerts){
      gtmp <- gu[[v]]
      guCode[v,1:length(gtmp)] <-  as.numeric(gtmp)
    }

    pruneTypes <- csv$Type_Pruned

    shapes <- rep('circle',nVerts)
    shapes[pruneTypes == 'T'] <- 'square'
    shapes[pruneTypes == 'B'] <- 'diamond'
    shapes[pruneTypes == 'C'] <- 'triangle'

    sizes <- rep(9,nVerts)
    #sizes[pruneTypes == 'B'] <- 11


    colrs <- rep(cbPalette[8],nVerts)
    gCycles <- sort(unique(csv$Growing_Cycle))
    i<-1
    for( j in gCycles){
      colrs[csv$Growing_Cycle == j] <- cbPalette[i]
      i<-i+1
    }

    fColors <- colrs

    g <- make_empty_graph(n=nVerts) %>%
      set_vertex_attr('color',value = colrs) %>%
      set_vertex_attr('label',value = '')


    s <- 1
    prevThings <- 1
    for(i in 2:nVerts){

      gc <- csv$Growing_Cycle[v]
      if(is.na(gc)){
        gcCol <- 'brown'
      } else {
        if(gc == 14) {
          gcCol <- 'green'
        }
        if(gc == 15) {
          gcCol <- 'blue'
        }
      }

      gu <- guCode[i,]
      nThings <- length(which(!is.na(gu)))
      if(any(is.na(gu))){
        e <- head(which(is.na(gu)),1) -1
      } else {
        e <- depth
      }

      #what if we have come to the end of the branch
      if(nThings < prevThings){
        #need to step back, but to where?
        #the place will be one less than the last NA in this column
        for(j in seq(i,1,-1)){ #need to look backwards to find the previous point
          things <- length(which(!is.na(guCode[j,])))
          if(things == (nThings -1)){
            #found the insertion point
            #cat(j,'-->',i,'\n')
            s<-j
            break
          }
        }
      }

      if(gu[e] != 0){
        #a branch
        #where from ? i - 1, but the first instance of that
        possI <- guCode[i,(e-1)] # now we look for the first possI in column e-1
        #search back along this Column
        s <- i - (tail(which(guCode[i:s,(e-1)] == possI),1) - 1)
        g <- g + edge(c(s,i),color='blue')
        #cat('Branch',s,' ->',i,'\n')
      } else {
        #extend
        s<-i-1
        g <- g + edge(c(s,i),color='black')
        #cat('Extend',s,' ->',i,'\n')
        s<-i
      }
      prevThings <- nThings
    }

    tree = layout_as_tree(g,root=1,flip.y=F)


    plot(g,vertex.shape=shapes,vertex.size=sizes,vertex.label.cex=0.8,layout=tree,main=plantFile)

    X <- -2
    symbols(x=X, y=1, circle=11/200, add=TRUE, inches=FALSE,bg='grey')
    symbols(x=X, y=0.75, circle=11/200, add=TRUE, inches=FALSE,bg=cbPalette[1])
    symbols(x=X, y=0.5, circle=11/200, add=TRUE, inches=FALSE,bg=cbPalette[2])
    symbols(x=X, y=0.25, squares = 22/200, add=TRUE, inches=FALSE)
    symbols(x=X, y=0, stars=cbind(11/200,11/200,11/200,11/200), add=TRUE, inches=FALSE)
    symbols(x=X, y= -0.25, stars=cbind(11/200,11/200,11/200), add=TRUE, inches=FALSE)

    X <- -1.95
    text(X,1,'GU: GC ?',pos=4)
    text(X,0.75,'GU: GC 14',pos=4)
    text(X,0.5,'GU: GC 15',pos=4)
    text(X,0.25,'GU Tip Prune',pos=4)
    text(X,0,'GU: Basal Prune',pos=4)
    text(X,-0.25,'GU: Complete Prune',pos=4)


  })

})
