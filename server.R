
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

    # var <- 'Calypso'
    # tn <- 1

    #plantFile <- paste(var,tn,'GC_15_Detail.csv',sep='_')
    plantFile <- paste('Master_2016_Tree_',tn,'.csv',sep='')

    print(plantFile)
    csv <- read.table(plantFile,skip=2,sep=',',stringsAsFactors = F)
    header <- unlist(read.table(plantFile,skip=1,sep=',',nrows=1))
    names(csv) <- header

    Density <- csv$Density[1]
    Train <- csv$Training[1]

    #every GU is a vertex

    nVerts = nrow(csv)


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

    sizes <- rep(7,nVerts)
    #sizes[pruneTypes == 'B'] <- 11


    colrs <- rep(cbPalette[8],nVerts)
    colrs[1] <- 'black'
    gCycles <- sort(unique(csv$Growing_Cycle))
    i<-1
    for( j in gCycles){
      colrs[csv$Growing_Cycle == j] <- cbPalette[i]
      i<-i+1
    }

    g <- make_empty_graph(n=nVerts) %>%
      set_vertex_attr('color',value = colrs) %>%
      set_vertex_attr('label',value = '')


    s <- 1
    prevThings <- 1
    for(i in 2:nVerts){

      gc <- csv$Growing_Cycle[v]

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

    plot(g,vertex.shape=shapes,vertex.size=sizes,vertex.label.cex=0.8,layout=layout_as_tree(g,flip.y=F),main=paste(plantFile,Density,Train,sep='\n'))

    X <- -2
    symbols(x=X, y=1, circle=11/200, add=TRUE, inches=FALSE,bg='grey')
    cbN <- 1
    ypos <- 0.75
    xpos <- -1.95
    for(uGC in gCycles){
      symbols(x=X, y=ypos, circle=11/200, add=TRUE, inches=FALSE,bg=cbPalette[cbN])
      text(xpos,ypos,paste('GU: GC',uGC),pos=4)
      cbN <- cbN + 1
      ypos <- ypos - 0.25
    }

    text(xpos,1,'GU: GC ?',pos=4)
    ypos <- ypos - 0.25
    symbols(x=X, y=ypos, squares = 22/200, add=TRUE, inches=FALSE)
    text(xpos,ypos,'GU Tip Prune',pos=4)
    ypos <- ypos - 0.25
    symbols(x=X, y=ypos, stars=cbind(11/200,11/200,11/200,11/200), add=TRUE, inches=FALSE)
    text(xpos,ypos,'GU: Basal Prune',pos=4)
    ypos <- ypos - 0.25
    symbols(x=X, y= ypos, stars=cbind(11/200,11/200,11/200), add=TRUE, inches=FALSE)
    text(xpos,ypos,'GU: Complete Prune',pos=4)


  })

  output$topoPlotly <- renderPlotly({

    var <- input$Variety
    tn<- input$treeNum

    # var <- 'Calypso'
    # tn <- 1


    #plantFile <- paste(var,tn,'GC_15_Detail.csv',sep='_')
    plantFile <- paste('Master_2016_Tree_',tn,'.csv',sep='')

    csv <- read.table(plantFile,skip=2,sep=',',stringsAsFactors = F)
    header <- unlist(read.table(plantFile,skip=1,sep=',',nrows=1))
    names(csv) <- header

    Density <- csv$Density[1]
    Train <- csv$Training[1]

    #every GU is a vertex

    nVerts = nrow(csv)


    gu <- strsplit(csv$Gu_Code,'.',fixed=T)

    depth <- (max(nchar(csv$Gu_Code)) + 1 ) / 2  # accounts for . between numbers

    guCode <- matrix(NA,nrow=nVerts,ncol=depth)
    for(v in 1:nVerts){
      gtmp <- gu[[v]]
      guCode[v,1:length(gtmp)] <-  as.numeric(gtmp)
    }

    pruneTypes <- csv$Type_Pruned
    pruneTypes[pruneTypes == ''] <- 'U'

    sizes <- rep(7,nVerts)

    colrs <- rep(cbPalette[8],nVerts)
    colrs[1] <- 'black'
    gCycles <- sort(unique(csv$Growing_Cycle))
    i<-1
    for( j in gCycles){
      colrs[csv$Growing_Cycle == j] <- cbPalette[i]
      i<-i+1
    }

    g <- make_empty_graph(n=nVerts) %>%
      set_vertex_attr('color',value = colrs) %>%
      set_vertex_attr('label',value = '')
    s <- 1
    prevThings <- 1
    for(i in 2:nVerts){
      gc <- csv$Growing_Cycle[v]
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



    G <- g
    axis <- list(title = "", showgrid = F, showticklabels = FALSE, zeroline = F)

    L <- layout_as_tree(G,flip.y=F)

    vs <- V(G)
    es <- as.data.frame(get.edgelist(G))

    Nv <- length(vs)
    Ne <- length(es[1]$V1)


    Xn <- L[,1]
    Yn <- L[,2]

    colGC <- rep('Z',nVerts)
    for(gc in csv$Growing_Cycle){
      if(!is.na(gc)){
        colGC[csv$Growing_Cycle == gc] <- LETTERS[gc - min(csv$Growing_Cycle,na.rm=T) + 1]
      }
    }


    network <- plot_ly(x = Xn, y = Yn, mode = "markers", text = paste("Row:",csv$Row_Number), hoverinfo = "text", symbol = pruneTypes,
      symbols = c('square','cross','triangle-right','triangle-down','circle'), colors=cbPalette,
      marker=list(size=10,col=colrs),
      type='scatter', showlegend = F)

    edge_shapes <- list()
    for(i in 1:Ne) {
      v0 <- es[i,]$V1
      v1 <- es[i,]$V2

      edge_shape = list(
        type = "line",
        line = list(color = colrs[i], width = 0.5),
        x0 = Xn[v0],
        y0 = Yn[v0],
        x1 = Xn[v1],
        y1 = Yn[v1]
      )

      edge_shapes[[i]] <- edge_shape
    }

    p <- layout(
      network,
      title = paste(var,tn,Density,Train),
      shapes = edge_shapes,
      xaxis = axis,
      yaxis = axis

    )

  })

  output$legend <- renderPlot({
    plot(c(0,10),c(0,10),type='n',axes=F,xlab=NA,ylab=NA)
    X <- 0
    circRad <- 0.3

    cbN <- 1
    ypos <- 10
    xpos <- .3

    var <- input$Variety
    tn<- input$treeNum

    # var <- 'Calypso'
    # tn <- 1
    plantFile <- paste('Master_2016_Tree_',tn,'.csv',sep='')

    csv <- read.table(plantFile,skip=2,sep=',',stringsAsFactors = F)
    header <- unlist(read.table(plantFile,skip=1,sep=',',nrows=1))
    names(csv) <- header
    gCycles <- sort(unique(csv$Growing_Cycle))

    for(uGC in gCycles){
      symbols(x=X, y=ypos, circle=circRad, add=TRUE, inches=FALSE,bg=cbPalette[cbN])
      text(xpos,ypos,paste('GU: GC',uGC),pos=4)
      cbN <- cbN + 1
      ypos <- ypos - 1
    }
    symbols(x=X, y=ypos, circle=circRad, add=TRUE, inches=FALSE,bg='grey')
    text(xpos,ypos,'GU: GC ?',pos=4)

    ypos <- ypos - 1.5
    points(x=X, y=ypos, pch=-9658) # see http://www.alanwood.net/demos/wgl4.html
    text(xpos,ypos,'Mid-Prune',pos=4)

    ypos <- ypos - 1
    points(x=X, y=ypos, pch=-9660)
    text(xpos,ypos,'Tip Prune',pos=4)

    ypos <- ypos - 1
    points(x=X, y=ypos, pch=-9632)
    text(xpos,ypos,'Basal Prune',pos=4)

    ypos <- ypos - 1
    points(x=X, y=ypos, pch='+',cex=2)
    text(xpos,ypos,'Complete Prune',pos=4)
  })

})
