library(shiny)
library(shinydashboard)
library(dplyr)
library("MASS")
library(lsr)
library("rapportools")
library("lubridate")
library("DescTools")
library("sfsmisc")
library("base")
library("pracma")
library("tidyverse")
library("dplyr")
library("janitor")
library("missRanger")
library(minerva)
library(qgraph)
library(igraph)
library(sjPlot)
library(RSSL)
library(htmlTable)
library(magrittr)
library(knitr)
library(kableExtra)
library(stringr)
library(parallel)
library(visNetwork)
library(igraph)
library(nnet)
library(parcoords)
library(pairsD3)
library(d3heatmap)
library(Hmisc)

library(randomcoloR)

library(formattable)

#setwd("D:/Data viz final/shinyDashExample")

source("global.R")
source("individualCluster.R")
# source("cyclicBarPlots.R")
# source("showClusters.R")
source("barPlots.R")
source("featSelection.R")
source("clusterGraph.R")
source("compGraph.R")
source("RepGraph.R")
source("clusterTable.R")
#source("dataSeg.R")
options(shiny.sanitize.errors = TRUE)

catCols <- reactiveValues(df_data_cat = NULL)
numCols <- reactiveValues(df_data_num = NULL)

resultCramer <- reactiveValues(df_cramer = NULL)
resultMIC <- reactiveValues(df_mic = NULL)

clusterListCat <- reactiveValues(df_clus_list_cat = NULL)
clusterLabelCat<- reactiveValues(df_clus_label_cat = NULL)
clusterTableCat<- reactiveValues(df_clus_table_cat = NULL)

clusterListNum <- reactiveValues(df_clus_list_num = NULL)
clusterLabelNum<- reactiveValues(df_clus_label_num = NULL)
clusterTableNum<- reactiveValues(df_clus_table_num = NULL)

selectedFeatCat<- reactiveValues(df_sel_feat_cat = NULL)

selectedFeatNum<- reactiveValues(df_sel_feat_num = NULL)

summry <- reactiveValues(df_data_summ = NULL)

shinyServer(function(input,output){
  
  values <- reactiveValues(df_data = NULL)
  currData <- reactiveValues(df_currData = NULL)
  
  observeEvent(input$file1, {
    values$df_data <- read.csv(input$file1$datapath, header=input$header, nrows=as.numeric(input$Nrow))
    currData$df_currData <- read.csv(input$file1$datapath, header=input$header, na.strings=c("","NA"))
   # resultCramer <- reactiveValues(df_cramer = NULL)
    #resultMIC <- reactiveValues(df_mic = NULL)
  })
  
  mydata <- reactive({
    
    inFile <- input$file1
    
    #if (is.null(inFile))
    # shinyalert("Oops!", "Please a file to show data", type = "error")
    
    #testRow <- as.numeric(input$Nrow)
    #print(testRow)
    
    .GlobalEnv$tbl <- read.csv(inFile$datapath, header=input$header, nrows=as.numeric(input$Nrow))
    
    
    #formattable(i1)
    return(formattable(.GlobalEnv$tbl))
    
    
    
    #return(.GlobalEnv$emp.data)
  })
  
  
  getCatClusterList <- function()({
    
    .GlobalEnv$catClusListDrop <- vector()
    
    for ( c in c(1: length(catClusList))) {
      .GlobalEnv$catClusListDrop[[length(.GlobalEnv$catClusListDrop)+1]] <-  paste("Cluster",c)
      
    }
    
    
  })
  
  
  
  getNumClusterList <- function()({
    
    .GlobalEnv$numClusListDrop <- vector()
    
    for ( c in c(1: length(numClusList))) {
      .GlobalEnv$numClusListDrop[[length(.GlobalEnv$numClusListDrop)+1]] <-  paste("Cluster",c)
      
    }
    
    
  })
  
  
  
  
  currFeaturesScatter <- reactive({
    
    .GlobalEnv$myFeat <- vector()
    
    Feat1 <- input$state1
    .GlobalEnv$myFeat[[length(.GlobalEnv$myFeat)+1]] <- Feat1
    
    Feat2 <- input$state2
    .GlobalEnv$myFeat[[length(.GlobalEnv$myFeat)+1]] <- Feat2
    
    
    return(.GlobalEnv$myFeat)
    
    
  })
  
  selectedCatCluster <- reactive({
    
    #.GlobalEnv$myClus <- vector()
    
    .GlobalEnv$myClus <- input$catCluster
    #.GlobalEnv$myClus[[length(.GlobalEnv$myClus)+1]] <- clus
    
    
    
    return(.GlobalEnv$myClus)
    
    
  })
  
  
  selectedNumCluster <- reactive({
    
    #.GlobalEnv$myClus <- vector()
    
    .GlobalEnv$myClus <- input$numCluster
   # .GlobalEnv$myClus[[length(.GlobalEnv$myClus)+1]] <- clus
    
    
    
    return(.GlobalEnv$myClus)
    
    
  })
  

  currFeaturesBar <- reactive({
    
    .GlobalEnv$myBarFeat <- vector()
    
    Feat1 <- input$bar1
    .GlobalEnv$myBarFeat[[length(.GlobalEnv$myBarFeat)+1]] <- Feat1
    
    Feat2 <- input$bar2
    .GlobalEnv$myBarFeat[[length(.GlobalEnv$myBarFeat)+1]] <- Feat2
    
    Feat3 <- input$bar3
    .GlobalEnv$myBarFeat[[length(.GlobalEnv$myBarFeat)+1]] <- Feat3
    
    Feat4 <- input$radio
    .GlobalEnv$myBarFeat[[length(.GlobalEnv$myBarFeat)+1]] <- Feat4
    
    
    print(.GlobalEnv$myBarFeat)
    return(.GlobalEnv$myBarFeat)
    
    
  })
  
  
  
  currFeatures <- reactive({
    
    
    .GlobalEnv$my3DFeat <- vector()
    
     Feat1 <- input$scatter1
    .GlobalEnv$my3DFeat[[length(.GlobalEnv$my3DFeat)+1]] <- Feat1
    
     Feat2 <- input$scatter2
    .GlobalEnv$my3DFeat[[length(.GlobalEnv$my3DFeat)+1]] <- Feat2
    
     Feat3 <- input$scatter3
    .GlobalEnv$my3DFeat[[length(.GlobalEnv$my3DFeat)+1]] <- Feat3
    
     Feat4 <- input$scatter4
    .GlobalEnv$my3DFeat[[length(.GlobalEnv$my3DFeat)+1]] <- Feat4
    
    
    
    
    return(.GlobalEnv$my3DFeat)
    
    
    
    #return(.GlobalEnv$emp.data)
  })
  
  
  
  # if (length(selectedNumFeat) > 1){
  #   .GlobalEnv$featList <- names(finalNumCols[selectedNumFeat])
  # }
  # else
  #   .GlobalEnv$featList <- c()
  
  #-------------------------------------------------------
  
  catClusterLabels <- reactive({
  
  output$inputCatCluster <-
    renderUI(expr = selectInput("catCluster", "Choose Cluster:",
                                list(`Clusters` = .GlobalEnv$catClusListDrop,
                                     ` ` = c(),
                                     ` ` = c()),
                                selected = "Cluster 1",
                                multiple = TRUE))
  
  })
  
  
  numClusterLabels <- reactive({
    
  output$inputNumCluster <-
    renderUI(expr = selectInput("numCluster", "Choose Cluster:",
                                list(`Clusters` = .GlobalEnv$numClusListDrop,
                                     ` ` = c(),
                                     ` ` = c()),
                                selected = "Cluster 1",
                                multiple = TRUE))
  
  })
  
  #----------------------------------------------------------
    
  scatterReactive <- reactive({
    
    output$inputScatter1 <-
      renderUI(expr = selectInput("state1", "Choose 1st Feature:",
                                  list(`Features` = .GlobalEnv$selectedNumFeatLab,
                                       ` ` = c(),
                                       ` ` = c()),
                                  multiple = FALSE))
    
    
    output$inputScatter2 <-
      renderUI(expr = selectInput("state2", "Choose 2nd Feature:",
                                  list(`Features` = .GlobalEnv$selectedNumFeatLab,
                                       ` ` = c(),
                                       ` ` = c()),
                                  multiple = FALSE))
    
  })
  
 
  
  #------------------------------------------------
  
  barReactive <- reactive({
    
    output$inputBar1 <-
      renderUI(expr = selectInput("bar1", "Choose 1st Feature:",
                                  list(`Features` = .GlobalEnv$selectedNumFeatLab,
                                       ` ` = c(),
                                       ` ` = c()),
                                  multiple = FALSE))
    
    output$inputBar2 <-
      renderUI(expr = selectInput("bar2", "Choose 2nd Feature:",
                                  list(`Features` = .GlobalEnv$selectedNumFeatLab,
                                       ` ` = c(),
                                       ` ` = c()),
                                  multiple = FALSE))
    
    
    output$inputBar3 <-
      renderUI(expr = selectInput("bar3", "Choose 3rd Feature:",
                                  list(`Features` = .GlobalEnv$selectedNumFeatLab,
                                       ` ` = c(),
                                       ` ` = c()),
                                  multiple = FALSE))
    
    
  })
  
  # ------------------------------------------------------------------
  
  
  scatter3DReactive <- reactive({
  
  output$input3DScatter1 <-
    renderUI(expr = selectInput("scatter1", "Choose 1st Feature:",
                                list(`Features` = .GlobalEnv$selectedNumFeatLab,
                                     ` ` = c(),
                                     ` ` = c()),
                                multiple = FALSE))
  output$input3DScatter2 <-
    renderUI(expr = selectInput("scatter2", "Choose 2nd Feature:",
                                list(`Features` = .GlobalEnv$selectedNumFeatLab,
                                     ` ` = c(),
                                     ` ` = c()),
                                multiple = FALSE))
  output$input3DScatter3 <-
    renderUI(expr = selectInput("scatter3", "Choose 3rd Feature:",
                                list(`Features` = .GlobalEnv$selectedNumFeatLab,
                                     ` ` = c(),
                                     ` ` = c()),
                                multiple = FALSE))
  output$input3DScatter4 <-
    renderUI(expr = selectInput("scatter4", "Choose 4th Feature:",
                                list(`Features` = .GlobalEnv$selectedNumFeatLab,
                                     ` ` = c(),
                                     ` ` = c()),
                                multiple = FALSE))
  
  
  
  })
  
  observeEvent(input$tabs, {
    
    
    if (input$tabs == "catCorrHMViz") {
      
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...', value = 0, {
      temp <- currData$df_currData
      
      .GlobalEnv$currData <- currData$df_currData
      
      dataSeg(temp)
      catCols$df_data_cat <- .GlobalEnv$finalCatCols
      numCols$df_data_num <- .GlobalEnv$finalNumCols
      
      
      incProgress(amount = 0.2, message = "Have Patience")
      .GlobalEnv$copyCramer <- list()
      .GlobalEnv$copyMIC <- list()
      corr()
      incProgress(amount = 0.7, message = "Have Patience")
      
      
      #output$catHeatmap <- renderD3heatmap({d3heatmap(resultCramer$df_cramer)})
      output$catHeatmap <- renderD3heatmap({d3heatmap(.GlobalEnv$copyCramer)})
      
     })
      
      
    }
    
    
    if (input$tabs == "numCorrHMViz") {
      
     # output$numHeatmap <- renderD3heatmap({d3heatmap(resultMIC$df_mic)})
      output$numHeatmap <- renderD3heatmap({d3heatmap(.GlobalEnv$copyMIC)})
      
    }
    
    
    if (input$tabs == "processData") {
      
      
      
      
                     #dataSeg(temp)
                     #catCols$df_data_cat <- .GlobalEnv$finalCatCols
                     #numCols$df_data_num <- .GlobalEnv$finalNumCols
                     
                     
                     # incProgress(amount = 0.2, message = "Have Patience")
                     # corr()
                     # incProgress(amount = 0.7, message = "Have Patience")
        print("process data")
      #if(length(.GlobalEnv$finalCatCols) != (dim(resultCramer$df_cramer)[1]) || length(.GlobalEnv$finalNumCols) != (dim(resultMIC$df_mic)[1]))
      x<-reactive({
        is.null(dim(resultCramer$df_cramer)[1])
        })
      
      if (x()) {
       print("okok") 
        
        temp <- currData$df_currData

        .GlobalEnv$currData <- currData$df_currData

        dataSeg(temp)
        catCols$df_data_cat <- .GlobalEnv$finalCatCols
        numCols$df_data_num <- .GlobalEnv$finalNumCols


        incProgress(amount = 0.2, message = "Have Patience")
        .GlobalEnv$copyCramer <- list()
        .GlobalEnv$copyMIC <- list()
        corr()
        incProgress(amount = 0.7, message = "Have Patience")
      }
      
      
                                                
                     
                     if((dim(resultCramer$df_cramer)[1]) > 2){
                       .GlobalEnv$result <- resultCramer$df_cramer
                       
                       .GlobalEnv$copyCramer <- .GlobalEnv$result
                       
                       .GlobalEnv$all_labels <- (row.names(.GlobalEnv$result))
                       .GlobalEnv$total_dim <- c(1:dim(.GlobalEnv$result)[1])
                       .GlobalEnv$remain_dim <- c(1:dim(.GlobalEnv$result)[1])
                       .GlobalEnv$cluster_res <- list()
                       .GlobalEnv$cluster_labels <- list()
                       .GlobalEnv$cluster_results <- list()
                       cliqueCoverTest()
                       clusterListCat$df_clus_list_cat <- .GlobalEnv$cluster_results[[1]]
                       clusterLabelCat$df_clus_label_cat <- .GlobalEnv$cluster_results[[2]]
                       .GlobalEnv$catClusList <- .GlobalEnv$cluster_results[[1]]
                       .GlobalEnv$catClusLab <- .GlobalEnv$cluster_results[[2]]
                       
                       clusterTableCat$df_clus_table_cat <- getClusTable("Categorical")
                     }
                     
                     if((dim(resultMIC$df_mic)[1]) > 2){
                       .GlobalEnv$result <- resultMIC$df_mic
                       
                       .GlobalEnv$copyMIC <- .GlobalEnv$result
                       
                       .GlobalEnv$all_labels <- (row.names(.GlobalEnv$result))
                       .GlobalEnv$total_dim <- c(1:dim(.GlobalEnv$result)[1])
                       .GlobalEnv$remain_dim <- c(1:dim(.GlobalEnv$result)[1])
                       .GlobalEnv$cluster_res <- list()
                       .GlobalEnv$cluster_labels <- list()
                       .GlobalEnv$cluster_results <- list()
                       cliqueCoverTest()
                       clusterListNum$df_clus_list_num <- .GlobalEnv$cluster_results[[1]]
                       clusterLabelNum$df_clus_label_num <- .GlobalEnv$cluster_results[[2]]
                       .GlobalEnv$numClusList <- .GlobalEnv$cluster_results[[1]]
                       .GlobalEnv$numClusLab <- .GlobalEnv$cluster_results[[2]]
                      
                       clusterTableNum$df_clus_table_num <- getClusTable("Numerical")
                     }
                     
                     
                    # incProgress(amount = 0.9, message = "Have Patience")
                     
                     
                     
                     
                     
                  
      
      output$process <- renderUI({
        HTML(
          #getClusTable()
          clusterTableCat$df_clus_table_cat
        )
      })
      
      output$process1 <- renderUI({
        HTML(
          clusterTableNum$df_clus_table_num
        )
      })
      
      
      
      
    }
    
    # if (input$tabs == "corrHMViz") {
    #  
    #   output$heatmap <- renderD3heatmap({d3heatmap(copyCramer)})
    #   
    # }
    
    if (input$tabs == "compCatGraph") {
      
      output$compCatGraphViz <- renderVisNetwork(
        {
          
          showCompFeatGraph(copyCramer, finalCatCols)
          
        })
    }
    
    if (input$tabs == "compNumGraph") {
      
      output$compNumGraphViz <- renderVisNetwork(
        {
          
          showCompFeatGraph(copyMIC, finalNumCols)
          
        })
    }
    
    if (input$tabs == "catClusters") {
      
      output$catClustersViz <- renderVisNetwork(
        {
          getCatClusterList()
          
          #print(catClusListDrop)
          
          catClusterLabels()
          
          currCluster<- selectedCatCluster()
          #currCluster <- parse_number(currCluster)
         
          #print(currCluster)
          showClusterGraph(copyCramer,catClusList,currCluster,"categorical")
          
        })
    }
    
    if (input$tabs == "numClusters") {
      
      output$numClustersViz <- renderVisNetwork(
        {
          
          getNumClusterList()
          print(numClusListDrop)
          
          numClusterLabels()
          
          currCluster<- selectedNumCluster()
          #currCluster <- parse_number(currCluster)
          
          print(currCluster)
          
          showClusterGraph(copyMIC,numClusList,currCluster,"numerical")
          
        })
    }
    
    if (input$tabs == "catIndividualCluster") {
      
      output$catVis1 <- renderVisNetwork(
        {
          
          showIndividualClusters(1,copyCramer,catClusList,catClusLab)
          
        })
      
      
      output$catVis2 <- renderVisNetwork(
        {
          showIndividualClusters(2,copyCramer,catClusList,catClusLab)
        })
      output$catVis3 <- renderVisNetwork(
        {
          showIndividualClusters(3,copyCramer,catClusList,catClusLab)
        })
      output$catVis4 <- renderVisNetwork(
        {
          showIndividualClusters(4,copyCramer,catClusList,catClusLab)
        })
      output$catVis5 <- renderVisNetwork(
        {
          showIndividualClusters(5,copyCramer,catClusList,catClusLab)
        })
      output$catVis6 <- renderVisNetwork(
        {
          showIndividualClusters(6,copyCramer,catClusList,catClusLab)
        })
      output$catVis7 <- renderVisNetwork(
        {
          showIndividualClusters(7,copyCramer,catClusList,catClusLab)
        })
      output$catVis8 <- renderVisNetwork(
        {
          showIndividualClusters(8,copyCramer,catClusList,catClusLab)
        })
      
      output$catVis9 <- renderVisNetwork(
        {
          showIndividualClusters(9,copyCramer,catClusList,catClusLab)
        })
      
      output$catVis10 <- renderVisNetwork(
        {
          showIndividualClusters(10,copyCramer,catClusList,catClusLab)
        })
      
     
      
    }
    
    if (input$tabs == "numIndividualCluster") {
      
      output$numVis1 <- renderVisNetwork(
        {
          
          showIndividualClusters(1,copyMIC,numClusList,numClusLab)
          
        })
      
      
      output$numVis2 <- renderVisNetwork(
        {
          showIndividualClusters(2,copyMIC,numClusList,numClusLab)
        })
      output$numVis3 <- renderVisNetwork(
        {
          showIndividualClusters(3,copyMIC,numClusList,numClusLab)
        })
      output$numVis4 <- renderVisNetwork(
        {
          showIndividualClusters(4,copyMIC,numClusList,numClusLab)
        })
      output$numVis5 <- renderVisNetwork(
        {
          showIndividualClusters(5,copyMIC,numClusList,numClusLab)
        })
      output$numVis6 <- renderVisNetwork(
        {
          showIndividualClusters(6,copyMIC,numClusList,numClusLab)
        })
      output$numVis7 <- renderVisNetwork(
        {
          showIndividualClusters(7,copyMIC,numClusList,numClusLab)
        })
      output$numVis8 <- renderVisNetwork(
        {
          showIndividualClusters(8,copyMIC,numClusList,numClusLab)
        })
      output$numVis9 <- renderVisNetwork(
        {
          showIndividualClusters(9,copyMIC,numClusList,numClusLab)
        })
      output$numVis10 <- renderVisNetwork(
        {
          showIndividualClusters(10,copyMIC,numClusList,numClusLab)
        })
      
      
    }
    
    if (input$tabs == "selectedFeat") {
      
      selectedFeatCat$df_sel_feat_cat <- featureSelection(catClusList, catClusLab,"categorical")
      
      selectedFeatNum$df_sel_feat_num <- featureSelection(numClusList, numClusLab,"numerical")
      
      output$selectedCatFeat <- renderUI({
        HTML(
          selectedFeatCat$df_sel_feat_cat
        )
      })
      
      output$selectedNumFeat <- renderUI({
        HTML(
          selectedFeatNum$df_sel_feat_num
        )
      })
    }
    
    
    if (input$tabs == "repCatGraph") {
      
      output$repCatGraphViz <- renderVisNetwork(
        {
          
          showRepFeatGraph(copyCramer, selectedcatFeat, selectedcatFeatLab)
          
        })
    }
    
    
    if (input$tabs == "repNumGraph") {
      
      output$repNumGraphViz <- renderVisNetwork(
        {
          
          showRepFeatGraph(copyMIC, selectedNumFeat, selectedNumFeatLab)
          
        })
    }
    
    if (input$tabs == "parallelViz") {
      
      if(.GlobalEnv$currID == "Aus"){
      
      currDF1 <- finalNumCols[selectedNumFeat]
      currDF2 <- finalCatCols[selectedcatFeat]
      
      currDF <- cbind(currDF2, currDF1)
      }
      else{
      
      
      currDF <- finalNumCols[selectedNumFeat]
      }
      
      
      names(currDF) <- abbreviate(names(currDF), minlength = 20)
      
      output$parcoords = renderParcoords(
        parcoords(
          
           currDF
          , rownames=F
          , brushMode="1d"
          ,reorderable = TRUE
         
        )
      )
      
      
    }
    
    if (input$tabs == "matrixScatterViz") {
      
      set.seed(42)
      if(nrow(finalNumCols) > 500)
          sampleNumColsViz <- finalNumCols[sample(nrow(finalNumCols), 200, replace=FALSE), ]
      
      else
        sampleNumColsViz <- finalNumCols
      
      #names(sampleNumColsViz) <- abbreviate(names(sampleNumColsViz), minlength = 10)
      names(sampleNumColsViz) <- substr(names(sampleNumColsViz), 1, 10)
      
      output$pairsplot = renderUI({
        pairsD3Output("pD3", width = "850px", height = "650px" )
      })
      
      output$pD3 <- renderPairsD3({
        pairsD3(sampleNumColsViz[selectedNumFeat])
    
        #pairsD3(datain()[,choices()],group=group(), subset=subset, labels = labels,
        #       theme = input$theme, opacity = input$opacity, cex = input$cex)
      })
      
      
      
    }
    
    if (input$tabs == "barPlotViz") {
      
      output$barplot <- renderPlot({
        
        showBarPlots()
        
      })
     
      
    }
    
    
    if (input$tabs == "grpBarPlotViz") {
      
      output$grpBarResult <- renderPlotly({
        
        barReactive()
        
        thisFeat<- currFeaturesBar()
        
        print(thisFeat)
        
        tempX<- NULL
        tempY<- NULL
        tempZ<- NULL
        tempW <- NULL
        
        tempX <- (thisFeat[1])
        tempY <- (thisFeat[2])
        tempZ <- (thisFeat[3])
        tempW <- (thisFeat[4])
        
        x <- list(
          title = tempX
          #titlefont = f
        )
        
        
        
        
        plot_ly(finalNumCols, x = finalNumCols[,tempX], y = finalNumCols[,tempY], type = 'bar', name = tempY) %>%
          add_trace(y = finalNumCols[,tempZ], name = tempZ) %>%
          layout(yaxis = list(title = 'Count'), 
                 xaxis = list(title = tempX),
                 barmode = tempW)
        
        
        
      })
      
      
    }
    
    
    
    if (input$tabs == "scatterViz") {
      

         
      
      
      output$scatterResult <- renderPlotly({
        
        scatterReactive()    
        
        thisFeat<- currFeaturesScatter()
        
        tempX<- NULL
        tempY<- NULL
        tempZ<- NULL
        tempW<- NULL
        
        tempX <- (thisFeat[1])
        tempY <- (thisFeat[2])
        tempZ <- (thisFeat[3])
        tempW <- (thisFeat[4])
        print(tempX)
        print(tempY)
        
      
        
        f <- list(
          family = "Courier New, monospace",
          size = 18,
          color = "#7f7f7f"
        )
        
        
        x <- list(
          title = tempX,
          titlefont = f
        )
        y <- list(
          title = tempY,
          titlefont = f
        )
        
        plot_ly(data = finalNumCols, x = finalNumCols[,tempX], y = finalNumCols[,tempY],
                marker = list(size = 10,
                              color = 'rgba(255, 182, 193, .9)',
                              line = list(color = 'rgba(152, 0, 0, .8)',
                                          width = 2))) %>%
          layout(title = 'Styled Scatter',
                 yaxis = y,
                 xaxis = x)
        
      })
      
      
    }
    
    if (input$tabs == "3DscatterViz") {
      
      output$scatter3DResult <- renderPlotly({
        
        scatter3DReactive()
        
        this3DFeat<- currFeatures()
        
       
        tempX<- NULL
        tempY<- NULL
        tempZ<- NULL
        tempW<- NULL
        
        tempX <- (this3DFeat[1])
        tempY <- (this3DFeat[2])
        tempZ <- (this3DFeat[3])
        tempW <- (this3DFeat[4])
        
        print(tempX)
        print(tempY)
        print(tempZ)
        
        
        plot_ly(finalNumCols, x = finalNumCols[,tempX], y = finalNumCols[,tempY], z = finalNumCols[,tempZ],
                marker = list(color = finalNumCols[,tempW], colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
          add_markers() %>%
          layout(scene = list(xaxis = list(title = tempX),
                              yaxis = list(title = tempY),
                              zaxis = list(title = tempZ)),
                 annotations = list(
                   x = 1.13,
                   y = 1.05,
                   text = tempW,
                   xref = '',
                   yref = '',
                   showarrow = FALSE
                 ))
        
      })
        
       
      
      
    }
    
    
     #.GlobalEnv$cutOff <- ceiling((dim(.GlobalEnv$currData)[2])/5)
    #.GlobalEnv$colIndex <- 0
    
    if (input$tabs == "showSummary") {
      
      .GlobalEnv$currData <- currData$df_currData
      
      if(is.null(dim(.GlobalEnv$currData)))
           shinyalert("Oops!", "Please select the file", type = "error")
     
      
      .GlobalEnv$cutOff <- ceiling((dim(.GlobalEnv$currData)[2])/3)
      
      output$summary1 <-  renderPrint({

        currDF <- .GlobalEnv$currData[,c(1:.GlobalEnv$cutOff)]

        summary(currDF)
      })
      
      output$summary2 <-  renderPrint({
        
        currDF <- .GlobalEnv$currData[,c(.GlobalEnv$cutOff: (.GlobalEnv$cutOff*2))]
        
        summary(currDF)
      })
      
      output$summary3 <-  renderPrint({
        
        currDF <- .GlobalEnv$currData[,c( (.GlobalEnv$cutOff*2) : (dim(.GlobalEnv$currData)[2]) ) ]
        
        summary(currDF)
      })
      

      #summary(currData[1:2])
      
      # if(is.null(dim(temp)))
      #    shinyalert("Oops!", "Something went wrong.", type = "error")
      
     # output$summary <- renderTable({
        
        #temp <- values$df_data
        #if(length(summData(temp)) == 0)
        # shinyalert("Oops!", "Something went wrong.", type = "error")
        #else
       # if(is.null(dim(values$df_data)))
          #if(isEmpty)
        #  shinyalert("Oops!", "Something went wrong.", type = "error")
        #else
         # summData(temp)
        
        
     # })
      
    }
    
     showSummary <- function(i){
       
       
       .GlobalEnv$currSumm <-summary(currData[(1:i)])
       
       return(.GlobalEnv$currSumm)
     }   
    
    
    
  }) # observe Events end
  
  
  
  output$table <- renderTable({
    
    temp <- values$df_data
    #isEmpty <- validate(need(values$df_data,message="Dataframe not found")).GlobalEnv$currData
    #print(isEmpty)
    if(is.null(dim(values$df_data)))
      #if(isEmpty)
      shinyalert("Oops!", "Please select the file", type = "error")
    else
      mydata()
    #values$df_data
  })
  
  
  
  
  #output$homeText <- #renderText({paste("Thesis Title")
    
  #})
  output$myList <- renderUI(HTML("<ul>
<li>The approach aims to perform feature clustering, selection and visualization of unsupervised high-dimensional
data.</li>
<li>A graph clustering algorithm based on the concepts of Clique Cover Theory 
is used to perform unsupervised feature clustering. </li>

<li>The Representative Features are selected from each cluster using the concepts of Eigenvector Centrality.</li>

<li>The selected features are visualized using various standard methods. </li>
</ul>
    
<p>The interactive User Interface is designed to facilitate the user to perform different operations:</p>        
  <ol>
                            
    <li> The data is displayed in the tabular format.</li>  
    <li> The Summary of all the features can be obtained.</li>  
    <li> The correlations between the features can be observed in the correlation matrix. </li> 
    <li> The features clusters and the representative features are presented in the tabular format. </li> 
    <li> The feature cluster graph, individual clusters, representative feature graph can be interactively explored.</li>
    <li> The selected features can be visualized using different plots. </li>  
  </ol>         "))
  
  
  
  
})  # shiny server end







corr <- function(){
  
  #resultCramer$df_cramer <- PairApply(catCols$df_data_cat, CramerV, symmetric = TRUE)
  resultCramer$df_cramer <- PairApply(.GlobalEnv$finalCatCols, CramerV, symmetric = TRUE)
  .GlobalEnv$copyCramer <- resultCramer$df_cramer
  
  if(length(finalNumCols) > 0){
  
  if(nrow(numCols$df_data_num) > 1400){
    set.seed(42)
    #sampleNumCols <- numCols$df_data_num[sample(nrow(numCols$df_data_num), 500, replace=FALSE), ]
    sampleNumCols <- .GlobalEnv$finalNumCols[sample(nrow(.GlobalEnv$finalNumCols), 500, replace=FALSE), ]
    
    #allMine <- PairApply(numCols$df_data_num, CramerV, symmetric = TRUE)
    allMine <- mine(sampleNumCols, n.cores= detectCores(), var.thr=0.0, use="pair", est="mic_e")
  }
  else
    #allMine <- mine(numCols$df_data_num, n.cores= detectCores() , var.thr=0.0, use="pair", est="mic_e")
    allMine <- mine(.GlobalEnv$finalNumCols, n.cores= detectCores() , var.thr=0.0, use="pair", est="mic_e")
  
  resultMIC$df_mic <- allMine$MIC
  
  .GlobalEnv$copyMIC <- resultMIC$df_mic
  
  
  }
  #resultMIC$df_mic <- allMine
  #print(resultCramer$df_cramer)
}





