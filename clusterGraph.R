
showClusterGraph <- function(currMat,currList,currCluster, featGrp){
  
  
  clusterDesc <- currCluster
  currId <- parse_number(currCluster)
  
  copy <- currMat
  colnames(copy) <- NULL
  rownames(copy) <- NULL
  diag(copy) <- 0 
  
  currGraph <- graph_from_adjacency_matrix(copy, mode = "undirected", weighted = TRUE)
  currEdge <- cbind( get.edgelist(currGraph) , E(currGraph)$weight)
  nodeLen <- dim(copy)[1]
  
  if(featGrp == "categorical")
      nodeNames <- colnames(finalCatCols)

  else
    nodeNames <- colnames(finalNumCols)

  
  
  
  set.seed(101)
  #allColor <- randomColor(count = 30, hue = c(" ", "random", "red", "orange", "yellow",
   #                              "green", "blue", "purple", "pink", "monochrome"),
    #          luminosity = c(" ","random", "light", "bright", "dark"))
 # allShape = c("square", "triangle",  "dot","diamond")
#  shape <- sample(allShape, 26, replace=TRUE)
  
  color = c("#f26430", "#e5b192", "#fccc94", "#e26cd1","#df18f9", "#4205bc",
               "#1b6891", "#99321e", "#9284ff", "#3ac2c4" ,"#732cd6", "#8dfc0f", 
               "#ba521a" ,"#e9ed78" ,"#0ffc0f" ,"#f49fa4" ,"#db786b" )
  #color <- sample(allColor, 26, replace=TRUE)
  
  
 # currGrp <- vector()
  #currShp <- vector()
 # currColr <- vector()
  
  edgeColr <- vector()
  edgeTotal <- dim(currEdge)[1]
  edgeColr <- c(rep("#D3D3D3",edgeTotal))
  
  #currGrp <- c(rep("a",nodeLen))
  #currShp <- c(rep("square",nodeLen))
  currColr <- c(rep("#D3D3D3",nodeLen))
  
  currLabel <- c(rep(" ",nodeLen))
  
  
  print("Hereee")
  
  
  for (i in currId) {
    
    currColr[currList[[i]]] <- color[i]
    currLabel[currList[[i]]] <- paste("Node",currList[[i]])
    
    i <- as.numeric(i)
    testComb <- combn(currList[[i]], 2)
    transComb <- t(testComb) 
  
  setCurrEdges <- vector()
  
  for ( c in c(1: dim(transComb)[1])) {
    
    # print(transComb[c,1])
    
    set <- which((currEdge[,1]== transComb[c,1] & currEdge[,2]== transComb[c,2]))
    
    if(length(set)==0)
       set <- which((currEdge[,1]== transComb[c,2] & currEdge[,2]== transComb[c,1]))
    
    
    
    #print(set)
    setCurrEdges[[length(setCurrEdges)+1]] <-  set
    
    
    
  }
  
  
  
  if(length(currList[[i]]) > 1)
    edgeColr[setCurrEdges] <- "green"
  
  
  
  }
  
  #print(edgeColr)
  
  currNodes <- data.frame(id = 1:nodeLen,
                                     label = abbreviate(nodeNames, minlength= 8), #currLabel, #paste("Node", 1:nodeLen),
                                     color = currColr,
                                     title = nodeNames,
                                     font.size = 20)
  
  currEdges <- data.frame(from = currEdge[,1], to = currEdge[,2],
                                     value = currEdge[,3],
                                     color = edgeColr
                                     
  )
  
  
  #print(currColr)
  
  #print(length(clusterDesc))
  
  lnodes <- data.frame(label = clusterDesc,
                       shape = c( "dot"), color = color[currId] ,
                       title = "Informations", id = 1:(length(clusterDesc)))
  


  # currLegendDesc <- vector()
  # currLegendShp <- vector()
  # currLegendColr <- vector()
  # 
  # currVisNw <-visNetwork(currNodes, currEdges)
  # 
  # CurrGetGrp <- function(k,i){
  #   
  #   CurrVisNw <- k %>% visGroups(groupname = letters[i],
  #                                           shadow = list(enabled = TRUE))
  # }
  # 
  # 
  # 
  # for(i in 1:length(currList)) {
  #   
  #   CurrGetGrp(currVisNw, i)
  #   #temp <- catClusList[[i]][1]
  #   currLegendDesc[[length(currLegendDesc)+1]] <-  paste("Cluster ", i, "length ",length(currList[[i]]))
  #   currLegendShp[[length(currLegendShp)+1]] <-  currShp[currList[[i]][1]]
  #   currLegendColr[[length(currLegendColr)+1]] <- currColr[currList[[i]][1]]
  # }
  # 
  # 
  # 
  # currlnodes <- data.frame(label = currLegendDesc,
  #                                     shape = currLegendShp, color = currLegendColr,
  #                                     title = "Informations", id = 1:length(currList),
  #                                     font.size = 30)
  # 
  # 
  
  
  

  
  
  .GlobalEnv$currCluster <- visNetwork(currNodes, currEdges, height = "500px", width = "100%") %>%
    
    visLegend(addNodes = lnodes, useGroups = FALSE) %>%
    
    visIgraphLayout()
  
  
  # .GlobalEnv$currCluster <- visNetwork(currNodes, currEdges)%>%
  #   visLegend(addNodes = currlnodes, useGroups = FALSE)%>%
  #   visIgraphLayout()
  
  return(.GlobalEnv$currCluster)
  
  
  
  
}


