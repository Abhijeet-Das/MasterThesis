showCompFeatGraph <- function(currMat, featGrp){
  
  #copy <- copyCramer
  copy <- currMat
  colnames(copy) <- NULL
  rownames(copy) <- NULL
  diag(copy) <- 0 
  
  currGraph <- graph_from_adjacency_matrix(copy, mode = "undirected", weighted = TRUE)
  currEdge <- cbind( get.edgelist(currGraph) , E(currGraph)$weight)
  nodeLen <- dim(copy)[1]
  #nodeNames <- colnames(finalCatCols)
  nodeNames <- colnames(featGrp)
  
  currNodes <- data.frame(id = 1:nodeLen,
                          label = abbreviate(nodeNames, minlength= 8), #paste("Node", 1:nodeLen),
                          color = "blue",
                          color.border = "green",
                          title = nodeNames,
                          font.size = 20)
  
  currEdges <- data.frame(from = currEdge[,1], to = currEdge[,2],
                          value = currEdge[,3],
                          color = "grey"
                          
  )
  
  
  
  .GlobalEnv$currCompGraph <- visNetwork(currNodes, currEdges, height = "500px", width = "100%") %>%
    
    visIgraphLayout()
  
  
  # visNetwork(currNodes, currEdges, height = "500px", width = "100%") %>%
  #   
  #   visIgraphLayout()
  
  # .GlobalEnv$currCluster <- visNetwork(currNodes, currEdges)%>%
  #   visLegend(addNodes = currlnodes, useGroups = FALSE)%>%
  #   visIgraphLayout()
  
  return(.GlobalEnv$currCompGraph)
  
  
  
  
}


