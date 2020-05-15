showRepFeatGraph <- function(currMat, currList, currLab){
  
  
  copy <- currMat
  
  clusterMat1  = matrix(data=NA, nrow=length(currList), ncol=length(currList))
  
  clus <- currList
  
  
  legLabel <- vector()
  
  for ( c in c(1: length(currList))) {
    legLabel[[length(legLabel)+1]] <-  paste("Feat from  \n Clus",c)
    
  }
  
  
  for (r in 1:nrow(clusterMat1)) {  
    for (c in 1:ncol(clusterMat1)) { 
      clusterMat1[r,c] = currMat[clus[r],clus[c]]
    }
    
  }
  
  nodeLen <- dim(copy)[1]
  
  allColor = c("#f26430", "#e5b192", "#fccc94", "#e26cd1","#df18f9", "#4205bc",
               "#1b6891", "#99321e", "#9284ff", "#3ac2c4" ,"#732cd6", "#8dfc0f", 
               "#ba521a" ,"#e9ed78" ,"#0ffc0f" ,"#f49fa4" ,"#db786b" )
  
  
  
  currColor <- allColor[c(1:length(currList))]
  
  
  diag(clusterMat1) <- 0 
  currGraph <- graph_from_adjacency_matrix(clusterMat1, mode = "undirected", weighted = TRUE)
  currEdge <- cbind( get.edgelist(currGraph) , E(currGraph)$weight)
  
  currNodes <- data.frame(id = 1:length(clus),
                          label = abbreviate(currLab,minlength= 8),#paste("Node", clus),
                          title = paste0("<p>", currLab,"</p>"),
                          color = currColor,
                          size = 25,
                          font.size = 20)
  
  
  currEdges <- data.frame(from = currEdge[,1], to = currEdge[,2],
                          #label = (round(as.numeric(currEdge[,3]), digits =3)),
                          #value = currEdge[,3],
                          title = (round(as.numeric(currEdge[,3]), digits =3)),
                          #font.size = 5,
                          color = "green"
                          #font.color =c ("red", "blue"),
                          #font.size = c(10,20))
  )
  

   lnodes <- data.frame(label = legLabel,
                        shape = c( "dot"), color = currColor ,
                        width = 1,
                       # ncol = 2,
                        size = 15,
                        font.size = 12,
                        title = "Informations", id = 1:(length(currList)))
  
 
  
  
  .GlobalEnv$currRepGraph <- visNetwork(currNodes, currEdges, height = "500px", width = "100%") %>%
    visLegend(addNodes = lnodes, useGroups = FALSE) %>%
    visIgraphLayout()
  
  
  # .GlobalEnv$currCluster <- visNetwork(currNodes, currEdges)%>%
  #   visLegend(addNodes = currlnodes, useGroups = FALSE)%>%
  #   visIgraphLayout()
  
  return(.GlobalEnv$currRepGraph)
  
  
  
  
}


