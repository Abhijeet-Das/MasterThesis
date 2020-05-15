showIndividualClusters <- function(i,currMat,currList,currLab){
  
  if(i > length(currList)){
   #  print(i)
   #  emptyNodes <- data.frame(id = 1, color = "white")
   # .GlobalEnv$currClusEmpty <- visNetwork(emptyNodes)
   #  return(.GlobalEnv$currClusEmpty)
    return(NULL)
    
  }
  
  else{
    
    
    copy <- currMat
    
    clusterMat1  = matrix(data=NA, nrow=length(currList[[i]]), ncol=length(currList[[i]]))
    
    clus <- currList[[i]]
    
    
    
    for (r in 1:nrow(clusterMat1)) {  
      for (c in 1:ncol(clusterMat1)) { 
        clusterMat1[r,c] = currMat[clus[r],clus[c]]
      }
      
    }
    
    nodeLen <- dim(copy)[1]
    
    allColor = c("#f26430", "#e5b192", "#fccc94", "#e26cd1","#df18f9", "#4205bc",
              "#1b6891", "#99321e", "#9284ff", "#3ac2c4" ,"#732cd6", "#8dfc0f", 
              "#ba521a" ,"#e9ed78" ,"#0ffc0f" ,"#f49fa4" ,"#db786b" )
    
    # set.seed(101)
    # allShape = c("square", "triangle",  "dot","diamond")
    # shape <- sample(allShape, 26, replace=TRUE)
    # 
    # allColor = c("red", "grey", "orange", "darkblue", "purple")
    # color <- sample(allColor, 26, replace=TRUE)
    # 
    
    # currGrp <- vector()
    # currShp <- vector()
    # currColr <- vector()
    # 
    # currGrp <- c(rep("a",nodeLen))
    # currShp <- c(rep("square",nodeLen))
    # currColr <- c(rep("darkred",nodeLen))
    # 
    # for ( c in c(2: length(currList))) {
    #   
    #   currGrp[currList[[c]]] <- letters[c]
    #   currShp[currList[[c]]] <- shape[c]
    #   currColr[currList[[c]]] <- color[c]
    #   
    #   
    # }
    
    
    if(length(currList[[i]]) >1){
    
    diag(clusterMat1) <- 0 
    currGraph <- graph_from_adjacency_matrix(clusterMat1, mode = "undirected", weighted = TRUE)
    currEdge <- cbind( get.edgelist(currGraph) , E(currGraph)$weight)
    
    currNodes <- data.frame(id = 1:length(clus),
                                       label = abbreviate(currLab[[i]], minlength= 8),#paste("Node", clus),
                                       title = paste0("<p>", currLab[[i]],"</p>"),
                                       color = allColor[i],
                                       size = 12,
                                       font.size = 15)
                                      
    
    currEdges <- data.frame(from = currEdge[,1], to = currEdge[,2],
                                       #label = (round(as.numeric(currEdge[,3]), digits =3)),
                                       value = currEdge[,3],
                                       title = (round(as.numeric(currEdge[,3]), digits =3)),
                                       font.size = 5,
                                       color = "green"
                                       #font.color =c ("red", "blue"),
                                       #font.size = c(10,20))
    )
    
    # 
    # legendDescClus <- vector()
    # 
    # for (j in 1:length(clus)) {
    #   
    #   #shrtName <- substr(currLab[[i]][j], 1, 20)
    #   shrtName <- abbreviate(currLab[[i]][j], minlength = 25)
    #   
    #   legendDescClus[j]  <- paste(as.character(currList[[i]][j]) ," ",  shrtName)
    #   
    # }
    # 
    # 
    # currlnodes <- data.frame(label =   legendDescClus,
    #                                     shape = currShp[currList[[i]][1]], color = currColr[currList[[i]][1]],
    #                          title = paste0("<p>", currLab[[i]],"</p>"),           
    #                          id = 1:length(currList[[i]]),
    #                                     size = 8,
    #                                     font.size = 15)
    
    #print(currlnodes)
    #options(shiny.sanitize.errors = TRUE)
    
    .GlobalEnv$currClus <- visNetwork(currNodes, currEdges, width = "600px")%>%
      #visLegend(addNodes = currlnodes, useGroups = FALSE)%>%
      visIgraphLayout() #physics = TRUE, smooth = TRUE)
    
    return (.GlobalEnv$currClus)
    
    
    }
    
    else{
      
      
      
      shrtName <- abbreviate(currLab[[i]][1], minlength = 20)
      
      currNodes <- data.frame(id = 1,
                              label = paste(shrtName),
                              color = allColor[i],
                              size = 10,
                              font.size = 12)
      
      
      
      .GlobalEnv$currClus <- visNetwork(currNodes) #%>%
        # visLegend(addNodes = currlnodes, useGroups = FALSE)%>%
        # visIgraphLayout(physics = TRUE, smooth = TRUE)
      
      return (.GlobalEnv$currClus)
      
    }
    
    
  }  
  
  
}




