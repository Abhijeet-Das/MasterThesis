

showClusters <- function(start,end, list , mat){
  
  #if((length(selectedcatFeat)) % 2 == 0)
  
  
  
  par(mfrow=c(2,2))
  
  #layout(matrix(c(1,2,3,4,5,6,7,8), nrow = 4, ncol = 2, byrow = TRUE))  
  
  for (i in start:end) { 
    .GlobalEnv$clus <- list[[i]]
    .GlobalEnv$clusterMat  = matrix(data=NA, nrow=length(.GlobalEnv$clus), ncol=length(.GlobalEnv$clus))
    
    for (r in 1:nrow(.GlobalEnv$clusterMat)) {  
      for (c in 1:ncol(.GlobalEnv$clusterMat)) { 
        .GlobalEnv$clusterMat[r,c] = mat[.GlobalEnv$clus[r],.GlobalEnv$clus[c]]
      }
      
    }
    
    diag(.GlobalEnv$clusterMat) <- 0
    
    
    qgraph(.GlobalEnv$clusterMat,
           mar = c(8,8,8,8),
           edge.labels=TRUE, esize = 12 , 
           #color= c("red","blue","green", "yellow","grey"), 
           color = "steelblue",
           vsize = 10,
           edge.color = "green", trans = TRUE,
           # bg = TRUE,
           overlay = TRUE,
           overlaySize = 0.8,
           edge.label.bg = "black",
           edge.label.cex = 2,
           # legend = TRUE,
           #nodeNames = c("Hp","Engine", "Fuel","Disp", "Price"),
           #legend.cex = 1,
           #legend.mode = "names",
           GLratio = 2.5,
           #edge.label.font= edgeLabelFont ,
           layout = "circular")
    
    
    
    
  }
  
}  
