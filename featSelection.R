featureSelection <- function(curr_list,curr_label,curr_type){
  
   selFeat <- vector()
   clusFrom <- vector()
   selFeatLab <- vector()
  
  for (i in 1:length(curr_list)) {
    
     clusterMat1  = matrix(data=NA, nrow=length(curr_list[[i]]), ncol=length(curr_list[[i]]))
     clus <- curr_list[[i]]
    
    if(curr_type == "categorical"){
      
      for (r in 1:nrow(clusterMat1)) {  
        for (c in 1:ncol(clusterMat1)) { 
            clusterMat1[r,c] = copyCramer[clus[r],clus[c]]
        }
        
      }
      
    } 
    
    if(curr_type == "numerical"){
      
      for (r in 1:nrow(clusterMat1)) {  
        for (c in 1:ncol(clusterMat1)) { 
           clusterMat1[r,c] = copyMIC[clus[r],clus[c]]
        }
        
      }
      
    } 
    
    testG1 <- graph_from_adjacency_matrix(clusterMat1, mode = "undirected", weighted = TRUE)
    eig <- eigen_centrality(testG1, directed=FALSE, weights=E(testG1)$weight)$vector
    maxEig <- which.is.max(eig)
    
     selFeat[[length(selFeat)+1]] <-  clus[maxEig]
     selFeatLab[[length(selFeatLab)+1]] <- curr_label[[i]][maxEig]
     clusFrom[[length(clusFrom)+1]] <- paste("Cluster ",i)
    
  }
  
  
  
  clustr_df <- data.frame(
    Selected_Features =  clusFrom,
    Feature_id = selFeat,
    Feature_desc = str_trunc(selFeatLab,30)
  )
  
  if(curr_type == "categorical"){
    
    .GlobalEnv$selectedcatFeat <-  selFeat
    .GlobalEnv$selectedcatFeatLab <- selFeatLab
    
    .GlobalEnv$featTable <- kable(clustr_df, caption = "Selected Categorical Features") %>%
      kable_styling("striped", full_width = F)
    
    return(.GlobalEnv$featTable)
    
  }
  
  if(curr_type == "numerical"){
    
    .GlobalEnv$selectedNumFeat <- selFeat
    .GlobalEnv$selectedNumFeatLab <- selFeatLab
    
    .GlobalEnv$featTable <- kable(clustr_df,  caption = "Selected Numerical Features") %>%
      kable_styling("striped", full_width = F)
    
    return(.GlobalEnv$featTable)
    
  }
  
}
