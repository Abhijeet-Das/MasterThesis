getClusTable <- function(p){
  
  flatCluster <- unlist(.GlobalEnv$cluster_res)
  flatLabel <- unlist(.GlobalEnv$cluster_labels)
  flatLabel <- str_trunc(flatLabel, 30)
  
  clustr_df <- data.frame(
    id = flatCluster,
    Desc = flatLabel
  )
  
  .GlobalEnv$Ktable <- kable(clustr_df, caption = paste(p, " Cluster Results")) %>%
    kable_styling("striped", full_width = F)
  
  .GlobalEnv$pack <- 0
  
  for(i in 1:length(.GlobalEnv$cluster_res)) {
    
    getPackRow(.GlobalEnv$Ktable, i)
    
  }
  
  return(.GlobalEnv$Ktable)
  
}



getPackRow <- function(k,i){
  .GlobalEnv$Ktable <- k %>% pack_rows( paste("Cluster", as.character(i)), .GlobalEnv$pack + 1, .GlobalEnv$pack + length(.GlobalEnv$cluster_res[[i]]) )
  
  .GlobalEnv$pack <- .GlobalEnv$pack + length(.GlobalEnv$cluster_res[[i]])
}


