source("dataSeg.R")


summData <- function(dat){
  
  
  return(summary(dat))
}

make.affinity <- function(S, n.neighboors) {
  N <- length(S[,1])
  
  if (n.neighboors >= N) {  # fully connected
    A <- S
  } else {
    A <- matrix(rep(0,N^2), ncol=N)
    for(i in 1:N) { # for each line
      # only connect to those points with larger similarity 
      best.similarities <- sort(S[i,], decreasing=TRUE)[1:n.neighboors]
      for (s in best.similarities) {
        j <- which(S[i,] == s)
        A[i,j] <- S[i,j]
        A[j,i] <- S[i,j] # to make an undirected graph, ie, the matrix becomes symmetric
      }
    }
  }
  A  
}




cliqueCoverTest <- function(){
  
  if(length(.GlobalEnv$remain_dim) >1){
    
    currCram <- .GlobalEnv$result[ .GlobalEnv$remain_dim , .GlobalEnv$remain_dim ]
    currK <- ceiling(sqrt(ncol(currCram)))
    
    .GlobalEnv$affMat <- make.affinity(currCram, currK)
    #.GlobalEnv$affMat <- make.affinity(copyCramer, 4)
    
    .GlobalEnv$aff_Mat_List[[length(.GlobalEnv$aff_Mat_List)+1]] <- .GlobalEnv$affMat
    
   
    
    if(dim(.GlobalEnv$affMat)[1] > 1){
      
      get_graph <- generate_graph(.GlobalEnv$affMat)
      .GlobalEnv$get_cliq <- max_cliques(get_graph, min=NULL, max=NULL)
      .GlobalEnv$count_cliques <- count_max_cliques(get_graph, min=NULL, max=NULL)
      
      initialize()
     
       edge_weights_of_cliqs <- find_weights(affMat)
       edge_weights_of_cliqs <- edge_weights_of_cliqs[!is.na(edge_weights_of_cliqs)]
       max_cluster_pos <- which.is.max(edge_weights_of_cliqs)
      
      #if(length(.GlobalEnv$total_dim) == length(.GlobalEnv$remain_dim)){
      
       get_cluster <- .GlobalEnv$get_cliq[[max_cluster_pos]]
       .GlobalEnv$ger_cluster_node_list[[length(.GlobalEnv$ger_cluster_node_list)+1]] <- get_cluster
       get_cluster_node <- sapply(get_cluster, function(x) .GlobalEnv$remain_dim[x])
      .GlobalEnv$cluster_res[[length(.GlobalEnv$cluster_res)+1]] <- get_cluster_node
      .GlobalEnv$cluster_labels[[length(.GlobalEnv$cluster_labels)+1]] <- all_labels[get_cluster_node]
      .GlobalEnv$remain_dim <- sort(.GlobalEnv$remain_dim [! .GlobalEnv$remain_dim %in% get_cluster_node])
      cliqueCoverTest()
      
      
    }
    
  }  
  else if(length(.GlobalEnv$remain_dim) == 1){
    .GlobalEnv$cluster_res[[length(.GlobalEnv$cluster_res)+1]] <- .GlobalEnv$remain_dim
    .GlobalEnv$cluster_labels[[length(.GlobalEnv$cluster_labels)+1]] <- all_labels[.GlobalEnv$remain_dim]
    #print("Get the results")
  }
  
  .GlobalEnv$cluster_results <- list(.GlobalEnv$cluster_res,.GlobalEnv$cluster_labels)
  
  
  
}




generate_graph <- function(affMat){
  g <- graph_from_adjacency_matrix(affMat, mode = "undirected", weighted = TRUE)
  g
  
}


initialize <- function(){
  .GlobalEnv$Counter <- 0
  .GlobalEnv$wt_sum <- vector()
  .GlobalEnv$Size <- 1
}




find_weights <- function(mat){
  
  for ( c in c(1: .GlobalEnv$count_cliques)) {
    find_each_weight(c, .GlobalEnv$get_cliq[c],mat)
  }
  
  wt_sum    
}



find_each_weight <- function(c,v,mat){
  
  cliq <- .GlobalEnv$get_cliq[[c]]
  #print(cliq)
  if(length(cliq)>1){
  vertex_mat <- combn(cliq, 2)
  transposeVer <- t(vertex_mat) 
  cliqSum <- sum(mat[transposeVer])
  }
  
  if( .GlobalEnv$Counter == .GlobalEnv$Size )
  {
    length(.GlobalEnv$wt_sum) <- .GlobalEnv$Size <- .GlobalEnv$Size * 2
  }
  
  .GlobalEnv$Counter <- .GlobalEnv$Counter + 1
  if(length(cliq)>1)
     .GlobalEnv$wt_sum[[.GlobalEnv$Counter]] <- cliqSum
  else
    .GlobalEnv$wt_sum[[.GlobalEnv$Counter]] <- 0
    
 
  
  
}

testClusTable <- function(id, descrip){
  
  print(id)
  
  
}








