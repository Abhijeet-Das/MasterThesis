library(dplyr)
library(tidyverse)

dataSeg <- function(data){
  
  identifier <- colnames(data)[1]
  print(identifier)
  # Automobile Dataset
  if(identical(identifier, "Mfr.Name")){
    
    #setwd("D:/Data viz final/segregation")
    #data <- read.csv("AutoData.csv", header=T, na.strings=c("","NA"))
    
    total_rows <- nrow(data)
    total_cols <- ncol(data)
    filtered_cols <- data #%>% select (-c(contains("Year"), contains("Date")))
    remove_all_empty_cols <- remove_empty(filtered_cols, which = c("rows", "cols"))
    remove_na_cols <- remove_all_empty_cols[, colMeans(is.na(remove_all_empty_cols)) <= .4]
    remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col) length(unique(col)) > 1)]
    #remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col) length(unique(col)) > 1 && length(unique(col)) != total_rows)]
    allColImputed <- missRanger(remove_nonUnique_cols, pmm.k = 3, seed = 75757,  num.trees = 80)
    
    print("hereee")
    
    num_cols <- select_if(allColImputed, is.numeric)
    cat_cols <- select_if(allColImputed, is.factor)
    
    all_cat_cols <- colnames(cat_cols)
    dup_desc_col <- c()
    for(i in 1:ncol(cat_cols)) {
      cat_colName <- all_cat_cols[i]
      value <- "Desc"
      if(grepl(value, cat_colName)){
        
        if( nlevels(cat_cols[,i]) == nlevels(cat_cols[,i-1])  )
          dup_desc_col <- c(dup_desc_col, i-1)
      }
      
    }
    
    cat_cols <- cat_cols[,-(dup_desc_col)] # removing abbv. Desc  columns
    
    .GlobalEnv$finalCatCols <- cat_cols
    
    .GlobalEnv$finalNumCols <- num_cols[, sapply(num_cols, function(col) length(unique(col)) == 1 ||  length(unique(col)) > 10)]
    
    #.GlobalEnv$finalNumCols <- num_cols[, sapply(num_cols, function(col) length(unique(col))) > 10]
    
    if( ncol(finalNumCols) !=  ncol(num_cols)){
      .GlobalEnv$finalCatCols  <- cbind(cat_cols,  num_cols[, sapply(num_cols, function(col) length(unique(col)) <= 10 && length(unique(col)) != 1)])  # appending 2 levels num cols 
      
    }
    
    
     # Manual segreagation .. Has to automate later
    
    
    
    
  }
  # Right Heart Catherization Dataset
  
  if(identical(identifier, "Primary.disease.category")){
    
    .GlobalEnv$currID <- "rightHeart"
   
    #setwd("D:/Data viz final/segregation")
    #data <- read.csv("Right heart catheterization.csv", header=T, na.strings=c("","NA")) 
    total_rows <- nrow(data)
    total_cols <- ncol(data)
    #filtered_cols <-  data[ , !(names(data) %in% c("Age"))]
    #filtered_cols <-  filtered_cols[ , !(names(filtered_cols) %in% c("Income"))]
    #filtered_cols <- select(data,-c("Age", "Income"))
    filtered_cols <- subset(data, select=-c(Age,Income))
    
    
    #filtered_cols <- select(data,-contains("Age")) 
    #filtered_cols <- select(filtered_cols,-contains("Income")) 
   # filtered_cols <- data %>% select (-c(("Age"),("Income"))) # May consider to include Age
    #filtered_cols <- data
    remove_all_empty_cols <- remove_empty(filtered_cols, which = c("rows", "cols"))
    remove_na_cols <- remove_all_empty_cols[, colMeans(is.na(remove_all_empty_cols)) <= .4] 
    
    #remove_na_cols <- remove_all_empty_cols[, -which(colMeans(is.na(remove_all_empty_cols)) > 0.4)]
    remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col) length(unique(col)) > 1 && length(unique(col)) != total_rows)]
    if(sum(is.na(remove_nonUnique_cols)) ==0){
      
      cat_cols <- select_if(remove_nonUnique_cols, is.factor)
      num_cols <- select_if(remove_nonUnique_cols, is.numeric)
    }else{
      # function to perform imputation
      allColImputed <- missRanger(remove_nonUnique_cols, pmm.k = 3, seed = 75757,  num.trees = 80)
    }
    
    
    
    .GlobalEnv$finalNumCols <- num_cols[, sapply(num_cols, function(col) length(unique(col))) > 2]
    
    if( ncol(finalNumCols) !=  ncol(num_cols)){
      .GlobalEnv$finalCatCols  <- cbind(cat_cols,  num_cols[, sapply(num_cols, function(col) length(unique(col))) == 2])  # appending 2 levels num cols 
      
    }
    
    #catCols$df_data_cat <- finalCatCols
    #numCols$df_data_num <- finalNumCols
    
    
    
    
    
    
  } 
  
  # Hepatitis Dataset
  if(identical(identifier, "Person.Age")){
    
    .GlobalEnv$currID <- "Hepatitis"
  
    total_rows <- nrow(data)
    total_cols <- ncol(data)
    filtered_cols <- data #subset(data, select=-c(Age,Income))
  
    remove_all_empty_cols <- remove_empty(filtered_cols, which = c("rows", "cols"))
    remove_na_cols <- remove_all_empty_cols[, colMeans(is.na(remove_all_empty_cols)) <= .4] 
    
    #remove_na_cols <- remove_all_empty_cols[, -which(colMeans(is.na(remove_all_empty_cols)) > 0.4)]
    remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col) length(unique(col)) > 1 && length(unique(col)) != total_rows)]
    if(sum(is.na(remove_nonUnique_cols)) ==0){
      
      cat_cols <- select_if(remove_nonUnique_cols, is.factor)
      num_cols <- select_if(remove_nonUnique_cols, is.numeric)
    }else{
      # function to perform imputation
      allColImputed <- missRanger(remove_nonUnique_cols, pmm.k = 3, seed = 75757,  num.trees = 80)
      
      cat_cols <- select_if(remove_nonUnique_cols, is.factor)
      num_cols <- select_if(remove_nonUnique_cols, is.numeric)
    }
    
    
    
    .GlobalEnv$finalNumCols <- num_cols[, sapply(num_cols, function(col) length(unique(col))) > 2]
    
    if( ncol(finalNumCols) !=  ncol(num_cols)){
      .GlobalEnv$finalCatCols  <- cbind(cat_cols,  num_cols[, sapply(num_cols, function(col) length(unique(col))) == 2])  # appending 2 levels num cols 
      
    }
    
  
  } 
  
  
  if(identical(identifier, "normalized-losses")){
    
    .GlobalEnv$currID <- "autoEval"
    
    
    total_rows <- nrow(data)
    total_cols <- ncol(data)
    
    filtered_cols <- subset(data, select=-c(Age,Income))
    remove_all_empty_cols <- remove_empty(filtered_cols, which = c("rows", "cols"))
    remove_na_cols <- remove_all_empty_cols[, colMeans(is.na(remove_all_empty_cols)) <= .4] 
    
    #remove_na_cols <- remove_all_empty_cols[, -which(colMeans(is.na(remove_all_empty_cols)) > 0.4)]
    remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col) length(unique(col)) > 1 && length(unique(col)) != total_rows)]
    
    if(sum(is.na(remove_nonUnique_cols)) ==0){
      
      cat_cols <- select_if(remove_nonUnique_cols, is.factor)
      num_cols <- select_if(remove_nonUnique_cols, is.numeric)
    }else{
      # function to perform imputation
      allColImputed <- missRanger(remove_nonUnique_cols, pmm.k = 3, seed = 75757,  num.trees = 80)
    }
    
    
    cat_cols <- select_if(allColImputed, is.factor)
    num_cols <- select_if(allColImputed, is.numeric)
    
    #.GlobalEnv$finalCatCols  <- num_cols
    
    .GlobalEnv$finalCatCols  <- cat_cols
    
    
    .GlobalEnv$finalNumCols <- num_cols[, sapply(num_cols, function(col) length(unique(col)) == 1 ||  length(unique(col)) > 22)]
    #.GlobalEnv$finalNumCols <- num_cols[, sapply(num_cols, function(col)  length(unique(col)) > 2)]
    
    if( ncol(finalNumCols) !=  ncol(num_cols)){
      .GlobalEnv$finalCatCols  <- cbind(cat_cols,  num_cols[, sapply(num_cols, function(col) length(unique(col)) <= 22 && length(unique(col)) != 1)])  # appending 2 levels num cols 
      
      #.GlobalEnv$finalCatCols  <- cbind(cat_cols,  num_cols[, sapply(num_cols, function(col) length(unique(col)) <= 2 )])  # appending 2 levels num cols 
      
    }
    
    
    
    
    
  } 
  
  
  
  
  # Arithheits Dataset
  if(identical(identifier, "Patient.Age")){
    
    print("Arithheits dataset")
    
    
    
    total_rows <- nrow(data)
    total_cols <- ncol(data)
    filtered_cols <- data #subset(data, select=-c(Age,Income))
    
    remove_all_empty_cols <- remove_empty(filtered_cols, which = c("rows", "cols"))
    remove_na_cols <- remove_all_empty_cols[, colMeans(is.na(remove_all_empty_cols)) <= .4] 
    
    #remove_na_cols <- remove_all_empty_cols[, -which(colMeans(is.na(remove_all_empty_cols)) > 0.4)]
    remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col) length(unique(col)) > 1)]
    
    #remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col) length(unique(col)) > 1 && length(unique(col)) != total_rows)]
    if(sum(is.na(remove_nonUnique_cols)) ==0){
      
      cat_cols <- select_if(remove_nonUnique_cols, is.factor)
      num_cols <- select_if(remove_nonUnique_cols, is.numeric)
    }else{
      # function to perform imputation
      allColImputed <- missRanger(remove_nonUnique_cols, pmm.k = 3, seed = 75757,  num.trees = 80)
      
      cat_cols <- select_if(remove_nonUnique_cols, is.factor)
      num_cols <- select_if(remove_nonUnique_cols, is.numeric)
    }
    
    
    .GlobalEnv$finalCatCols <- cat_cols
    
    
    .GlobalEnv$finalNumCols <- num_cols[, sapply(num_cols, function(col) length(unique(col))) > 10]
    
    if( ncol(finalNumCols) !=  ncol(num_cols)){
      .GlobalEnv$finalCatCols  <- cbind(cat_cols,  num_cols[, sapply(num_cols, function(col) length(unique(col))) <= 10 ])  # appending 2 levels num cols 
      
    }
    
    
  } 
  
  
  
  #Horse
  if(identical(identifier, "surgery")){
    
   
    total_rows <- nrow(data)
    total_cols <- ncol(data)
    filtered_cols <- data #subset(data, select=-c(Age,Income))
    
    remove_all_empty_cols <- remove_empty(filtered_cols, which = c("rows", "cols"))
    remove_na_cols <- remove_all_empty_cols[, colMeans(is.na(remove_all_empty_cols)) <= .4] 
    
    #remove_na_cols <- remove_all_empty_cols[, -which(colMeans(is.na(remove_all_empty_cols)) > 0.4)]
    remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col) length(unique(col)) > 1)]
    
    #remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col) length(unique(col)) > 1 && length(unique(col)) != total_rows)]
    if(sum(is.na(remove_nonUnique_cols)) ==0){
      
      cat_cols <- select_if(remove_nonUnique_cols, is.factor)
      num_cols <- select_if(remove_nonUnique_cols, is.numeric)
    }else{
      # function to perform imputation
      allColImputed <- missRanger(remove_nonUnique_cols, pmm.k = 3, seed = 75757,  num.trees = 80)
      
      cat_cols <- select_if(remove_nonUnique_cols, is.factor)
      num_cols <- select_if(remove_nonUnique_cols, is.numeric)
    }
    
    
    .GlobalEnv$finalCatCols <- cat_cols
    
    
    .GlobalEnv$finalNumCols <- num_cols[, sapply(num_cols, function(col) length(unique(col))) > 10]
    
    if( ncol(finalNumCols) !=  ncol(num_cols)){
      .GlobalEnv$finalCatCols  <- cbind(cat_cols,  num_cols[, sapply(num_cols, function(col) length(unique(col))) <= 10 ])  # appending 2 levels num cols 
      
    }
    
    
  } 
  
  
  
  
  
  if(identical(identifier, "Country") || identical(identifier, "Instance_name")){
    
    
    total_rows <- nrow(data)
    total_cols <- ncol(data)
    filtered_cols <- data #subset(data, select=-c(Age,Income))
    
    remove_all_empty_cols <- remove_empty(filtered_cols, which = c("rows", "cols"))
    remove_na_cols <- remove_all_empty_cols[, colMeans(is.na(remove_all_empty_cols)) <= .4] 
    
    #remove_na_cols <- remove_all_empty_cols[, -which(colMeans(is.na(remove_all_empty_cols)) > 0.4)]
    remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col) length(unique(col)) > 1)]
    
    #remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col) length(unique(col)) > 1 && length(unique(col)) != total_rows)]
    if(sum(is.na(remove_nonUnique_cols)) ==0){
      
      cat_cols <- select_if(remove_nonUnique_cols, is.factor)
      num_cols <- select_if(remove_nonUnique_cols, is.numeric)
    }else{
      # function to perform imputation
      allColImputed <- missRanger(remove_nonUnique_cols, pmm.k = 3, seed = 75757,  num.trees = 80)
      
      cat_cols <- select_if(remove_nonUnique_cols, is.factor)
      num_cols <- select_if(remove_nonUnique_cols, is.numeric)
    }
    
    
    .GlobalEnv$finalCatCols <- cat_cols
    
    
    .GlobalEnv$finalNumCols <- num_cols[, sapply(num_cols, function(col) length(unique(col))) > 10]
    
    if( ncol(finalNumCols) !=  ncol(num_cols)){
      .GlobalEnv$finalCatCols  <- cbind(cat_cols,  num_cols[, sapply(num_cols, function(col) length(unique(col))) <= 10 ])  # appending 2 levels num cols 
      
    }
    
    
  } 
  
  
  # Heart Data
  
  if(identical(identifier, "age")){
    .GlobalEnv$currFactor <- 6
  } 
  
  # Auto Eval Data
  
  if(identical(identifier, "normalized-losses")){
    .GlobalEnv$currFactor <- 22
  } 
  
  # Breast Cancer Eval Data
  
  if(identical(identifier, "breastid")){
    .GlobalEnv$currFactor <- 2
  } 
  
  # Aus Credit 
  if(identical(identifier, "A11")){
    .GlobalEnv$currFactor <- 9
    .GlobalEnv$currID <- "Aus"
  } 
  
  
    
    total_rows <- nrow(data)
    total_cols <- ncol(data)
    filtered_cols <- data #subset(data, select=-c(Age,Income))
    
    remove_all_empty_cols <- remove_empty(filtered_cols, which = c("rows", "cols"))
    remove_na_cols <- remove_all_empty_cols[, colMeans(is.na(remove_all_empty_cols)) <= .4] 
    
    #remove_na_cols <- remove_all_empty_cols[, -which(colMeans(is.na(remove_all_empty_cols)) > 0.4)]
    #remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col) length(unique(col)) > 1)]
    
    remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col)  length(unique(col)) != total_rows)]
    
    #remove_nonUnique_cols <- remove_na_cols[, sapply(remove_na_cols, function(col) length(unique(col)) > 1 && length(unique(col)) != total_rows)]
    if(sum(is.na(remove_nonUnique_cols)) ==0){
      
      cat_cols <- select_if(remove_nonUnique_cols, is.factor)
      num_cols <- select_if(remove_nonUnique_cols, is.numeric)
    }else{
      # function to perform imputation
      allColImputed <- missRanger(remove_nonUnique_cols, pmm.k = 3, seed = 75757,  num.trees = 80)
      
      cat_cols <- select_if(allColImputed, is.factor)
      num_cols <- select_if(allColImputed, is.numeric)
    }
    
    
    .GlobalEnv$finalCatCols <- cat_cols
    
    
    .GlobalEnv$finalNumCols <- num_cols[, sapply(num_cols, function(col) length(unique(col))) > .GlobalEnv$currFactor]
    
    if( ncol(finalNumCols) !=  ncol(num_cols)){
      .GlobalEnv$finalCatCols  <- cbind(cat_cols,  num_cols[, sapply(num_cols, function(col) length(unique(col))) <= 10 ])  # appending 2 levels num cols 
      
    }
    
    
  
  
  
  
  
  
  
}
