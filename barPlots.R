showBarPlots  <- function(){
  
  if((length(selectedcatFeat)) %% 2 == 0)
      par(mfrow=c((length(selectedcatFeat))/2,2))
  else
    par(mfrow=c(((length(selectedcatFeat))/2)+1,2))
  
  #par(mfrow=c(5,2))
  
  for (i in selectedcatFeat) { 
    feeds <- table(finalCatCols[i])
    #names(finalCatCols[i]) <- abbreviate(names(finalCatCols[i]), minlength = 5)
    barplot(feeds[order(feeds)],
            horiz = FALSE,
            las = 1,
            col= c("bisque1", "bisque2", "bisque3","bisque4"),
            border =  NA,
            xlab = substr(names(finalCatCols[i]), 1, 15),
            cex.names= 1.0,
            cex.lab=1.5)
  }
}

