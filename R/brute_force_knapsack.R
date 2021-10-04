

#' Brute Force Knapsack
#'
#'This algorithm estimates how to put available items into the knapsack
#'in order to get the maximum possible value less than weight of knapsack.
#'Here brute force approach is implemented that means all possible
#' combinations of items O(2^n) are considered.
#'
#' @param x Data frame with two numeric columns: weights and values
#' @param W Total weight of the knapsack
#' @param parallel logical parameter for parallel computation
#'
#' @return
#' List with numbers of items that can be put into knapsack and maximum obtained value
#'
#' @export
#'
#'
#'
#'
#'
brute_force_knapsack<-function(x,W,parallel=FALSE){

  if (length(colnames(x))!=2 || !is.numeric(W) || !all(colnames(x)==c("w","v")) || !is.data.frame(x) || !all(x[,'w']>0) || !all(x[,'w']>0) || W<0)  stop("Wrong input")

  v = x[,'v']
  w = x[,'w']
  n=length(w)

  #define logical argument parallel to apply parLapply/lapply

  if (parallel==TRUE){


    cl<-makeCluster(4,"PSOCK")

    clusterExport(cl,"n")

    #create all possible combinations of 0 and 1 for n items using parLapply

    bin<-parLapply(cl,0:(2^n-1), function(x) head(as.integer(intToBits(x)),n))
    stopCluster(cl)
  }

  if (parallel==FALSE){

    #create all possible combinations of 0 and 1 for n items using lapply

    bin<-lapply(0:(2^n-1), function(x) head(as.integer(intToBits(x)),n))

  }

  #create a data frame of 0 and 1 combinations

  bin_df<-t(as.data.frame(bin))

  rownames(bin_df)<-NULL

  #calculate total weights and values for each combination

  values<-c()
  weights<-c()

  for (i in 1:length(bin)){

    values<-c(values, sum(bin[[i]]*v))
    weights<-c(weights, sum(bin[[i]]*w))

  }

  new<-cbind(bin_df,values,weights)

  #choose combination with maximum value given its weight is less than or equal to W of knapsack

  optimal<-max(new[new[,'weights']<=W,'values'])

  #derive items from combination

  binary_elements<-new[new[,'values']==optimal,c(1:n)]

  answer<-list(value=round(optimal,digits=0),
               elements=c(1:n)[(binary_elements*c(1:n))!=0])

  return(answer)

}




