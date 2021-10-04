

#' Knapsack Dynamic Programming Implementation
#'
#' This is a dynamic programming (DP) approach to solving knapsack problem.
#' Here a tabulation method is used. DP approach results into better performance
#'due to O(W*n) gain.
#'
#' @param x Data frame with two numeric columns: weights and values
#' @param W Total weight of the knapsack
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
knapsack_dynamic<-function(x,W){

  if (length(colnames(x))!=2 || !is.numeric(W) || !all(colnames(x)==c("w","v")) || !is.data.frame(x) || !all(x[,'w']>0) || !all(x[,'w']>0) || W<0 )  stop("Wrong input")


  v = x[,'v']
  w = x[,'w']
  n=length(w)

  #create empty matrix of zeros

  kn<-matrix(0,n+1,W+1)

  #use tabulation method to compare weight of iterated item with previous row that does not contain this item

  for (i in 2:(n+1)){

    for (c in 2:(W+1)){

      if (w[i-1]>c){

        kn[i,c]=kn[i-1,c]

      }else{

        kn[i,c]=max( v[i-1]+kn[i-1,c-w[i-1]], kn[i-1,c] )
      }
    }
  }

  #go backwards to identify selected items

  elements<-c()

  nW<-W+1

  for (j in (n+1):2){

    if (kn[j,nW] != kn[j-1,nW]){

      elements<-c(elements,j-1)
      nW<-nW-w[j-1]

    }
  }

  answer<-list(value=round(kn[n+1,W+1],digits=0),elements=elements[order(elements)])

  return(answer)

}


