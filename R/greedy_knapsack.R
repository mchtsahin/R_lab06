


#' Greedy Knapsack
#'
#'This is a greedy approach to knapsack problem. Here unit value per weight is calculated for each item.
#'After calculating unit per weight, the items with maximum values are selected in an order.
#'Total value of all selected items must be below knapsack weight \code{W}.
#'This approach does not give an exact value but returns at least 50% of the true maximum value.
#'It reduces computational complexity due to O(n*log n).
#'
#'
#' @param x Data frame with two numeric columns: weights and values
#' @param W Total weight of the knapsack
#'
#' @return
#' List with numbers of items that can be put into knapsack and approximate maximum obtained value
#'
#' @export
#'
#'
#'
#'
#'
#'
greedy_knapsack<-function(x,W){

  if (length(colnames(x))!=2 || !is.numeric(W) || !all(colnames(x)==c("w","v")) || !is.data.frame(x) || !all(x[,'w']>0) || !all(x[,'w']>0) || W<0 )  stop("Wrong input")


  w=x[,'w']
  v=x[,'v']

  ratio<-v/w

  dec_ratio<-ratio[order(ratio,decreasing=TRUE)]

  elements<-order(ratio,decreasing=TRUE)

  bag<-c()

  i=1

  while (sum(bag)<=W){


    bag<-c(bag,w[elements[i]])

    i<-i+1

  }

  answer<-list(value=round(sum(v[elements[1:(i-2)]]), digits=0), elements = elements[1:(i-2)])

  return(answer)

}

