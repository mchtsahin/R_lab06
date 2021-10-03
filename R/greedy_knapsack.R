


greedy_knapsack<-function(x,W){

  if (length(colnames(x))!=2 || !is.numeric(W) || !all(colnames(x)==c("w","v")) || !is.data.frame(x) || !all(x[,'w']>0) || !all(x[,'w']>0) )  stop("Wrong input")


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

