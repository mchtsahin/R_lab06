

brute_force_knapsack<-function(x,W,parallel=FALSE){

  if (length(colnames(x))!=2 || !is.numeric(W) || !all(colnames(x)==c("w","v")) || !is.data.frame(x) || !all(x[,'w']>0) || !all(x[,'w']>0) || W<0)  stop("Wrong input")

  v = x[,'v']
  w = x[,'w']
  n=length(w)

  bin<-lapply(0:(2^n-1), function(x) head(as.integer(intToBits(x)),n))


  bin_df<-t(as.data.frame(bin))

  rownames(bin_df)<-NULL


  values<-c()
  weights<-c()

  for (i in 1:length(bin)){

    values<-c(values, sum(bin[[i]]*v))
    weights<-c(weights, sum(bin[[i]]*w))

  }

  new<-cbind(bin_df,values,weights)

  optimal<-max(new[new[,'weights']<=W,'values'])

  binary_elements<-new[new[,'values']==optimal,c(1:n)]

  answer<-list(value=round(optimal,digits=0),
               elements=c(1:n)[(binary_elements*c(1:n))!=0])

  return(answer)

}




