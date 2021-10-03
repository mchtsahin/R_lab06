

knapsack_dynamic<-function(x,W){

  if (length(colnames(x))!=2 || !is.numeric(W) || !all(colnames(x)==c("w","v")) || !is.data.frame(x) || !all(x[,'w']>0) || !all(x[,'w']>0) )  stop("Wrong input")


  v = x[,'v']
  w = x[,'w']
  n=length(w)

  kn<-matrix(0,n+1,W+1)

  for (i in 2:(n+1)){

    for (c in 2:(W+1)){

      if (w[i-1]>c){

        kn[i,c]=kn[i-1,c]

      }else{

        kn[i,c]=max( v[i-1]+kn[i-1,c-w[i-1]], kn[i-1,c] )
      }
    }
  }

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

