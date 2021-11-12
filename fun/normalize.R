normalize<-function(x){
  n<-(x-min(x))/(max(x)-min(x))
  return(n)
}

# Example:
# c<-c(1,2,4,5,6,8,3,8)
# normalize(c)