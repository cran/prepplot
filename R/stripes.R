## functions stripes and prepplot
stripes <- function(stripesx=numeric(0), stripesy=numeric(0), col.stripes="grey90", usr=NULL, ...){
  ## function for drawing stripes
  ## consecutive pairs of elements of the numeric vectors 
  ##    stripesx and stripesy are made into stripes
  ## in case of odd length vectors, the last element is ignored
  ## the vectors are sorted before being processed
  stopifnot(is.numeric(stripesx))
  stopifnot(is.numeric(stripesy))
  if (is.null(usr)) usr <- par("usr")
  stopifnot(is.numeric(usr))
  stopifnot(length(usr)==4)
  
  ls <- length(stripesx)
  if (ls > 1){
    stripesx <- sort(unique(stripesx))
    ls <- length(stripesx)
    if (!ls%%2==0){
      stripesx <- stripesx[-ls]
      message("odd number of elements in stripesx, one element omitted")
    }
    hilf <- matrix(stripesx, 2, length(stripesx)%/%2)
    for (i in 1:ncol(hilf))
      rect(hilf[1,i], usr[3], hilf[2,i], usr[4], 
           col=col.stripes, border = col.stripes, ...)
  }
  ls <- length(stripesy)
  if (ls > 1){
    stripesy <- sort(unique(stripesy))
    ls <- length(stripesy)
    if (!ls%%2==0){
      stripesy <- stripesy[-ls]
      message("odd number of elements in stripesy, one element omitted")
    }
    hilf <- matrix(stripesy, 2, length(stripesy)%/%2)
    for (i in 1:ncol(hilf))
      rect(usr[1], hilf[1,i], usr[2], hilf[2,i], 
           col=col.stripes, border = col.stripes, ...)
  }
}