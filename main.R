shapirowilk<-function(x, Interpret=FALSE){
  datname <- deparse(substitute(x))
  
  ##Error handling
  #check if input parameter is empty
  tryCatch(
    {
      stopifnot(!missing(x))
    }, 
    error=function(e){
      message('It appears that no input data set was specified. Make sure to pass a numeric vector to the function.')
      stop(e)
    }
  )

  #check if input parameter is a numeric vector
  tryCatch(
    {
      stopifnot(is.numeric(x))
    }, 
    error=function(e){
      message(paste0('It appears that the input is not a numeric vector but instead of class ',class(x)))
      stop(e)
    }
  )
  
  #check if NAs are present and filter
  tryCatch(
    {
      stopifnot(!anyNA(x))
    }, 
    error=function(e){
      warning(paste0('It appears that the input vector contains NA values. They will be filterd out.',"\n",e))
    }
  )
  
  #check if inf are present and filter
  tryCatch(
    {
      stopifnot(all(is.finite(na.omit(x))))
    }, 
    error=function(e){
      warning(paste0('It appears that the input vector contains INF values. They will be filterd out.',"\n",e))
    }
  )

  #check if vector has the right length
  tryCatch(
    {
      n<-length(x[!is.na(x) & !is.infinite(x)])
      stopifnot(n >=3 && n <=5000)
    }, 
    error=function(e){
      message('It appears that the input vector is shorter then 3 or longer than 5000. That might be due to filtering out INF and NA values. Make sure to stay within these limits.')
      stop(e)
    }
  )
 result= shapiro.test(x[!is.na(x) & !is.infinite(x)])
 if(Interpret){result$method<-"Test method: Shapiro-Wilk normality test. Explanation: The null hypothesis is that the data is normally distributed. So rejecting H0 (a p value lower than a chosen alpha level of e.g. 0.05) means that there is evidence that the data is not normally distributed. It is advisable to check the effect size of the non-normality with a Q-Q plot."}
 return(result)
}  


  
  
 