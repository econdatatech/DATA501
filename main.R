shapirowilk<-function(x, Interpret=FALSE){
  datname <- deparse(substitute(x))
  
  ##Error handling
  #check if input parameter is empty
  tryCatch(
    {
      stopifnot(!missing(x))
    }, 
    error=function(e){
      message('It appears that no input data set was specified. 
              Make sure to pass a numeric vector to the function.')
      stop(e)
    }
  )

  #check if input parameter is a numeric vector
  tryCatch(
    {
      stopifnot(is.numeric(x))
    }, 
    error=function(e){
      message(paste0('It appears that the input is not a numeric vector 
                     but instead of class ',class(x)))
      stop(e)
    }
  )
  
  #check if NAs are present and filter
  tryCatch(
    {
      stopifnot(!anyNA(x))
    }, 
    error=function(e){
      warning(paste0('It appears that the input vector contains NA values. 
                     They will be filterd out.',"\n",e))
    }
  )
  
  #check if inf are present and filter
  tryCatch(
    {
      stopifnot(all(is.finite(na.omit(x))))
    }, 
    error=function(e){
      warning(paste0('It appears that the input vector contains INF values. 
                     They will be filterd out.',"\n",e))
    }
  )

  x <- sort(x[!is.na(x) & !is.infinite(x)])
  
  n=length(x)
  #some more error handling based on shapiro.test.R in stats package
  rng <- x[n] - x[1L]
  if(rng == 0) stop("all 'x' values are identical")
  if(rng < 1e-10) x <- x/rng # rescale to avoid ifault=6 with single version.

  #number crunching starts
  #based on https://www.tandfonline.com/doi/abs/10.1080/02664769723828
  
  n <- length(x)
  
  # Initialize variable vectors
  mi <- numeric(n)
  phii <- numeric(n) 
  tailterm <- numeric(n+2)
  aistar <- numeric(n)
  ai <- numeric(n)
  
  # Compute mi, fi, and tailterm of equation 19 in 
  # https://www.tandfonline.com/doi/abs/10.1080/02664769723828
  tailterm[1] <- 0.0
  tailterm[n+2] <- 0.0
  for (i in 1:n) { 
    #equation 13 and 14 
    mi[i] <- qnorm(i / (n + 1), mean = 0.0, sd = 1.0, lower.tail = T, log.p = F)
    #prep work for equation (19)
    phii[i] <- dnorm(mi[i], mean = 0.0, sd = 1.0, log = FALSE)
    #prep work for equation (19) (tail part of it)
    tailterm[i+1] <-  mi[i] *  phii[i]
  }
  
  # Compute aistar equation 19
  for (i in 1:n) {
    #final calculation of equation 19
    aistar[i] <- -((n + 1) * (n + 2)) * phii[i] * (tailterm[i]   
                                                 - 2 * tailterm[i+1] + tailterm[i+2])
  }
  # Compute ai according to equation 17
  ai <- aistar / as.vector(sqrt(aistar %*% aistar))
  # Sort x (c.f. section above equation 1)

  # Compute meanX
  meanX <- mean(x)
  
  # Compute equation 3
  S <- sum((x - meanX)^2)
  
  # Compute W
  W <- as.vector((ai %*% x))^2 / S
  
  # Return the test statistic W
  result<-W
  
  if(Interpret){
  result<-paste0("The test statistic W of the Shapiro-Wilk test has the following value: ",W)
 }
  return(result)
}