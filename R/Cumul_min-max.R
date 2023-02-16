cumulmin <- function(v) {
  #' Calculate length of input vector
  N <- length(v)
  if (N==0) return()
  
  #' For each element i  
  #' Calculate min of the vector starting on 
  #' position i until end of vector
  #' Remove i and repeat
  sapply(1:N, function(i)
    min(v[i:N])
  )
}

cumulmax <- function(v) {
  N <- length(v)
  if (N==0) return()
  #' For each element i  
  #' Calculate max of the vector starting on 
  #' position i until end of vector
  #' Remove i and repeat
  sapply(1:N, function(i)
    max(v[i:N])
  )
}