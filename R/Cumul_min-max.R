#'	@title cumulmin
#'
#'	@description Given a vector v, for each element i calculate minimum of the v
#'	starting on position i until end of vector. Remove i and repeat
#'
#'	@param v a numeric vector
#'
#'	@return A vector of the same length of input vector v. Each position i contains
#'	`min(v[i:N])`
#'	@examples
#'	vec <- c(1,2,4,2,1)
#'	cumulmin(v = vec)
#'	@export
cumulmin <- function(v) {
  ## Calculate length of input vector
  N <- length(v)
  if (N==0) return()

  ## For each element i
  ## Calculate min of the vector starting on
  ## position i until end of vector
  ## Remove i and repeat
  sapply(1:N, function(i)
    min(v[i:N])
  )
}


#'	@title cumulmax
#'
#'	@description Given a vector v, for each element i calculate maximum of the v
#'	starting on position i until end of vector. Remove i and repeat
#'
#'	@param v a numeric vector
#'
#'	@return A vector of the same length of input vector v. Each position i contains
#'	`max(v[i:N])`
#'	@examples
#'	vec <- c(1,2,4,2,1)
#'	cumulmax(v = vec)
#'	@export

cumulmax <- function(v) {
  N <- length(v)
  if (N==0) return()
  ## For each element i
  ## Calculate max of the vector starting on
  ## position i until end of vector
  ## Remove i and repeat
  sapply(1:N, function(i)
    max(v[i:N])
  )
}
