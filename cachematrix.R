## These are two functions that cache the inverse of a matrix.
##================================================================

## The first function, `makeCacheMatrix` creates a list containing a function to 
## 		1.  set the value of the matrix
## 		2.  get the value of the matrix
## 		3.  set the value of the invert matrix
## 		4.  get the value of the invert matrix

## 'invMtr' - a variable in the environment of the function 'makeCacheMatrix' 
## 		where an invert matrix is stored

makeCacheMatrix <- function(x = matrix())
{
  invMtr <- NULL
  set <- function(y)
  {
    x <<- y
    invMtr <<- NULL
  }
  get <- function(){x}
  setInvMatrix <- function(invMtr_val){invMtr <<- invMtr_val}
  getInvMatrix <- function(){invMtr}
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## The function 'cacheSolve' calculates the invert matrix the special matrix from the list
## created with the above function 'makeCacheMatrix'. 
## It first checks to see if the invert matrix has already been calculated. 
## If so, it `get`s the invert matrix from the cache (environments of 'makeCacheMatrix') 
## and skips the computation. Otherwise, it calculates the invert matrix of the data 
## and sets the value of the mean in the cache via the `setInvMatrix` function.

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  invMtr <- x$getInvMatrix()
  if(!is.null(invMtr))
  {
    message("getting cached data")
    return(invMtr)
  }
  data <- x$get()
  invMtr <- solve(data, ...)
  x$setInvMatrix(invMtr)
  invMtr
}