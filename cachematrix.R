## Put comments here that give an overall description of what your
## functions do

## This function caching the inverse of a matrix as matrix inversion is a costly computation.
## Please note that this function is cater for a valid square invertible matrix only.
## You may test the program by using command below : 
## testData <- makeCacheMatrix(matrix(c(1,2,3,4), 2,2)) --> testData$getInverseM() 
## You will get a NULL for first time . Try :
## cacheSolve(testData) --> testData$getInverseM()
## Play around with the methods , have fun!

## Write a short comment describing this function
## makeCacheMatrix : creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL ##Setting variable , "inverseM", to NULL .This is used as a placeholder for a future value
  
  ## set the matrix, "x", to a new matrix, "y", and resets the variable, "inverseM" , to NULL
  set <- function(y) {
    x <<- y
    inverseM <<- NULL  
  }
  
  ## return matrix value "x" 
  get <- function() x
  
  ## setting input into cache variable "inverseM" 
  setInverseM <- function(inputMatrix) inverseM <<- inputMatrix
  ## return cached value "inverseM"
  getInverseM <- function() inverseM

  list(set = set, get = get,
       setInverseM = setInverseM,
       getInverseM = getInverseM)
}

  
## Write a short comment describing this function
## cacheSolve : Computes the inverse of square invertible matrix.If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  cacheIM <- x$getInverseM() 
  ## Grab the value from function makeCacheMatrix/getInverseM If not null, return "cacheIM" value.
  if(!is.null(cacheIM))
  {
    message("getting cached data")
    return(cacheIM) 
  }
  
  data <- x$get() ## Get the matrix value "x" from makeCacheMatrix/get and assign it to "data"
  cacheIM <- solve(data) ##Computing the inverse of "data"
  x$setInverseM(cacheIM) ##Store the result into cache using function cacheIM/setInverseM
  ## Return a matrix that is the inverse of 'x'
  x 
  
}
