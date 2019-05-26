## Put comments here that give an overall description of what your functions do



## Write a short comment describing this function
## makeCacheMatrix <- function(x = matrix()) {}
##In this function the <<- operator is used to assign a value to an object in an environment that is different from the current environment
##Below are two functions that are used to create a special object that stores a matrix and cache's its inverse


## Take the matrix as an input
## i is the future inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x							          ## get the value of the matrix
  setinv <- function(inv) i <<- inv					## set the value of the invertible matrix i
  getinv <- function() i						        ## get the value of the invertible matrix i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}







## Write a short comment describing this function
## cacheSolve <- function(x, ...) {} 
## The following function calculates the inverse of the matrix created with the above function. 
## But only if the inverted matrix hasn't already been calculated. 
## If so, it gets the inverted matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverted matrix and sets the inverted matrix in the cache via the setinv function.


cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}




## Test
x <- matrix(c(1,2,3,4,5,6,7,8,21),3,3)				## 3x3 Matrix
cm <- makeCacheMatrix(x)					            ## CacheMatrix
cacheSolve(cm)
##          [,1]       [,2]        [,3]
##[1,] -1.58333333  1.1666667  0.08333333
##[2,]  0.50000000  0.0000000 -0.16666667
##[3,]  0.08333333 -0.1666667  0.08333333

