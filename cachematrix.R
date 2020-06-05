## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix function creates a special "matrix" object which can cache its inverse. 
##cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix 


## Write a short comment describing this function

##The function makeCacheMAtrix creates a special "matrix", which is actually a list containing a function to do the following:
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

##The following function, cacheSolve calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}

##Example1

M<- matrix(c(5,6,7,8), nrow = 2, ncol = 2)
solve(M)
MI<- makeCacheMatrix(M)
cacheSolve(MI)

##Example2

M1<- matrix(c(100,200,300,400), nrow=2, ncol=2)
solve(M1)
MI1<- makeCacheMatrix(M1)
cacheSolve(MI1)
