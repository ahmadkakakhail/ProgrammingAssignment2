# This is the Lexical Scoping assignment as per requirement of the course "R Programming" on Coursera

# The function "makeCacheMatrix" creates a special "matrix" object that has the ability to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) {
    i <<- inv
  }
  
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# The function "cacheSolve" computes the inverse of special "matrix" created by "makeCacheMatrix"
## If the inverse already exists, it will take the inverse from the cache

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

### Checking The Code

# > m <- matrix(rnorm(4), 2, 2)
# > m
# [,1]        [,2]
# [1,]  0.8624581 -0.09034939
# [2,] -2.6891041 -0.47685152
# > madeMatrix <- makeCacheMatrix(m)
# > madeMatrix$getInverse()
# NULL
# > cacheSolve(madeMatrix)
# [,1]       [,2]
# [1,]  0.7288818 -0.1381017
# [2,] -4.1103761 -1.3182930
# > cacheSolve(madeMatrix)
# getting cached inverse
# [,1]       [,2]
# [1,]  0.7288818 -0.1381017
# [2,] -4.1103761 -1.3182930
# > madeMatrix$getInverse()
# [,1]       [,2]
# [1,]  0.7288818 -0.1381017
# [2,] -4.1103761 -1.3182930