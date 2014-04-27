## These functions take a matrix and create the inverse. The interesting thing is once the inverse has been calculated it is pulled 
## from memory rather then recalculated the next time the functions are called.

## Write a short comment describing this function

##This function takes a matrix and fills in 4 holders of information that are required for the next function. The 4 holders are
## set, get, setinverse, getinverse
##Of interest is that the makeCacheMatrix function actually contains functions inside it. The first component of the function assigns
##NULL to m then it sets up a function that assigns values to x and m. Of interest is that this is done using <<- which assigns the values
##to a higher level (so outside the function). A list assignment also occurs which allows for $ to be used to call those components.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#This function checks to see if the inverse of this function exists by assigning x$getinverse and checking if its null.
#If its not NULL it prints that it is obtaining the cached variable and prints it, otherwise it creates the inverse itself

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

