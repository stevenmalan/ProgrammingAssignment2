## makeCacheMatrix - creates a matrix and can save its inverse
## cacheSolve - finds and returns inverse


## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
      ## first say that the inverse is nothing
      inv <- NULL
      
      ## now, if i want to pass a new matrix, I need to start inv over again
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      ## "get" will tell you what the current matrix is
      get <- function() x
      
      ## setinv will set inv how it is told (when called)
      
      setinv <- function(inverse) inv <<- inverse
      
      ##getinv will tell you what the current inverse is.
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## 
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached inverse")
            return(m)
      }
      
      ## data loads the current matrix into m
      data <- x$get()
      
      ## now find the inverse 
      i <- solve(data, ...)
      
      ## now store this newly found inverse
      x$setinv(i)
      
      ## lastly, return the inverse
      ## it is stored as "inv,", which works due to lexical scoping
      i
}
