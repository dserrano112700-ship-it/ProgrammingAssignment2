## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL # to hold the inverse of the matrix
     
     set <- function(y) {
          x <<- y        # change the x stored in the surrounding environment
          inv <<- NULL   # clears the previously cached inverse
     }
     get <- function() x # retrieves the matrix
     setInverse <- function(inverse) {
          inv <<- inverse
     }
     getInverse <- function() inv  # retrieves the cached inverse
     
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse
          )    # The list() at the end makes those four functions available through the object.
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
       
        if(!is.null(inv)) {
             message("getting cached data")
             return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        
        inv
}


# testing
m <- matrix(c(1, 2, 3, 4), 2, 2)
x <- makeCacheMatrix(m)
x$get()
x$getInverse()

cacheSolve(x)
cacheSolve(x)
