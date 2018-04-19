## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix create a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix which is always invertible
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve computes the inverse of the specical "matrix" returned by makeCacheMatrix above
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return (m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
