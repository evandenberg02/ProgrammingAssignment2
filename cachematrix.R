## These functions will cache a passed matrix and its inverse. The second 
## function will print the inverse matrix.

## makeCacheMatrix stores a matrix and its inverse in the cache.

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


## cacheSolve will return a matrix that is the inverse of x. If it has already
## been calculated and stored in the cache, it will print "getting cached data"
## and then display the inverse matrix. 

cacheSolve <- function(x, ...) {
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

