
## Creates a function that contains a Matrix and a Variable for the Inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL # Create a variable
        set <- function(y) { # Function "set" will save the Matrix y on a variable x
                x <<- y
                i <<- NULL
        }
        get <- function() x # Function "get" will return the Matrix in variable X
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function that verifies whether the Inverse has been calculated or not. 
## If the inverse is not cached, then the it is calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
