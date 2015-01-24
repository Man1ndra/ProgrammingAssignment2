## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This first function, makeCacheMatrix creates a matix, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {## Set x to empty matrix
        ## Set Inverse i to NULL
        i <- NULL
        set <- function(y) {
                x <<- y ## setter assigns y to x using operator <<
                i <<- NULL
        }
        get <- function() x ## getter
        
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## The following function cacheSolve calculates the inverse of the matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache
## via the setInverse function.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ## Retrieves recent inverse from cache
        i <- x$getInverse()

        ## If i is not NULL, return cached value/data of i        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i        
}
