## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #function to create and store a cached matrix and it's inverse
        inv <- NULL #sets inverse to NULL
        set <- function(y){
                #sets new matrix and inverse to NULL
                x <<- y
                inv <<- NULL
        }
        get <- function() x #returns the stored matrix
        setinverse <- function(inverse) inv <- inverse #sets the inverse matrix
        getinverse <- function() inv #returns the inverse matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #lists the available functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        #calculate the inverse of a matrix, stores it in the cache object and returns the inverse of x
        inv <- x$getinverse() #retrieves the currently cached value
        if(!is.null(inv)){
                #checks if there is a currently stored value and returns it if it exists
                message("Getting cached inverse")
                return(inv)
        }
        mat <- x$get() #gets the cached matrix
        inv <- solve(mat, ...) #calculates the inverse
        x$setinverse(inv) #stores the inverse in the cache object
        inv #returns the inverse matrix
}
