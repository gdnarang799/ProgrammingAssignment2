# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{

    inv <- NULL
    
    ## Method to set the matrix
    set <- function(y) 
    {
            x <<- y
            inv <<- NULL
    }
    
    ## Method the get the matrix 
    get <- function() x
    
    ## Method to set the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse

    ## Method to get the inverse of the matrix
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
    
    ## Just return the inverse if its already set
    if(!is.null(inv)) 
    {
        message("getting cached data.")
        return(inv)
    }
    ## Get the matrix from our object
        data <- x$get()
    ## Calculate the inverse using matrix multiplication
        inv <- solve(data)
    ## Set the inverse to the object
        x$setinverse(inv)
    ## Return the matrix
        inv
    
}
