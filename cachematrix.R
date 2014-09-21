## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Vladimir Stozhkov's comments: 
## I prefer to use a capital 'X' instead of 'x' because
## capital letters are associated with matrices whereas lowercase letters correspond to vectors.
## invM is a cached inverse matrix.
## Function 'set' copies an input matrix into 'X'.
## Function 'get' outputs a stored matrix from cache.
## Function 'setInvMatrix' computes an inverse matrix of a stored 'X'.
## Function 'getInvMatrix' shows an inverse matrix stored in cache.

makeCacheMatrix <- function(X = matrix()) 
{
    invM <- NULL
    set <- function(Y) 
    {
        X <<- Y
        invM <<- NULL
    }
    get <- function() X
    setInvMatrix <- function(invMatrix) invM <<- invMatrix
    getInvMatrix <- function() invM
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix) 
}


## Write a short comment describing this function
## Vladimir Stozhkov's comments:
## Function 'cacheSolve' first checks if there is an inverse matrix of the object 'X' stored in cache.
## If there is not the function calls a standard function 'solve' to calculate it.

cacheSolve <- function(X, ...) 
{
    ## Return a matrix that is the inverse of 'X'
    invM <- X$getInvMatrix()
    if(!is.null(invM))
    {
        message("getting a cached inverse matrix")
        return(invM)
    }
    data <- X$get()
    invM <- solve(data, ...)
    X$setInvMatrix(invM)
    invM
}
