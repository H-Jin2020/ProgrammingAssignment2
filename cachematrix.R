## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # Default Inverse Matrix is NULL
        set <- function(y) { # To change the matrix which we want to solve
                x <<- y
                inv <- NULL
        }
        get <- function() x # To get the matrix which we want to solve
        setinverseM <- function(inverseM) inv <- inverseM # After calculate the inverse matrix, store it by setinverseM
        getinverseM <- function() inv # To get the inverse matrix
        
        list(set=set, get=get, setinverseM=setinverseM, getinverseM=getinverseM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverseM()
        if(!is.null(inv)) {     # if inverse matrix already exist == (!is.null(inv) == TRUE)  , Then use stored inverse matrix('inv')
                message('getting cached data')
                return(inv)
        }
        data <- x$get() # the matrix which we want to solve (we want to get inverse matrix of this)
        inv <- solve(data, ...) # using solve function
        x$setinverseM(inv) # store it in cachematrix
        inv # return the inverse matrix
}
