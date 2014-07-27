## R function that is able to cache potentially time-consuming computations
## and preserve state of an R object (matrix in this case)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x  # Returns original matrix
        setinverse <- function(inverse) m_inv <<- inverse
        getinverse <- function() m_inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


##  this function calculates the inverse of the matrix but first checks to see 
## if the inverse has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinverse()
        if(!is.null(m_inv)) {
                message("getting cached matrix")
                return(m_inv)
        }
        data <- x$get()
        m_inv <- solve(data, ...)
        x$setinverse(m_inv)
        m_inv
        
}
