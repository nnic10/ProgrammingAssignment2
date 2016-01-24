makeCacheMatrix <- function(x = matrix()) {
        # return a list of get/set functions for original and inverse
        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m_inv <<- inv
        getinv <- function() m_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
        # return the inverse of matrix x (from cache if exist)
        m_inv <- x$getinv()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        data <- x$get()
        m_inv <- solve(data, ...)
        x$setinv(m_inv)
        m_inv
}
