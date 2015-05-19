## This piece of code has two functions
## It creates a list from a matrix. The list does not just contain the matrix but
## also it's inverse. The reason is that calculating the inverse is time consuming
## so it makes sense to do it once

# In this function the CacheMatrix is made. Initially the inverse is set to NULL
# in addition four functions are defined (the set, get, setsolve, getsolve)
# afterwards these funtions are put in the list that is returned to the calling
# envirnoment.
# the <<- sets a variable in the parent environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                z <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# This function checks if the inverse has been made already. if that is the case
# the inverse is returned to the calling envirnoment and a message is displayed.
# if not, the data is sent to the solve funtion to get the inverse and the result
# is stored in cache

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("Now you're getting the benefits")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}