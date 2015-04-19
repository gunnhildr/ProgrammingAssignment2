# This function generally creates a special "matrix" object that can cache its 
# inverse.
# More precise makecachematrix function creates a list containing functions to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inversed matrix
# get the value of the inversed matrix

makecachematrix <- function(x=matrix()) {
    z <- NULL
    
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverseMatrix <- function(invertedMatrix){
        z <<- invertedMatrix
    }
    
    getInverseMatrix <- function() {
        z
    }
    
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}

# This function computes the inverse of the special "matrix" returned by 
# makecachematrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cachesolve <- function(x,...) {
    z <- x$getInverseMatrix()
    if(!is.null(z)) {
        message("getting cached data")
        return(z)
    }
    data <- x$get()
    z <- solve(data)
    x$setInverseMatrix(z)
    z
}

###############################################################################

## Tests

# Test basic caching
# PASS if function returns message "Cached version matches solved version"
# FAIL if function returns message "Cached version does not match solved version"

testBasicCaching <- function(){
    message<-"Cached version matches solved version"
    n <- 3
    mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
    matCached <- makecachematrix(mat)
    matSolved1 <- cachesolve(matCached)
    matSolved2 <- cachesolve(matCached)
    if (!identical(matSolved1, matSolved2))
        message<-message("Cached version does not match solved version")
    message
}


# Use a time test to see if we really save time
# PASS if function returns message "Solve time is more or equal than cache time"
# FAIL if function returns message "Solve time is less than cache time"

timeTest <- function(){
    message<-"Solve time is more or equal than cache time"
    n <- 128
    mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
    matCached <- makecachematrix(mat)
    time1 <- system.time(matSolved1 <- cachesolve(matCached))
    time2 <- system.time(matSolved2 <- cachesolve(matCached))
    if (time1["user.self"] < time2["user.self"])
        message<-message("Solve time is less than cache time")
    message
}
