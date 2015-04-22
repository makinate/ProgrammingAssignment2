#Coursera R programming Rprog-013
#Programming Assignment 2
#04-22-2015


#cache matrix: This function caches a matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) { 
                x <<- y      
                s <<- NULL
        }
        get <- function() x #get the value of x
        setsolve <- function(solve) s <<- solve #set value of solve
        getsolve <- function() s #get the value of the mean
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## this function solves a cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

