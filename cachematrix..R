## Matrix Inversion of long objects is a potentially time consuming computations.
## The goal is to cache the value of the inverse so that when we need it again,
## it can be looked up in the cache rather than recomputing
## makeCache matrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function (x=matrix()){
        inv <- NULL
        set <- function (y){
                x <<-y
                inv<<-NULL
        }
        get <- function() x
        setSolve <- function(solve)inv <<-solve
        getSolve <- function()inv
        list (set=set, get=get, setSolve=setSolve, getSolve=getSolve)
        }
## cacheSolve computes the inverse of a special matrix return by makeCacheMatrix
## Assuming matrix is invertible. CacheSolve retribes the inverse from the cache
## if already calculated
cacheSolve <- function (x,...){
        ## Return a matrix that is the inverse of 'X'
        inv <- x$getSolve
        if (!is.null(inv)){
                message("getting cache data")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setSolve(inv)
        inv
}
        
