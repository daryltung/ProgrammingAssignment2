## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get<- function() x    ##function to get matrix x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <-function() inv     ##function to get value of inverse 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {    ##computes the inverse of the special "matrix" returned by makeCacheMatrix 
        inv <- x$getinverse()   ##gets inverse of x
        if(!is.null(inv)){      ##check if inverse is null
                message("getting cached data")
                return(inv)     ##return inverse 
        }
        mat <- x$get()
        inv <- solve(mat, ...)   ##calculate inverse value
        x$setinverse(inv)
        inv
}
