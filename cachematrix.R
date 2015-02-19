## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

### When a matrix 'x' is presented for the very first time in order to
### computed its inverse matrix, this information is stored so that no 
### future calculations of the inverse of 'x' are performed.

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y) {
       x <<- y
       s <<- NULL
     }
     get <- function() x
     setinv <- function(solve) s <<- solve
     getinv <- function() s
     list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

###  If the output of x$geintv is not NULL, this means that the inverse of 'x' 
###  has been previously computed, returning that computed inverse; otherwise,
###  the inverse of 'x' is computed by means of function 'solve'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     s <- x$getinv()
     if(!is.null(s)) {
       message("getting cached data")
       return(s)
     }
     data <- x$get()
     s <- solve(data)
     x$setinv(s)
     s 
}
