
## The objective of these functions is to minimise the computational cost of repeatedly calcuating the inverse
## of a matrix by creating a means of caching the inverse.

## makeCacheMatrix is a function which creates a special list of functions that can 
## 1.Set the value of the matrix
## 2.Get the value of the matrix
## 3.Set the Value of the Matrix Inverse
## 4.Get the value of the Matrix Inverse
## The overall aim is to use this function to assist in caching the inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cachSolve is a function which caches the inverse of a matrix. It first checks to see if the inverse 
## has already been calculated or not. If so the function will obtain the inverse from the list created 
## by the makeCacheMatrix function, display a message notifying the user that it is getting the cached data 
## and return this value to the user. Otherwise it will caclulate the inverse of the matrix, cache 
## and display the value.
 

cacheSolve <- function(x, ...) {
        cacheSolve <- function(x, ...) {
                inv <- x$getinverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv
        }
}
