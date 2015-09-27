## Code for Programming Assignment 2 in R Programming course
## These functions allow for cacheing the inverse of a matrix

## Create functions that allow for cacheing a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
     invx <- NULL
     setMatrix <- function(y = matrix()) {
          x <<- y
          invx <<- NULL
     } 
     getMatrix <- function() x
     setInvMatrix <- function(invMatrix) invx <<- invMatrix
     getInvMatrix <- function() invx
     list(setMatrix = setMatrix, getMatrix = getMatrix,
          setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## Calculate the inverse of a matrix

cacheSolve <- function(x, ...) {
     invx <- x$getInvMatrix()
     if(!is.null(invx)){
          message("Getting cached inverse...")
          return(invx)
     }
     data <- x$getMatrix()
     invx <- solve(data)
     x$setInvMatrix(invx)
     invx
}