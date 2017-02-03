## R Programming Week 3 Assignment 
## Lexical Scoping: Caching the Inverse of a Matrix
## 
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of 
## a matrix rather than compute it repeatedly. 
## 
## Jiangbin Yang, Feb. 2, 2017

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  getMat <- function() x
  setMat <- function(y) {x<<-y; inv<<-NULL}
  getInv <- function() inv
  setInv <- function(inverse) inv<<-inverse
  list(getMat = getMat,
       setMat = setMat,
       getInv = getInv,
       setInv = setInv)
}

## This function retrieves the inverse of the special "matrix" 
## returned by makeCacheMatrix above, if the inverse has already 
## been calculated (and the matrix has not changed). 
## Otherwsie it calculates the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (is.null(inv)){
    message("calculate inverse")
    mat <- x$getMat()
    inv <- solve(mat, ...)
    x$setInv(inv)
    return(inv)
  }
  else {
    message("retrieve cached inverse")
    return(inv)
  }
}
