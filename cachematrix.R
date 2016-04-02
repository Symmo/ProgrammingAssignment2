## These pair of functions cache the inverse of a
## matrix. 

## This function makes a matrix object with a 
## list of methods to get and set values of its elements
## and to get and set its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function ()x
  set_inv<-function(invr) inv<<-invr
  get_inv<-function() inv
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## This function returns the inverse of a matrix.
## It returns the cached inverse if there is one,
## if not it computes the inverse and caches it.
## It assumes the input matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv<- x$get_inv()
  if (!is.null(inv)){
    message ("getting cached data")
    return (inv)
  }  
  m<- x$get()
  inv<-solve(m)
  x$set_inv(inv)
  inv
}

## Sample run

## > q <- rbind(c(0,1,2), c(1,0,3), c(4,-3,8))
## > r <- makeCacheMatrix(q)
## > r$get()
## [,1] [,2] [,3]
## [1,]    0    1    2
## [2,]    1    0    3
## [3,]    4   -3    8

## In the first run there is no
## cached inverse for matrix r.
## > cacheSolve(r)
## [,1] [,2] [,3]
## [1,] -4.5    7 -1.5
## [2,] -2.0    4 -1.0
## [3,]  1.5   -2  0.5

## In the second run, the function
## returns the cached inverse of 
## matrix r.
## > cacheSolve(r)
## getting cached data
## [,1] [,2] [,3]
## [1,] -4.5    7 -1.5
## [2,] -2.0    4 -1.0
## [3,]  1.5   -2  0.5
