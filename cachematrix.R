## Put comments here that give an overall description of what your functions do
# this is a set of functions that caches the inverse of a matrix

# How to setup to run on the machine:
# setwd("c:/users/acongdon3/Documents/Coursera/2 course/ProgrammingAssignment2")
# mat <- matrix(1:4, 2, 2)
# source("cachematrix.R")

## Write a short comment describing this function
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse, 
# which is really a list containing a function to:
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  #Computing the inverse of a square matrix can be done with the solve function in R. 
  #For example, if X is a square invertible matrix, then solve(X) returns its inverse.
  # below "setinverse" is the way to solve the matrix
  
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  #return this list of the four things to do to the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)


}


## Return a matrix that is the inverse of 'x'
#The following function calculates the inverse of the special "matrix" created with the above function.
# However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the 
#cache via the setinverse function.
cacheSolve <- function(x, ...) {
  #check if the inverse exists
  m <- x$getinverse()
  
  #if it exists return the cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #exit the function
  }
  
  #otherwise - solve the matrix and return the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m

}
