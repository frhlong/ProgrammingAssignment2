makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #set container for both m and y
  y <- NULL
  setmatrix <- function(y) { #pass the new matrix
    x <<- y #and store new matrix
    m <<- NULL 
    
  }
  getmatrix <- function()
    x #return matrix value
  setinverse <- function(solve) 
    m <<- solve # cache inverse matrix
  getinverse <- function() 
    m # return inverse of matrix
  list(setmatrix = setmatrix, getmatrix = getmatrix, #4 elements of functions returned
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function (x, ...) {
  m <- x$getinverse() #pass the inverse from above MakeCacheMatrix
  y<-x$getmatrix()
  if(!is.null(m)){ #if m is NULL, it means we have not calculated inverse matrix before
    message("getting cached data")
    m <- x$getinverse()}
  m
  if(identical(x$setmatrix(y), x$getmatrix())) { #check if matrix has changed or not
    message("getting cached data")
    m <- x$getinverse()}
  m  
  
  y <- x$getmatrix() #get changed matrix
  x$setmatrix(y) #cache changed matrix
  m <- solve(y, ...) #inverse changed matrix
  x$setinverse(m) #cache new inverse matrix
  m #return new inverse matrix
}