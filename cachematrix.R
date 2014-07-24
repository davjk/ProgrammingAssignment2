## Assignment to demonstrate the scoping aspects of R by first creating a function
## that can crate a matrix and cache its inverse, then creating a function that 
## returns the inverse matrix by 1st checking the cache to see if the inverse has 
## already been calculated and returnting it, or claculating the inverse if it has
## not already been done

## function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
## set and store the value of matrix
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
## get the value of matrix
  get <- function()x
## set and store value of inverse matrix using solve() function
  setinverse <- function(solve) m <<- solve
## get inverse matrix
  getinverse <- function()m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## function to return inverse matrix by 1st checking the cache to see if it has 
## already been calculated and returning it, or calculating itself if not already 
## in cache

cacheSolve <- function(x, ...){
## check to see if the inverse matrix has already been calculated and return if it
## has
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
## if the inverse is not available get the original matrix
  data <- x$get()
## set & strore then return the inverse matrix
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}