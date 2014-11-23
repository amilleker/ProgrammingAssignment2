
makeCacheMatrix <- function(x = matrix()) {
  
  inv  <- NULL
  #initialize inv that will store the inverted matrix
  
  set  <- function(y){
  #Set the value of the matrix
    x <<- y
    inv <<- NULL 
  }
  
  get  <- function() x
  #Get the value of the matrix
  
  setinverse  <- function(inverse) inv  <<- inverse
  #Set the value of the inverse
  
  getinverse  <- function() inv
  #Get the value of the inverse
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) 
  
}
#makeCacheMatrix: This function creates the matrix object that can cache its inverse. It will be used in the second function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  #bring back the inverted matrix if its exists, else inv is NULL
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if inv is not null, it will return the already inverted matrix
  
  data <- x$get()
  inv <- solve(data, ...) #to inverse the matrix
  x$setinverse(inv) #to cache the inverse
  inv
}
#This function computes the inverse of the matrix returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.
