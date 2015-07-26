## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  
  #create empty placeholder, m
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #fetches the matrix contents
  get <- function() x
  
  #calculates matrix inverse and assigns value to m in a different environment
  setinverse <- function(solve) m <<- solve
  
  #fetches the stored inverse of matrix x , stored as m, from a different environment
  getinverse <- function() m
  
  #lists operations/methods the function exposes
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#fetches cached inverse of initial matrix, 
#if this doesnt exist the function calculates inverse of supplied matrix, 
#calls the set method of the function above and caches its calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #call getinverse method of function above and assings result to m
  m <- x$getinverse()
  
  #checks if m is not null, if it is not prints message below and returns the fetched inverse and exits
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #as no data is cached, it fetches the initial matrix
  data <- x$get()
  
  #calculates the inverse and assigns to m
  m <- solve(data, ...)
  
  #calls setinverse method from above and caches m
  x$setinverse(m)
  
  #returns m
  m
  
}
