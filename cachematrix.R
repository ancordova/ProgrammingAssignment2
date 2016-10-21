## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversa <- function(solve) inversa <<- solve(x)
  getinversa <- function() inversa
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  funciones <- makeCacheMatrix(x)
  inversa <- funciones$getinversa()
  if(!is.null(inversa)){
    message("getting cache data")
    return(inversa)
  }
  data <- funciones$get()
  inversa <- solve(data)
  funciones$setinversa(inversa)
  inversa
}
