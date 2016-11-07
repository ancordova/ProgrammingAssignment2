## @author: Andrés Córdova
## This script contains 2 functions:
## 1) makeCacheMatrix: funcion which contains 4 functions in it,
## set, get, setinversa and getinversa. 
## 2) cacheSolve: return the inverse of a matrix, if the matrix has an inverse


## This function "makeCacheMatrix" recieve a matrix.
## The four functions in it are set, get, setinversa, getinversa
makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  ## function set assign the value "y" to the default matrix "x"
  ## function set assign the value "NULL" to the parameter "m", 
  ## who is invoked in the function which calls "MakeCacheMatrix"
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## function get only returns the value of matrix x
  get <- function() x
  ## function setinversa returns the inverse of matrix x if it have one
  setinversa <- function(solve) inversa <<- solve(x)
  ## function getinversa returns the value calculated in function setinversa
  getinversa <- function() inversa
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}

## This function "cacheMatrix" recieve a matrix and if 
## the value of its inverse is calculated, it returns 
## this value, if not, it calculates the inverse. 
cacheSolve <- function(x, ...) {
  ## Create a vector of the functions of MakeCacheMatrix with parameter x
  funciones <- makeCacheMatrix(x)
  ## Try to return the inverse of the matrix x if exist. 
  inversa <- funciones$getinversa()
  if(!is.null(inversa)){
    message("getting cache data")
    return(inversa)
  }
  ## If the value wasn't in cache, it calculates the inverse. 
  data <- funciones$get()
  inversa <- solve(data)
  funciones$setinversa(inversa)
  inversa
}
