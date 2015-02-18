## Function makeCacheMatrix create a special matrix, which 
## can cache the result if the matrix has already computed. 
## Then function cacheSolve will use the return of the makeCacheMatrix
## to give the inverse of matrix, if it's invertible.
## If you want to inverse a matrix "x", input should be:
## cacheSolve(makeCacheMatrix(x))


## makeCacheMatrix is a list containing functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  ## first, you need to test whether the matrix is invertible.
  if(det(x)==0){
    message("This Matrix is impossible to inverse!")
  } else{
    result <- NULL
    set <- function(y){
      x <<- y
      result <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) result <<- inverse
    getInverse <- function() result
    list(set = set, setInverse = setInverse, 
         get = get, getInverse = getInverse)
  }
  }




## cacheSolve will give the answer for inverse of the matrix if it 
## exists. First, it will check whether it has been calculated: if 
## yes, it will return the value in cache; if no, it will calculate
## the inverse matrix and save the answer to the cache.
cacheSolve <- function(x, ...) {
  result <- x$getInverse()
    if(!is.null(result)){
      message("getting inversed matrix")
      return(result)
    }
    data <- x$get()
    result <- solve(data,...)
    x$setInverse(result)
    result
}
