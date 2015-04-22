 # Create a 'cacheMatrix' object capable of storing the matrix and its inverse
 # @param  {matrix} m   The matrix to store
 # @return {list}       A list of getters and setters for both the matrix and its inverse
makeCacheMatrix <- function(m = matrix()) {
  matrixInverse <- NULL
  set <- function(newMatrix) {
    m <<- newMatrix
    # Nullify the inverse when changing the original matrix
    matrixInverse <<- NULL
  }
  get <- function() m
  setMatrixInverse <- function(newMatrixInverse) matrixInverse <<- newMatrixInverse
  getMatrixInverse <- function() matrixInverse
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


 # Return the cached inverse of a given matrix, if not yet cached, solve the inverse and cache the result
 # @param  {cacheMatrix} x    A cacheMatrix derived from calling makeCacheMatrix
 # @return {matrix}           The inverse of matrix x
cacheSolve <- function(x) {
  mi <- x$getMatrixInverse()

  # Return the inverse of x if cached
  if(!is.null(mi)) {
    return(mi)
  }

  # If not cached, get the matrix, compute the inverse, cache results, and return the inverse
  m <- x$get()
  mi <- solve(m)
  x$setMatrixInverse(mi)
  mi
}
