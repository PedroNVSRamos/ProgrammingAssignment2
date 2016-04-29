makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse.
  
  # But first we need to inspect if x is squared.
  # In order to do so we evaluate the equality of the dimensions
  is_squared <- function(x){
    if (nrow(x) == ncol(x)){
      # "x is squared matrix"
      
      if(det(x) == 0){
        # x is singular
        print(paste("|", deparse(substitute(x)),"| = 0.", sep = ""))
        print(paste(deparse(substitute(x)),"is singular and thus it has no inverse.", sep = ""))
      } else{
        # x is not singular
        CacheMatrix <- solve(x)
        CacheMatrix
      }
    } 
    else{
      print(paste(deparse(substitute(x)),"is not squared matrix.Non-square matrices do not have an inverse."), sep = "")
    }
  }
  special_matrix <<- is_squared(x)
}


## The first step for the function is to evaluate whether a matirix is squared, and thus ivertible, or not.
## In case not, the following message is returned "Non-square matrices do not have an inverse."
## In case the matrix has inverse, this one is computed and saved


cacheSolve <- function(x, ...) {
  if( identical(makeCacheMatrix(x), solve(x))){
    special_matrix
  } else{
    makeCacheMatrix(x)
  }
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.