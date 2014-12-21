## These 2 functions work together to cache the inverse of a matrix
## First function loads a matrix and the second function retrieves it

## "makeCacheMatrix" takes a square matrix for which an inverse can be calculated
## There are 4 methods - set_mat, get_mat, set_inv_mat, & get_inv_mat - which
## allow for a matrix and it's inverse to be loaded/fetched from cache
## Please check out help on assignment operator ?`<<-` 

makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set_mat <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    get_mat <- function() { x }
    set_inv_mat <- function(solve) { inv_mat <<- solve }
    get_inv_mat <- function() { inv_mat }
    list(set_mat = set_mat, get_mat = get_mat, set_inv_mat = set_inv_mat, get_inv_mat = get_inv_mat)
}


## Returns a matrix that is the inverse of matrix loaded by "makeCacheMatrix"
## Checks if the inverse has already in cache and gets it from cache
## If inverse is not available then it's calculated

cacheSolve <- function(x, ...) {
          inv_mat <- x$get_inv_mat()
          if (!is.null(inv_mat)) {
              message("getting cached inverse matrix")
              return(inv_mat)
          }
          mat_2_op <- x$get_mat()
          inv_mat <- solve(mat_2_op, ...)
          x$set_inv_mat(inv_mat)
          inv_mat        
}
