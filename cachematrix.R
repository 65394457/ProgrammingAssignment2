# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
# begin by giving inital parameters x as matrix input and inv which will store inverse as empty
        inv <- NULL
        
# want everything in this environment to be accessible to outside environment calling this function
        set <- function(y) {
                x <<- y
                inv <<- NULL   # best to keep resetting to empty or might store wrong answers
        }

# if function already called then inverse should be cached as would original input matrix
# make it accessible by storing it in get
        get <- function() x

# this is where the cacheSolve function will set the inverse and store it in inv
# inv is available in global environment and not just in setmean
        setinverse <- function(inverse) inv <<- inverse
  
# this is where cacheSolve function will check to see if an inverse has already been cached        
        getinverse <- function() inv
  
# finish by making a named list so that cacheSolve can just access by calling its name with $
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}





# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache.

# NOTE: assume that the matrix supplied is always invertible
# NOTE: if X is a square invertible matrix, then solve(X) returns its inverse

cacheSolve <- function(x, ...) {
# takes makeCacheMatrix format as input x (which is a list and has 2 objects x and inv in it)
# since it's a list see if inverse already cached and just return it if it is in object inv
# if it's already cachec then value of inv will not be NULL
        inv <- x$getinverse()
        if(!is.null(inv)) {
           # message("getting cached data")
            return(inv)
        }
        
# if it's not cached (then inv is NULL) then calculate the inverse from scratch
# get the input matrix stored in $get part of the list and set the inverse with the answer
        data <- x$get()
        inv <- solve(data, ...)     # only for square matrices
        x$setinverse(inv)
        inv
}
