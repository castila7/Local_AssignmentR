
## The purpose of this function is to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## where x is a square invertible matrix
	## set() sets the matrix
	## get() gets the matrix
	## setinv.matrix() sets the inverse of the matrix
	## getinv.matrix() gets the inverse of the matrix
	inv.matrix <- NULL
	set <-function(y){
		x <<- y
		inv.matrix <<- NULL
	}
	get <-function()x
	setinv.matrix <- function(inv) inv.matrix <<- inv 
	## The '<<-' is used to assign a value to an onbject in an 
	##environment different from the current environment
	getinv.matrix <- function () inv.matrix
	## The list generated below is used as the input to cacheSolve()
	list (set=set, get=get, setinv.matrix=setinv.matrix, getinv.matrix=getinv.matrix)
}

## The purpose of this function is to compute the inverse of the special "matrix" returned by the function makeCacheMatrix() above. Note that if the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv.matrix <- x$getinv.matrix()
        ##If the inverse has alreadt been calculated, skip computation and get from cache
        if(!is.null(inv.matrix)){
        	message("getting cached data")
        	return(inv.matrix)
        }
        #otherwise, calculate the inverse of the matrix
        data <- x$get()
        inv.matrix <-solve (data, ...)
        x$setinv.matrix(inv.matrix)
        return (inv.matrix)
}
