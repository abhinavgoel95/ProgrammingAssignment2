## Put comments here that give an overall description of what your
## functions do
##The functions are used to depict the use of Lexical Scoping in R.

## Write a short comment describing this function
##makeCacheMatrix is a function which returns a list of functions.
##These functions are: set, get, setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
	matinv <- NULL
	set <- function(y)
	{
      		x <<- y
      		matinv <<- NULL
    	}
	get <- function() 	x 			#Retrieve the matrix
	setinv <- function(inv)	matinv <<- inv 		#Store the inverse in cache
	getinv <- function() matinv 				#Retirive the inverse from cache.
	list(set = set, get = get , setinv = setinv, getinv = getinv)
												#Return a list of functions
}


## Write a short comment describing this function
##cacheSolve is for finding the inverse of the matrix (defined in x$set()).
##It will check if the inverse exits in the cache. If not it will find the inverse.

cacheSolve <- function(x, ...) {
     inv <- x$getinv() 		#Read inverse from cache
	if(! is.null(inv)) 		#Check if inverse exists in cache
	{
		print("Available in cache");
		matinv <<- inv
		return(matinv)
	}
	mat <- x$get() 			#Obtain the matrix
	inv <- solve(mat,...) 	#Find the inverse
	x$setinv(inv) 			#Store inverse in cache
	inv 					#return the inverse
}
