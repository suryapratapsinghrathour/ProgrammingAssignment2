## Caching the Inverse of a Matrix
## through lexical scoping

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
	## setting initial value of inv as NULL 
	inv <- NULL
	## function for setting the matrix
	set <- function(y)
		{	
			# '<<-' operator assign value
			x <<- y
			inv <<- NULL
		}
	## function for getting the matrix
	get <- function()
		{
			 x
		}
	## function for setting inverse of the matrix
	setmatinv <- function(invmat)
			{
				inv <<- invmat	
			}
	## function for getting inverse of the matrix
	getmatinv <- function()
			{
				inv
			}
	## return the above function as list 
	list(set = set, get = get, setmatinv = setmatinv, getmatinv=getmatinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
	inv <- x$getmatinv()
	## check if the inverse is already exist
	if(!is.null(inv))
	{	# if inverse exit then display this message
		message("Getting cached Matrix")
		# return inverse
		return(inv)
	}
	## if inverse of the matrix does not exist then getting the matrix object at storing it in mat.data
	mat.data <- x$get()
	## finding inverse of matrix by using solve() function
	inv <- solve(mat.data, ...)
	## setting the inverse value
	x$setmatinv(inv)
	## Return a matrix that is the inverse of 'x'
	return(inv)  
}
