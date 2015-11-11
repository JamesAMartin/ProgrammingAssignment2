
###	makeCacheMatrix and cacheSolve work together to provide cached
###	access to a stored matrix, and the inverse of that same matrix.  Used
###	properly, the cost of inverting the cached matrix will happen once,
###	regardless of how many times it is needed for other calculations.


##	Create a list of functions that allow for the caching and reacall of
##	a matrix, as well as the caching and recall of the matrix inverse.
makeCacheMatrix <- function( x = matrix() )
{
	#	Storage for the inverse matrix (Inverse Matrix)
	im <- NULL
	
	
	#	Store a new matrix in the cache
	set <- function( y )
	{
		#	Assign passed in matrix to cache storage for the original matrix
		#	in the parent environment.
		x <<- y
		
		#	Reset the cache storage for the inverse matrix in the parent
		#	environment to NULL since we just received a new matrix.
		im <<- NULL
	}
	
	#	Return the cached original matrix (non-inverse)
	get <- function()
	{
		x
	}
	
	#	Store the passed in matrix in cache as the inverse matrix
	setinverse <- function( inverse )
	{
		im <<- inverse
	}
	
	#	Return the inverse matrix stored in cache.  Since the cache
	#	storage is initialized with NULL, if setinverse() has not been
	#	called, this function will return NULL.
	getinverse <- function()
	{
		im
	}
	
	#	Return the list of functions.  This provides outside access
	#	to the values cached within the makeCacheMatrix function call.
	list(
			set = set
			, get = get
			, setinverse = setinverse
			, getinverse = getinverse
		)

}


##	Solves for the inverse of a matrix, pulling the matrix to invert
##	from the passed in makeCacheMatrix object (x) and storing the inverse
##	matrix in the same passed in object for future reference.
cacheSolve <- function(x, ...)
{
	#	Pull the inverse matrix stored in cache
	im <- x$getinverse()
	
	#	Test to see if the inverse matrix has been cached yet
	if( !is.null( im ) )
	{
		#	Since the inverse matrix exists in cache, notify we are pulling 
		#	from cache and return the inverted matrix.
		message( "Getting inverse matrix from cache." )
		return( im )
	}
	else
	{
		#	Since the inverse matrix has not been cached, solve for the inverse,
		#	store it to cache, then return the inverse.
		
		#	PUll the original, non-inverse matrix
		om <- x$get()
		
		#	Invert the matrix
		im <- solve( om, ... )
		
		#	Store the locally calculated inverse matrix back into the
		#	cache object
		x$setinverse( im )
		
		#	Return the inverse matrix
		return( im )
	}
}
