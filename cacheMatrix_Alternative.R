
cacheMatrix <- function( m = matrix() )
{
	#	Storage for the inverse matrix (Inverse Matrix)
	im <- NULL
	
	
	#	Store a new matrix in the cache
	set <- function( mat )
	{
		#	Assign passed in matrix to cache storage for the original matrix
		#	in the parent environment.
		m <<- mat
		
		#	Reset the cache storage for the inverse matrix in the parent
		#	environment to NULL since we just received a new matrix.
		im <<- NULL
	}
	
	#	Return the cached original matrix (non-inverse)
	get <- function()
	{
		m
	}
	
	getinverse <- function()
	{
		if( is.null( im ) )
		{
			#	Need to calculate the inverse before returning
			message( "Solving inverse matrix..." )
			im <<- solve( m )
		}
		
		#	Return the cached inverse matrix
		im
	}
	
	#	Return the list of functions.  This provides outside access
	#	to the values cached within the makeCacheMatrix function call.
	list(
			set = set
			, get = get
			, getinverse = getinverse
		)

}
