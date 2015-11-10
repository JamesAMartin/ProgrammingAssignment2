
#	Create a list comprised of functions that allows for the
#	caching of a vector and get/set the values
makeVector <- function(x = numeric())
{
	m <- NULL
	
	set <- function(y)
	{
		#	Assign x to parent environment, essentially creating
		#	a cache for the vector passed in.
		x <<- y
		
		#	Set the mean storage in the parrent environment to NULL
		#	since we have a new vector passed in.
		m <<- NULL
	}
	
	get <- function()
	{
		#	Return the cached vector that was used when makeVector was
		#	first called.  If it has not been instantiated yet by the set
		#	function, this function will fail.
		x
	}
	
	setmean <- function(mean)
	{
		#	Assign value passed in to the parent environment.
		m <<- mean
	}
	
	getmean <- function()
	{
		#	Return the cached value of the mean.
		m
	}
	
	#	Return the list of functions.  This provides outside access
	#	to the values created/stored within the makeVector function call.
	list(
			set = set
			, get = get
			, setmean = setmean
			, getmean = getmean
		)
}


#	Caches the mean of a vector.  Requires a list created by  makeVector()
#	be passed in.
cachemean <- function(x, ...)
{
	#	Check to see if the mean has already been calculated and cached.
	m <- x$getmean()
	
	#	If the mean has been cached, notify we are pulling from cache and
	#	return the mean.
	if(!is.null(m))
	{
		message("getting cached data")
		return(m)
	}
	
	#	Pull the vector used in calling makeVector().
	data <- x$get()
	
	#	Calculate the mean of the vector, storing it locally.
	m <- mean(data, ...)
	
	#	Store the locally calculated mean back into the cache object.
	x$setmean(m)
	
	#	Return the calculated mean
	m
}
























