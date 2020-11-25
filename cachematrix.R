makeCacheMatrix <- function(x = matrix()){
        
        I <- NULL
        set <- function(m){
                x <<- m
                I <<- NULL
        }
        get <- function() x
        setInv <- function(i) I <<- i
        getInv <- function() I
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


cacheSolve <- function( x, ...){
	M <- x$getInv()
	if (is.null(M)) {
		data <- x$get()
		M <- solve(data, ...)
		x$setInv(M)
	} 
	M
}

