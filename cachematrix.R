## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {     # input x will be a matrix
        
        m <- NULL # m will be the 'inverse' and it's reset to NULL every time makeCacheMatrix is called
        
        set <- function(y) {    # takes an input vector
                x <<- y         # saves the input vector 
                m <<- NULL      # resets the mean to NULL, basically what happens when a new object is generated.
        }
        
        get <- function(){x} # This funtion returns the value of the original matrix
        
        setInverse <- function(solve) { m <<- solve } # This is called by cacheSolve() during the first cacheSolve access and it will store the value using superassignment
        
        getInverse <- function() { m } # This will return the cache value to cacheSolve() on subsequent access
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) # This is accessed each time makeCacheMatrix() is called, i.e. each time you make a new object.
                                                                          # This is a list of the internal functions ('methods') so a calling function knows how to access those methods.    
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {       # The input x is an object created by makeCacheMatrix
        
        m <- x$getInverse()     # Accesses the object 'x' and gets the value of the inverse 
        if(!is.null(m)) {       # If inverse was already cached (not NULL) ...
                
                message("getting cached data")  # ... send this message to the console
                return(m)                       # ... and return the inverse ... "return" ends the function cacheSolve()
        }
        data <- x$get()         # We reach this code only if x$getInverse() is returned NULL
        m <- solve(data, ...)   # If m was NULL then we have to calculate the inverse
        x$setInverse(m)         # store the calculated inverse value in x (see setInverse() in makeCacheMatrix)
        m                       # Return the inverse to the code that called this function
        
}