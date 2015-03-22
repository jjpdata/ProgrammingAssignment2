makeCacheMatrix <- function(x = matrix()) {
        # initialize the "inverse" variable to NULL to avoid a function error
        inv <- NULL
        # y is the matrix argument passed to "makeCacheMatrix"
        set <- function(y) {
                # set x for the function environment to y
                x <<- y
                # set inv for the makeCacheMatrix envrionment to NULL
                inv <<- NULL
        }
        # create a function for get in the makeCacheMatrix parent and assign a matrix to it
        get <- function() x 
        # takes a value ('inverse') and sets it to the value of inv in the makeCacheMatrix farme
        setinv <- function(solve) inv <<- solve
        # returns the value of inv from makeCacheMatrix frame 
        getinv <- function() inv
        # lists out the functions in makeCacheMatrix frame
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
        # goes to the x environment and assigns the inv value from that environment to this one
        inv <- x$getinv() 
        #if the x environment has been evaluated before, the function prints the message
        #and the value of inv (the cached inverse)
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #if this x hasn't been evaluated, pull the x variable into a local variable called "data" 
        data <- x$get()
        #calculate the inverse of the matrix by calling the "solve" function on the local variable 
        inv <- solve(data)
        #Assign the calcualated inverse to the x envrionment using the "setinv" function
        x$setinv(inv)
        #return the inverse 
        inv
}