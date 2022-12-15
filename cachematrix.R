## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a "special" matrix, and returns a list
## containing a function to set the matrix, get the matrix,
## set the inverse, and get the inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ##initiates our cached inverse as a null value
        inverse <- NULL
        
        ## Defines a function that takes a matrix as an
        ## argument and super assigns it to the argument
        ## passed
        setMatrix <- function(mtx) {
                x <<- mtx
                inverse <<- NULL
        }
        
        ## Defines a function that returns the matrix
        getMatrix <- function() {
                x 
        }
        
        ## Defines a function that sets the inverse of
        ## the matrix
        setInverse <- function(inv) {
                inverse <<- inv
        }
        
        ## Defines a function that gets the inversed
        ## matrix
        getInverse <- function() {
              inverse
        }
        
        ## The list we return with our functions
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse, 
             getInverse = getInverse)
        
}


## Write a short comment describing this function

## This function calculates the inverse value of the 
## "special" matrix created in the above function.
cacheSolve <- function(x, ...) {
        
        ## Defines the value of the inverse and sets it as
        ## the cached inverse
        inverse <- x$getInverse()
        
        ## If the inverse value is not null and has a value
        ## and the matrix has not changed and is the same,
        ## We display a status message and then simply 
        ## return the cached value.
        if (!is.null(inverse) | x == x$getMatrix()) {
                message("Retrieving cached data..")
                return(inverse)
        }
        
        ## Define our data as the cached matrix
        data <- x$getMatrix()
        
        ## Define the inverse of the matrix and calculates
        ## using the solve function
        inverse <- solve(data,...)
        
        ## Gets the inverse 
        x$setInverse(inverse)
        
        ## Returns the inverse matrix
        inverse
}
