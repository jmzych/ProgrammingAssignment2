## =======================================================================
## Project: Programming Assignment 2: Lexical Scoping.  jmzych--20160303
## =======================================================================
##      
## Two R functions: makeCacheMatrix() and cacheSolve() work together to create,  
##     a "special" matrix object and store its inverse calculations in memory.
##
## Benefit: Caching the inverse of a large matrix rather than computing it 
##      repeatedly saves computation time - creates efficiency
##
## The makeCacheMatrix() function 
## Takes input of a matrix (value x) to create a "special" matrix object
##     and set it.
## Provides a list of 4 functions contained within the function to get or set 
##     the matrix, and get or set the inverse matrix calculation.
## Uses a special assignment operator to modify variables not exposed
##     to the global environment. 
## Receives and/or sets the inverse matrix calculation returned from the 
##     cacheSolve() function, storing its value to an allocated address in memory.
## 
##     NOTE: A "special" matrix is a square matrix that is an equal number of rows and 
##     columns, in order to allow a matrix inverse caluculation to be performed.
## 
##     NOTE Lexical Scope: global variable "m" is declared with standard
##     assignment operator "<-" and requires the special assignement operator
##     "<<-" to be used within the containing set() function, to prevent outside
##     exposure in order for the calculated inverse matrix value to be updated 
##      and stored in memory "m" correctly. Be mindful of how the memory is managed.
##
## Returns the "special" matrix object, or NULL, if no inverse calculation exists,  
##     or the inverse matrix calculation if one exists.
## 
## Test use original: makeCMtest <- makeCacheMatrix(matrix(c(1:4), nrow=2, ncol=2))  
##                    makeCMtest$get()
##                    cacheSolve(makeCMtest)
##                    makeCMtest$getinverse()      
##
## Test use update    makeCMtest1 <- makeCacheMatrix(SomeOtherSquareMatrix)
## matrix:            makeCMtest1$get()
##                    cacheSolve(makeCMtest1)
##                    makeCMtest1$getinverse()
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                       ## where matrix inverse calc is stored, default is NULL 
        set <- function(y) {                            ## sets the matrix object x 
                x <<- y                                  
                m <<- NULL
        }
        get <- function() x                             ## if matrix object x exists, will return the matrix 
        setinverse <- function(inverse) m <<- inverse   ## will set the matrix inverse calculation
        getinverse <- function() m                      ## if matrix inverse exists Will return it
        list(set = set, get = get,                      ## list of 4 functions: 
             setinverse = setinverse,                      ## set = create matrix
             getinverse = getinverse)                      ## get = return matrix
                                                           ## setinverse = set inverse matrix calculation  
}                                                          ## getinverse = get inverse matrix calculation 

## The cacheSolve() function checks if an inverse matrix calculation exists 
## cached in memory, and gets it. If the inverse matrix calculation does not 
## exist or the matrix has changed, it will compute a new inverse of the 
## matrix object that was originally returned from the makeCacheMatrix() function,  
## and set the new matrix inverse calculation to memory "m". 

## Test use original:       cacheSolve(makeCMtest)
## Test use update matrix:  cacheSolve(makeCMtest1)

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                             ## get the inverse matrix from x                        
        if(!is.null(m)) {                               ## if the inverse exists...(not NULL)
                message("getting cached data")          ##   echo or print the "message"
                return(m)                               ##   and return the inverse matrix calculation
        }
        data <- x$get()                                 ## if nothing is returned gets the matrix object
        m <- solve(data, ...)                           ## calculate the matrix inverse
        x$setinverse(m)                                 ## set the inverse matrix calculation of x matrix 
        m                                               ## return the inverse matrix calculation
}
##end