## Calculates the inverse of a matrix. In order to save time and resources, 
## first check if the inverse already exists from previous operations, if so
## it loads it from the cache, if not, it calculates it and stores it in 
## cache for later use.

## This helper function creates a special matrix that:
## 1. Set the matrix
## 2. Get the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## The get/set method are similar to provided example, using matrix type
    m <- NULL
    set <- function(y){
        ## stores the x and m matrixes in the upper environment, where 
        ## they can be accessed
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    ## this is where we calculate the inverse
    setinverse <- function(solve) m <<- solve
    getinverse <- function() return(m)
    
    ## finally we return the object created with these functions as a list 
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function calculates the inverse of a matrix using the makeCacheMatrix 
## function if the matrix already exists, it retrieves it from the cache, if 
## not, it calculates the inverse and store it for later use.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## check if it is already in the cache and return it if so
    if(!is.null(m)){
        message("getting the cached inverse matrix")
        return(m)
    }
    
    ## if it is not in cache, it sends the matrix to cache calculating the
    ## inverse in the process and storing it
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    ## finally it returns the inverse matrix
    return(m)
}
