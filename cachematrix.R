## The functions makeCacheMatrix and cacheSolve basically work in tandem to store 
## a matrix and its inverse in cache. An example is provided at the bottom.

## This function allows to store matrices and their inverses in cache so that 
## they can be used later. Its input has to be an invertible matrix (no need
## to provide the number of columns, as the matrix is a square matrix). Its output is 
## a list that does not show explicitly any stored information. To access the stored
## matrix one has to call the get element in the list. Similarly, to access the 
## associated inverse, one has to call the getinv element in the list. Note that the
## first time a matrix is stored, it will not have any inverse stored alongside; 
## only after using cacheSolve will there be the associated inverse stored.

makeCacheMatrix <- function(M = matrix()){
        inv <<- NULL
        set <- function(A){
                M <<- A
                inv <<- NULL
        }
        get <- function() M
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function retrieves the matrix inverse of a matrix, if this one has already 
## been stored, or it calculates it and then stores it. This function needs the 
## output of the makeCacheMatrix as input. The first time it is used with a matrix,
## that was just stored for the first time, it calculates its inverse and stores it
## in cache and then returns the inverse. Later if one wants to access this inverse,
## one just calls the getinv element of the list in makeCacheMatrix.

cacheSolve <- function(M, ...) {
        inv <- M$getinv()
        if(!is.null(inv)){
                message("Getting cached data.")
                return(inv)
        }
        data <- M$get()
        inv <- solve(data,...)
        M$setinv(inv)
        inv
}

## Let's say I am going to need to use the following matrix for a while

matrix(1:4,2)

## then to store it and be able to store its inverse for further use, I use the 
## following code:

Vector1 <- makeCacheMatrix(matrix(1:4,2))
Vector1$get() ## This provides the matrix just stored a moment ago.
Vector1$getinv() ## At this point, this code will return NULL, since the inverse as
## not been calculated yet.
cacheSolve(Vector1) ## It will return at this point the inverse of the matrix 
## stored in Vector1 and simultaneously will store its inverse in Vector1.
Vector1$getinv() ## This code now will return the inverse of the matrix stored in 
## Vector1.