########################
# Course 2: Week 3 Project (Lexical Scoping)
# Aldreen Venzon
# 8/27/18
########################

## Put comments here that give an overall description of what your functions do
# These 2 functions below cache the inverse of a matrix.

## Write a short comment describing this function
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( x = matrix() ) 
{
        m <- NULL
        
        set <- function( y ){
                x <<- y
                m <<- NULL
        }
        
        get <- function( ) 
                x
        
        setM <- function( solve ) 
                m <<- solve
        
        getM <- function( ) 
                m
        
        list(set = set, get = get, 
             setM = setM, 
             getM = getM)
}


## Write a short comment describing this function
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function( x, ... ) 
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getM( )
        
        if( !is.null(m) ){
                message("Getting Cached Data...")
                m
        }
        
        data <- x$get( )
        
        m <- solve( data, ... )
        
        x$setM( m )
        
        m       
}


## Tests:
# sampleMX <- makeCacheMatrix(matrix(1:4, 2, 2))
# str(sampleMX)
# names(sampleMX)
# head(sampleMX)
# 
# sampleMX$set()
# sampleMX$get()
# sampleMX$getM()
# 
# cacheSolve(sampleMX)

#Sample:
# makeVector <- function(x = numeric()) {
#         m <- NULL
#         set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function() x
#         setmean <- function(mean) m <<- mean
#         getmean <- function() m
#         list(set = set, get = get,
#              setmean = setmean,
#              getmean = getmean)
# }
# cachemean <- function(x, ...) {
#         m <- x$getmean()
#         if(!is.null(m)) {
#                 message("getting cached data")
#                 return(m)
#         }
#         data <- x$get()
#         m <- mean(data, ...)
#         x$setmean(m)
#         m
# }















