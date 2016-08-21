##This script computes inverse of a matrix or retrieves it from cache

## This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        Inverse<-NULL
        set<-function(my_matrix){
                x<<-my_matrix
                Inverse<<-NULL
        }         
        get<-function()x
        setInverse<-function(Inv) Inverse<<-Inv
        getInverse<-function()Inverse
        list(set = set, get = get,
             setInv = setInverse,
             getInv = getInverse)
}
## This function computes the inverse of the matrix returned by makeCacheMatrix if it has not already been 
##computed, in which case it retrieves the inverse from cache
cacheSolve <- function(x, ...) {
        Inverse <- x$getInv()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        data <-x$get()
        Inverse<- solve(data, ...)
        x$setInv(Inverse)
        Inverse
}