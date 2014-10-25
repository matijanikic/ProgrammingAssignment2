#Creates the function capable of preserving the state of the matrix.
#Input is a default 2x2 matrix of NA values
 
 
 
makeCacheMatrix <- function(input_matrix = matrix(nrow=2,ncol=2)) {
 
            inverse_matrix <<- NULL                                          # reset the matrix
        get <- function() input_matrix                           # retrieve the inputed matrix
        get_inverse_matrix <- function() inverse_matrix    # retrieve the previously calculated inverse matrix
           
                                                                                        # set the  inverse_matrix calculated in the main function into cache
            set <- function(y){                                 
                                         inverse_matrix <<- y
                                         }
            clearCache <- function() inverse_matrix <<- NULL     # clear the cache on request
        list(get = get, get_inverse_matrix = get_inverse_matrix, set = set, clearCache = clearCache)
}
 
 
#Main function which calls the makeCacheMatrix function
#Arguments are a list created by makeCacheMatrix function and the clearcache option
 
#with clearcache as TRUE:
 
#> cacheSolve(z,clearcache = FALSE)
#getting cached data
#     [,1] [,2]
#[1,]   -1  1.5
#[2,]    1 -1.0
 
#with clearcache as FALSE(defualt behaviour)
 
#> cacheSolve(z,clearcache = TRUE)
#     [,1] [,2]
#[1,]   -1  1.5
#[2,]    1 -1.0
 
 
 
 
cacheSolve <- function(x, clearcache = FALSE,...) {
 
            if(clearcache == TRUE) {
                        x$clearCache()               # reset the cache
                  }
           
        inverse <- x$get_inverse_matrix()      # get the cached matrix
        if(!is.null(inverse)) {                # if it is not empty, return and exit function
                message("getting cached data")
                return(inverse)
        }
        object <- x$get()                            # get the original inputed data
        inverse <- solve(object)               # calc the inverse matrix
            x$set(inverse)                                 # set it into cache
        inverse                                      # return the calculated inverse matrix
}

