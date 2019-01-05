## Put comments here that give an overall description of what your
## functions do

##The makeCacheMatrix function is a special function that creates an object that
##stores the matrix and its calculated inverse matrix. The function also defines 4 functions
## that work on the matrix at different stages.

makeCacheMatrix <- function(x = matrix()) {
    #the value of m is initialised to NULL. m is the cache variable in the parent environment.
    m <- NULL
    
    ##The set function below takes argument y of type matrix. Here the matrix y
    ## is different from the matrix x that was passed originally as the argument to the function.
    ## x in the parent environment is assigned the value of y with the <<- operator. Now as x has
    ## taken up a new value, it is important to reset the cache value to NULL. m could contain the
    ## earlier cached result if cacheSolve() function had been called already.
    set <- function (y = matrix()){
        x <<- y
        m <<- NULL
    }
    
    ##The get function gets the value of x from the parent environment of makeCacheMatrix
    get <- function() x
    
    ##The setsolve function is called by the CacheSolve function when a new inverse for the matrix is
    ## calculated. This function makes use of the <<- operator to assign the latest value of inverse
    ## matrix to cache in the parent environment.
    setsolve <- function(mysol = matrix())  m <<- mysol
    
    ## getsolve() function is used to retrive the cached value of m from the parent environment.
    getsolve <- function() m
    
    ## a list of functions defined in makeCacheMatrix is returned with names assigned to functions for easy reference
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The cacheSolve function takes an object of type makeCacheMatrix and either retrieves the cached value of
##the inverse of matrix or, if m is NULL (which means the inverse needs to be calculated first), then it calculates
##the inverse of matrix and returns the value back to be cached in the parent environment.

cacheSolve <- function(x, ...) {
    
    ## in the line below, a local variable m gets the latest value of the cached inverse matrix which is stored in
    ## the parent's environment.It does this by calling the getsolve() function which is accessible as cacheSolve
    ## takes object of type makeCacheMatrix as argument.
    m <- x$getsolve()
    
    ## the if condition checks whehter there is an existing cached value stored in the parent environment. If there is,
    ## then function will return this cached value instead of computing it again. The cached result is recognisable at the
    ## console because of the printed message "getting cached matrix data".
    if(!is.null(m)) {
        message("getting cached matrix data")
        return(m)
    }
    
    ## if there is no cached result, then the function begins to compute the inverse of matrix by calling get()
    ## and obtaining the matrix that is stored in makeCacheMatrix. This matrix is stored in variable 'data'.
    ## The 'solve' function computes the inverse of the matrix and the result is stored in the parent
    ##  environemnt variable (the cache) by calling the function setsolve().
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    
    ## As the inverse has been calculated for the first time, cacheSolve returns this value to the console as well. This value
    ## is now cached and will be refered to in future as long as the input matrix does not change.
    m
}
