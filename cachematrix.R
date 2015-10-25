## These functions first create a list of functions. setinv() stores the
## value of the solved inverse matrix in the containing environment of the
## function makeCacheMatrix. This allows the cacheSolve to use getinv() and
## check whether an inverse has already been calculated. If yes, it returns 
## the stored, if no, then it retrieves the matrix with get(), calculates the 
## inverse and stores the value again with setinv().
## These functions make use of the nesting of functions and superassignment so 
## that the value in setinv() is stored in the containing makeCacheMatrix function
## and can be retrieved from there even though they are set within environments of
## set() and setinv(),



## The function below creates a list containing 3 functions, setinv() stores 
## solved inverse matrix. While getinv() retrieves the stored inverse matrix and
## get() retrieves the matrix.

makeCacheMatrix <- function(x = matrix()){
      i <- NULL
      get <- function()x
      setinv <- function(solved) i <<- solved
      getinv <- function()i
      func <- list(set=set,get=get, setinv=setinv,getinv=getinv)
}



## cacheSolve takes the list as input and checks to see if the inverse 
## of the matrix is cached by calling on the stored functions from makeCacheMatrix.
## If yes, the value is returned. Otherwise it calcualtes the inverse then caches
## it and returns the output. 

cacheSolve <- function(x, ...) {
      i <- func$getinv()
      if(!is.null(i)){
            message("getting cached data")
            i
      } else {
            data <- func$get()
            i <- solve(data, ...)
            func$setinv(i)
            i
      }
      
      ## Return a matrix that is the inverse of 'x'
}
