## The overall aim of the following two functions is to calculate the inverse of a matrix
## if it has not been calculated before, and then store it in cache; if the inverse of a 
## matrix has been calculated before (and the matrix has not changed), however, there is 
## no need to calculate the inverse again, so in this case the previously cached value is
## retrieved and returned (thus saving computation time).


## The function makeCacheMatrix() takes in as argument a matrix to be inversed, and sets 
## up 4 functions used for getting and setting the values of the original matrix and its
## inverse. It is important to point out that if the function is called and the result of
## the call is stored in an object, the object will not only store the 4 funcions, but  
## also the value of the original matrix, as well as the value of its inverse - if it has
## already been calculated. This is due to the lexical scoping rules of R.

makeCacheMatrix <- function(x = matrix()) { ## takes in a matrix as argument
  i <- NULL ## the inverse is set to null each time the function is called
  
  set <- function(y) { ## this is a function to set the value of the matrix whose inverse
    x <<- y            ## we are looking for
    i <<- NULL
  }
  get <- function() x ## this is a function to get the value of the original matrix
  
  setinverse <- function(inverse) i <<- inverse ## this is a function to set the value of
                                                ## the inverse (will be called by cacheSolve)
  getinverse <- function() i ## simply returns the value of the inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## the output of the function is a list with 4 functions
                                ## that will be called by cacheSolve (except for set())
}



## The function cacheSolve() takes in an object that was used to store a call of the   
## previous function. In case the inverse of the matrix has already been calculated,   
## then it is returned. If the inverse has not been calculated, then it is performed 
## and the result is stored in cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ## gets the value of the inverse
  if(!is.null(i)) {
    message("getting cached data")
    return(i) ## if the inverse is already there, it is returned and the function ends
  }
  data <- x$get() ## else the original matrix is retrieved by the get() function
  i <- solve(data, ...) ## the inverse is calculated...
  x$setinverse(i) ## ...and the inverse is stored, so that on a next run it need not
                  ## be calculated again.
  i ## Return a matrix that is the inverse of 'x'
}
