#makeCacheMatrix function initializes the cache value of 
# "i" i.e. the inverted matrix to NULL. 
#When setinverse() function is called by the function 'cacheSolve()'
#this function sets the value of "i" to the latest value calculated 
#for the matrix x
#getinverse function returns the cached value of i to the 'cacheSolve()' 
#function
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #initialize cache value to NULL
  set <- function(y) {
    x <<-y
    i <<- NULL
  }
  get <- function() x #to read the current matrix into memory
  setinverse <- function (solve) i <<- solve #set the cache value to the last calculated value
  getinverse <- function () i #function to read the cache value of "i"
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#cacheSolve function calculates the inverse "i" of the matrix "x". 
#It checks the cache to see if the value has been calculated before
#i.e.check the cache value for not null. If the cache value is null 
#then the function calculates the inverse and writes it to the 
#cache, otherwise it retrieves the cached value.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
#First look up the cache to find the value for "i" i.e. the inverse of matrix
  i <- x$getinverse()
#For the first run, as "i" has not been calculated before, its value in cache is set to null (makeCachematrix - i<-NULL)
#so the first run skips the below 'if' loop
#For the second run if the same matrix is passed then the value for "i" is retrieved from the cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
#x$get simply retrieves the matrix to be inverted
  data <-x$get()
#solve(data) function inverts the matrix stored in 'data'
  i <-solve(data, ...)
#The below function sets the latest calculated value of "i" in the cache
  x$setinverse(i)
#return the inverted matrix to the function call
  i 
}
