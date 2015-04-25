## Create a cache matrix object that can be use to repeatably solve the inverse of the matrix, but only
## calculates the inverse once.
##
## Usage:
## M <- matrix (c(1,2,3,4), nrow=2, ncol =2)
## cacheMatrix <- makeCacheMatrix(M)
##
## caheMatrix$set(M)      # change the matrix being cached.
## M <- cacheMatrix$get() # Returns the matrix being cached.
##
## cacheMatrix$setInverse(solve(data, ...))  # Private function containing cached inverse of x
## cacheMAtrix$getInverse()                  # Private function used to get the ached invserse of x

## The fisrt function, makeVector creates a special "vector", which is rally a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y) {
         x <<- y
         m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## The following function calculates the mean of the special "vector" created with the above function. 
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean 
## in the cache via the setmean function.

cacheSolve <- function(x, ...) {
         m <- x$getmean()
         if(!is.null(m)) {
                message("getting cached data")
                return(m)
         }
         data <- x$get()
         m <- solve(data) %*% data
         x$setmean(m)
         m
        ## Return a matrix that is the inverse of 'x'
}
