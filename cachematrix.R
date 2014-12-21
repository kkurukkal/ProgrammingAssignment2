## December 20th, 2014

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to 

## 1.set the matrix
## 2.get the value of the matrix
## 3.set the inverse of the matrix 
## 4.get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

      m <- NULL
      set <- function(y) {
         x <<- y
         m <<- NULL
      }

      get <- function() x
      setInv <- function(inv) m <<- inv
      getInv <- function() m
      list(set = set, get = get, 
              setInv = setInv,
              getInv = getInv)
}


## This function returns a matrix that is the inverse of 'x'. This is same as the example
## provided in the assignment to calculate the mean of the special "vector" created. Only difference is, as
## you can see below, it uses solve to calculate the inverse of the "matrix" created above.
## The below function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has already
## been calculated. If so, it gets the Inverse from the cache and skips the computation.
## Otherwise, it calculates the Inverse of the data and sets the value of the Inverse in
## the cache via the setInv function. I have also verified the function and provided the output below.

cacheSolve <- function(x, ...) {
            
     m <- x$getInv()
        if(!is.null(m)) {
           message("getting cached data")
           return(m)
        }
      
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
   m
}

## output from R using the above function is listed below

## > x <- matrix(c(2, -1, 1, 4, 1, 4, 1, -1, 0), nrow = 3, ncol = 3)
## > a <- makeCacheMatrix(x)
## > a$get()
##      [,1] [,2] [,3]
## [1,]    2    4    1
## [2,]   -1    1   -1
## [3,]    1    4    0
## > a$getInv()
## NULL
## > cacheSolve(a)
##      [,1] [,2] [,3]
## [1,]   -4   -4    5
## [2,]    1    1   -1
## [3,]    5    4   -6
## > a$getInv()
##      [,1] [,2] [,3]
## [1,]   -4   -4    5
## [2,]    1    1   -1
## [3,]    5    4   -6
## > 
