## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The first function, `makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to
#
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

## example
# x=rbind(c(1,7,2),c(3,1,2),c(2,1,5))
# m=makeCacheMatrix(x)
# m$get()
#    [,1] [,2] [,3]
# [1,]    1    7    2
# [2,]    3    1    2
# [3,]    2    1    5


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function
# The following function 'cacheSolve' returns the inverse of the matrix.
# First it will check to see if the inverse of the matrix has already been
# calculated. If so it will skip the computations and will directly give result
# from cache. Otherwise it calculates the inverse of the matrix and sets the value
# in the cache via the 'setinverse' functions.

## example
# x=rbind(c(1,7,2),c(3,1,2),c(2,1,5))
# m=makeCacheMatrix(x)
# m$get()
#    [,1] [,2] [,3]
# [1,]    1    7    2
# [2,]    3    1    2
# [3,]    2    1    5

#cacheSolve(m)  ==> First run
#    [,1]        [,2]        [,3]
#[1,] -0.04166667  0.45833333 -0.16666667
#[2,]  0.15277778 -0.01388889 -0.05555556
#[3,] -0.01388889 -0.18055556  0.27777778

#cacheSolve(m)  ==> Second run, retrieving data from cache
#getting cached data
#    [,1]        [,2]        [,3]
#[1,] -0.04166667  0.45833333 -0.16666667
#[2,]  0.15277778 -0.01388889 -0.05555556
#[3,] -0.01388889 -0.18055556  0.27777778

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <-x$getinverse()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
