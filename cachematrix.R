## This little program aims to cache the inverse of a matrix. 

## Write a short comment describing this function
## The first function, 'makeCacheMatrix' creates a special "vector", which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<- function(inverse) m<<-inverse
    getinverse<-function() m
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## The following function will first check to see if the inverse has already been calculated. 
## If so, it will use 'get' to get the inverse of the matrix and won't go through the computation.
## Otherwise, it calculates the inverse of the matrix and sets the values of the inverse of the matrix in the cache.

cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if(!is.null(m)){
        message('getting cached data')
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
