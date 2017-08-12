#In this example we introduce the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. Below are two functions that are used to create a special object that stores a numeric vector and cache's its mean.

#The first function, makeVector creates a special "vector", which is really a list containing a function to

#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

## The 1st finction store the Matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x<<-y
            inv<<-NULL
      }
      get<-function() x
      setinverse <- function(inverse) inv<<-inverse
      getinverse <- function()inv
      list (set=set, get = get, setinverse = setinverse, getinverse=getinverse)
}


# The 2nd function retrive the inverse of the matrix from the cache if it exists, 
# otherwise, it calculats it with the helmp of the 1st function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <-x$getinverse()
      if(!is.null(inv)){
            return(inv)
      }
      data<-x$get()
      inv<-solve(data,...)
      x$setinverse(inv)
      inv
}