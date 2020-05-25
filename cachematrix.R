## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix creates a new environment where the matrix is passed to
#cacheSolve takes the inititalized list as argument and returns the inverse




## Write a short comment describing this function

#Input is matrix to find reverse
#Input matrix is saved to a variable x,m which will later hold the inverse is initialized to NULL
#Returns a list of functions to initialize and set values of input and inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                 #inverse is declared as NULL initlially
  set <- function(y){                       #function to set value of x to matrix and m to NULL
    x <<- y
    m <<- NULL
  }
  get <- function() x                      #function to return value of input matrix
  setSolve <- function(inverse) m <<- inverse       #function to set value of m(inverse is passed here later)
  getSolve <- function() m                          #function to return value of matrix(inverse found and cached will be returned)
  list(set=set,get=get,setSolve=setSolve,getSolve=getSolve)

}


## Write a short comment describing this function
#Input is list which is output of makecacheMatrix and all the inputs to solve function in r 
#If variable to store inverse is  not NULL returns cached value of inverse
#Else find and stores the value of inverse of stored matrix to m and returns this value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'  #Input is list which is output of makeCacheMatrix and other arguments to solve function in r 
  m <- x$getSolve()                                   #Get and check if value of
  if(!is.null(m)){                                    #inverse is stored or not
    message("getting cached data")                    #Return stored value if m was not calculated before
    return(m)                                         
  }
  data <- x$get()                                    #Else find inverse of input and set the value of inverse
  m <- solve(data,...)                               #Return the inverse of matrix
  x$setSolve(m)
  m
}
