##This is the submission for Coursera Week 3 assignment

##This function was created in lieu of the vector example given in the prompt 
##It is an input to be able to create a special "matrix" and set the inverse of that matrix as well

##setting the value of the matrix
##null will set place for a future value
##we are setting the function "r" to a new matrix "d" and it will reset the inverse to NULL
makeCacheMatrix <- function(d = matrix()) { 
  inv <- NULL                                          
  set <- function(r) {  
    d <<- r
    inv <<- NULL
  }
  
##here we will be setting the value of the matrix 
##we will also be getting the value of the matrix
##setting the value of the inverse matrix
##getting the value of the inverse matrix
  get <- function() d  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This second function will take the results of the inverse matrix via the above function. 
cacheinverse <- function(d, ...) {
  inv <- d$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- d$get()
  inv <- solve(matrix_to_invert, ...)
  d$setinverse(inv)
  inv
}

##Here are my results using a 3x3 matrix 
 my_Matrix <- makeCacheMatrix(matrix(c(1, 2, 4, 3, 4, 2, 6, 2, 4), 3, 3))
 my_Matrix$get()
     [,1] [,2] [,3]
[1,]    1    3    6
[2,]    2    4    2
[3,]    4    2    4
> my_Matrix$getinverse()
NULL
> cacheinverse(my_Matrix)
     [,1]       [,2]        [,3]
[1,] -0.2  0.0000000  0.30000000
[2,]  0.0  0.3333333 -0.16666667
[3,]  0.2 -0.1666667  0.03333333
> cacheinverse(my_Matrix)
getting cached data
     [,1]       [,2]        [,3]
[1,] -0.2  0.0000000  0.30000000
[2,]  0.0  0.3333333 -0.16666667
[3,]  0.2 -0.1666667  0.03333333

##Any feedback will help!
