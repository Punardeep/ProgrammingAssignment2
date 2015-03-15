# Author:Punardeep Singh (Punardeep.singh@gmail.com)
# makeCacheMatrix creates a special matrix object, and then cacheSolve 
# calculates the inverse of the matrix.
# If the matrix inverse has already been calculated, it will instead 
# find it in the cache and return it, and not calculate it again.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(myMatrix = matrix()) {
  Minv <-NULL
  set<-function(Y){
    myMatrix<<- Y
    Minv <<-NULL
  }
  get<-function() myMatrix
  setMatInv <-function(MatInverse) Minv <<- MatInverse
  getMatInv<-function() Minv
  mypseudoMatrix<<-list(get=get,setMatInv=setMatInv,getMatInv=getMatInv)
  mypseudoMatrix
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(get_setMatInv_getMatInv_of_myMatrix = mypseudoMatrix, ...) {
  ## Return a matrix that is the inverse of 'Get_SetMatInv_GetMatInv_of_myMatrix'
  Minv<- get_setMatInv_getMatInv_of_myMatrix$getMatInv()
  if(!is.null(Minv)){
    message("getting cached matrix inverse")
    return(Minv)    
  }
  DataMatrix<-get_setMatInv_getMatInv_of_myMatrix$get()
  Minv<-solve(DataMatrix,...)
  get_setMatInv_getMatInv_of_myMatrix$setMatInv(Minv)
  Minv
}

## Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 

