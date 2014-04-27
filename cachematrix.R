## -------------------------------------------------Peer Assignments - R Programming ---------------------------------------- ##
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## -------------------------------------------------------------------------------------------------------------------------- ##

## makeCacheMatrix function to cache (set & get) the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL ##setting NULL to m inside the function
  set<-function(y)
  {
    x<<-y ##assigning matrix to x globally
    m<<-NULL ##assigning NULL to m globally
  }
  get <- function() x ##getting the stored matrix
  setinverse <- function(inverse) m <<- inverse ##calculating the inverse of matrix
  getinverse <- function() m ##getting the inverse of matrix
  d<-list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse) ##listing the stored value of set,get,setinverse and getinverse.
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
  
  m <- x$getinverse() ##getting the inverse of given matrix.
  if(!is.null(m)) { ##validating whether given matrix is stored globally. 
    ##If yes then message "getting cached data" will be printed and return the inverse from cache
    message("getting cached data")
    return(m)
  }
  data <- x$get() ##matrix will be assigned to "data"
  m <- solve(data, ...) ##function to calculate inverse matrix.
  x$setinverse(m) ##inverse matrix is set globally by setinverse() function.
  m
}

## ------------------------------------------------- Sample execution --------------------------------------------------------------- ##

##Input a squared matrix to find the inverse
mat<-matrix(c(1,1,1,3,4,3,3,3,4),nrow=3,ncol=3)

##Call makeCacheMatrix which will return a list of matrix and its inverse
mat_list<-makeCacheMatrix(mat)

##For a given matrix, if the inverse has been calculated already then it will cache the data from memory
##Else cacheSolve function will calculate the inverse
mat_inverse<-cacheSolve(mat_list)

## ---------------------------------------------------- OUTPUT --------------------------------------------------------------------- ##
mat_inverse  ##Inverse Matrix will be displayed

##How to verify Inverse Matrix is right?
mat %*% mat_inverse ##If matrix resulted in Identical values in diagonal of matrix then its inverse matrix.