## cache matrix, store the inverse with superassignement, call the result if it
## has been already calculated, if not it calculates and stores matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL  ## m will store our 'inverse' and it's reset to NULL
	set <-function(y){
	x << -y 
	m <<- NULL
  }
  get <- function(){x}
  setsolve <- function(solve)
  {m <<- solve} #store the value using superassignment
  getsolve <- function(){m}
  list(get=get, setsolve=setsolve,getsolve=getsolve)
}

## Call cache solve if already calculated if not it solves matrix

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
m<-x$getsolve() # accesses the object 'x' and gets the inverse
if(!is.null(m)){  # if mean was already cached (not NULL) 
  message("getting cache data")
  return(m) #return inverse
}
data<-x$get()
m<-solve(data,...)  # if m was NULL then we have to calculate the inverse
x$setsolve(m)
m #return inverse
}
