
## create and cache inverse of a matrix.  Define functions to get and store values.  

makeCacheMatrix <- function(x = matrix()) {
  #defined in workspace
  inv<- NULL #cached inverse
  
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  get<-function()x
  #assign function
  setinv<-function(solve)inv<<-solve
  getinv<-function()inv
  #create a list of functions defined in makeCacheMatrix
  list(set=set, get=get, 
       setinv=setinv,
       getinv=getinv)
}

##Return a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
  inv<-x$getinv()  
  
#searches for and gets value if currently stored

  if(!is.null(inv))   {
    message("getting cached data")
#returns inverse of either computed or stored values   
    return(inv)
    
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}
  
  
  ## 
