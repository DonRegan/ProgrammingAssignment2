## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    x_inv<- NULL
    set_mat<- function(y){
        x<<- y
        x_inv<<-NULL #set inverse to null initially
    }
    get_mat<- function(){
        x
    } 
    set_inverse<- function(inv){#once we've the inverse we set it here
      x_inv<<-inv
    }
    get_inverse<- function(){#can get the inverse from the cache using this
      x_inv
    }
    ##now create the special "matrix" object which is really
    ##a list with a function to set the matrix, get it, set the inverse, get the inverse
    list(set_mat=set_mat,get_mat=get_mat,set_inverse=set_inverse,
         get_inverse=get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv<-x$get_inverse() #on a first pass this will be null
  if(!is.null(x_inv)){
      message("getting cached matrix inverse")
      return(x_inv)
  }
  data<-x$get_mat()
  x_inv<-solve(data,...)
  x$set_inverse(x_inv)
  x_inv
}
