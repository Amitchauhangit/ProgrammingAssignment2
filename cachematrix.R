## Here the Function makeCaccheMatrix function is creating a list of sub functions associated 
## with a matrix(user input), these subFunctions can be used to get and set the matrix data(x)
## as well as its inverse (defined as inv)

## function makeCacheMatrix takes one arguement named x ( defaultly defined to empty matrix )
## contain one local variable inv to store inverse of matrix x , initially assigned to NULL.
## also contain a list of functions to access and update x and inv.
## sub functions get ,set , setinv, get inv.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<- function(y){
        x <<- y
        inv <<- NULL 
    }
    get<- function() x
    setinv<- function(z){
        inv <<- z
    }
    getinv<- function() inv
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## function cacheSolve takes one arguement which is actually a list returned by makeCacheMatrix()
## list attributes are the functions which is used to get and set the values of matrix and its 
## inverse. Ultimately the inverse of the matrix is cached in the above function which can be 
## accessed through those sub functions.

cacheSolve <- function(x, ...) {
    
    inv<-x$getinv()
    if(!is.null(inv))
    {
        message("getting cached data")
        return (inv)
    }
    mat<-x$get()
    inv<-solve(mat)
    x$setinv(inv)
    inv
        
        
        ## Return a matrix that is the inverse of 'x'
}
