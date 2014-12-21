##my code, that creates the inverse of any nonsingular square matrix

## mtx <- makeCacheMatrix(matrix(1:4,2,2))    (remove first "##" so you can begin testing with predefined sample matrix) 
                                              ##Sample matrix: "matrix(1:4,2,2)" defined and assigned to "mtx", to demostrate the code
                                              ##Certainly can be changed, but be aware of changing it to the same at the 
                                              ##bottom, when applying it into the "cacheSolve(mtx)"

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
                setmatrix=setmatrix,
                getmatrix=getmatrix)
}

## Returns the invers of the at-the-top defined matrix, and (as described below) if it's 
##coming from cache, it lets us know about it

cacheSolve <- function(x, ...) {
        m<-x$getmatrix()              
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}                                     

##copied twice to get the "getting cached data" in use.
##If applied only once, than the the message will not be displayed since the first gets the computed inverse, 
##but when the second is called, it's already pulled from cache, so is the message displayed

cacheSolve(mtx) 
cacheSolve(mtx)         
