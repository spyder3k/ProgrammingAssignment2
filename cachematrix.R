## DataScience practice 
## Assignament 2. Inverse of a Matrix

## This function is a "list" of operations availables
makeCacheMatrix <- function(mtx = matrix()) {
    # intializing variables
    inv <- NULL
    set <- function(param) {
        mtx <<- param
        inv <<- NULL
    }
    
    get <- function() x
    setInvMatrix <- function(inverse) inv <<- inverse
    getInvMatrix <- function() inv
    # "List" of operations or function availables
    list(set=set, get=get, setInvMatrix=setInvMatrix, getInvMatrix=getInvMatrix)
}


## This function verify if an operation has been executed
cacheSolve <- function(param, ...) {
    inv <- param$getInvMatrix()
    # operation has been executed?
    if(!is.null(inv)) {
        message("Your data has been cached.")
        return(inv)
    }
    data <- param$get()
    inv <- solve(data)
    param$setInvMatrix(inv)
    inv
}
