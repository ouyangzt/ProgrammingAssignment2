## I wrote three funnctions here, The first one makeCacheMatrix
#creates a special “matrix” object that can cache its inverse.
#The second one cacheSolve solve the inverse of such spectial "matrix" from the return of the first function
# if the inverse already exist, get from the cache, otherwiteh solve the inverse and set the cache
# The third function is a test funtion of the cache by compring the time difference between the first run
#of cacheSolve (inverse of the matrix not exist yet) and second run of cacheSolve (the inverse has been cached)



makeCacheMatrix <- function(x = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)) {
    #inv to store in inverse of matrix x temporally
    inv <- NULL
    set <- function(y) {
        #y is a invertable matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) inv <<- Inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##The cacheSolve function creates a special "vector", 
#which is a list containing 4 functions:
#1,set the value of the matrix
#2,get the value of the vector
#3,set the value of the mean
#4,get the value of the mean

cacheSolve <- function(x, ...) {
    #x is the returen from function makeCacheMatrix
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached Inverse")
        return(inv)
    }
    data <- x$get()
    message("computiing the Inverse")
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}


##The test funciton takes in any invertible matrix, 
#calculates its inverse twice using the above functions, 
#and prints out the times it takes for both runs. 
#The first run should take longer than the second run because it actually calculates the inverse 
#while the second run only does a look-up from the cached inverse
test = function(mat){
    ## @mat: an invertible matrix
    
    temp = makeCacheMatrix(mat)
    
    start.time = Sys.time()
    cacheSolve(temp)
    dur = Sys.time() - start.time
    print(dur)
    
    start.time = Sys.time()
    cacheSolve(temp)
    dur = Sys.time() - start.time
    print(dur)
}

set.seed(1000)
mat1 = matrix(rnorm(1000000), nrow=1000, ncol=1000)
test(mat1)
### we'll see a signficant time difference between getting inverse from the cache and calculating the inverse