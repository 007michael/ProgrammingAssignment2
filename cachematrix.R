## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    } #设置矩阵值
    
    get <- function() x #获取矩阵值
    
    setSolve <- function(solve) m <<- solve #设置逆矩阵值
    
    getSolve <- function() m #获取逆矩阵值
    
    list(set = set, get = get, 
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve() #获取逆矩阵值
    
    if(!is.null(m)) { #如果缓存中逆矩阵值
        message("getting cached data")
        return(m)
    }
    
    #如果缓存中没有值，则设置逆矩阵值，并缓存
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}