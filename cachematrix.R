## Ability to cache potentially time-consuming computations.
##  "If the contents of a vector are not changing, it may make sense to
##  cache the value of the mean so that when we need it again, it can
##  be looked up in the cache rather than recomputed." - R Programming (rprog-004)
##  by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD

## "makeCacheMatrix: This function creates a special 'matrix'
## object that can cache its inverse." - R Programming (rprog-004)
##  by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
test_that("makeCacheMatrix: This function creates a special 'matrix'
          object that can cache its inverse.",{
                  expect_that({
                          q1 <- matrix(c(2,1,3,2),ncol=2,nrow=2)
                          q2 <- makeCacheMatrix(q1)
                          q2$get()},
                          equals(
                                  matrix(c(2,1,3,2),ncol=2,nrow=2)))
                  expect_that({
                          q1 <- makeCacheMatrix()
                          q1$set(matrix(c(2,1,3,2),ncol=2,nrow=2))
                          q1$get()},
                          equals(
                                  matrix(c(2,1,3,2),ncol=2,nrow=2)))
                  expect_that({
                          q1 <- matrix(c(2,1,3,2),ncol=2,nrow=2)
                          q2 <- makeCacheMatrix(q1)
                          q2$getinverse()},
                          equals(
                                  NULL))
                  expect_that({
                          q1 <- matrix(c(2,1,3,2),ncol=2,nrow=2)
                          q2inverse <- solve(q1)
                          q2 <- makeCacheMatrix(q1)
                          q2$setinverse(q2inverse)
                          q2$getinverse()},
                          equals(
                                  matrix(c(2,-1,-3,2),ncol=2,nrow=2)))
                  expect_that({
                          q1 <- matrix(c(3,2,0,0,0,1,2,-2,1),ncol=3,nrow=3)
                          q2inverse <- solve(q1)
                          q2 <- makeCacheMatrix(q1)
                          q2$setinverse(q2inverse)
                          q2$getinverse()},
                          equals(
                                  matrix(c(0.2,-0.2,0.2,0.2,0.3,-0.3,0,1,0),ncol=3,nrow=3)))
                  })

## "cacheSolve: This function computes the inverse of the special
## 'matrix' returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache." - R Programming (rprog-004)
##  by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
cacheSolve <- function(x,...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
test_that("cacheSolve: This function computes the inverse of the special
'matrix' returned by makeCacheMatrix above. If the inverse has already
been calculated (and the matrix has not changed), then the cachesolve
should retrieve the inverse from the cache.",{
        expect_that({
                q1 <- makeCacheMatrix()
                q1$set(matrix(c(3,2,0,0,0,1,2,-2,1),ncol=3,nrow=3))
                cacheSolve(q1)
                cacheSolve(q1)},
                equals({
                        matrix(c(0.2,-0.2,0.2,0.2,0.3,-0.3,0,1,0),ncol=3,nrow=3)}))
})
