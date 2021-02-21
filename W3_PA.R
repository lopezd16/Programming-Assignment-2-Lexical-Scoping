

makeCacheMatrix <- function(d = matrix()) {
                          a <- NULL
                          set <- function(b){
                            d <<- b
                            a <<- NULL
                          }
                      get <- function()d
                      setInverse <- function(inverse) a <<- inverse
                      getInverse <- function() a 
                      list(set = set, get = get, 
                           setInverse = setInverse, 
                           getInverse = getInverse)
}

cacheSolve <- function(d, ...) {
                  a <- d$getInverse()
                  if(!is.null(a)){
                    message("waiting cached data")
                    return(a)
                  }
                  mat <- d$get()
                  a <- solve(mat,...)
                  d$setInverse(a)
                  a
}