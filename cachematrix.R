## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## OK... not sure I'm verbalizing correctly what's going on but I'll try.
##1 Sets incoming matrix (x), sets inverse matrix (im) to NULL (e.g. clears cache for fresh matrix)
##2 Gets incoming matrix (x)
##3 Sets inverse matrix (invmat), 
##4 Puts inverse matrix (im) into gettable subfunction (accessible from cacheSolve())
##5 Creates accessible list of functions (getting, setting)
makeCacheMatrix <- function(x = matrix()) {
        ##1.1 Set incoming matrix as x
        ##1.2 Set inverse matrix (im) to NULL
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        
        ##2 Get matrix (x)
        get <- function()
                x
        
        ##3 Set invmat
        set_im <- function(invmat)
                im <<- invmat
        
        ##4 Create gettable subfunction for inv matrix (im)
        get_im <- function() im
        
        ##5 Set the names of subfunctions accessible from cacheSolve
        list(set = set,
             get = get,
             set_im = set_im,
             get_im = get_im)
}


## Write a short comment describing this function
##1 Get inv matrix (im) from makeCacheMatrix object
##2 Check if reusable im exists in cache; if yes, return it w a msg
##3 Else, get the new matrix
##4 Calculate the inverse matrix
##5 Commit the newly calc'd inverse matrix to cache
##6 Return newly calc'd inv matrix as output
cacheSolve <- function(x, ...) {
        ##1 Go find and get inv matrix using makeCacheMatrix object/function
        im <- x$get_im()
        
        ##2 Does an inv matrix already exist in the cache? 
        ##  If so, reuse the cached inv matrix
        if(!is.null(im)) {
                message("getting cached data")
                return(im) ## If from cache, program ends here, returns cached im
        }
        
        ##3 ELSE (e.g. no cached im bc new matrix), get the new matrix
        data <- x$get()
        
        ##4 Calculate the inverse matrix
        im <- solve(data, ...)
        
        ##5 Set inv matrix (im) with the calculated im
        ##  e.g. stuff the newly calc'd im into the cache
        x$set_im(im)
        
        ##6 Return the newly calculated im
        return(im)
}
