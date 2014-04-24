
##For a more detailed explanation of how
#these functions work scroll down, examples were included.


makeCacheMatrix <- function(x = matrix()) 
{
   
      #Creates a list containing 4 functions:
      #set(): Set the value of the matrix 
      #get(): Get the value of the matrix
      #setInv(): Set the value of the inverse
      #getInv(): Get the value of the inverse
      
      m <- NULL
    
      set <- function(y) { x <<- y; m <<- NULL }
      
      get <- function() x
      
      setInv <- function(Inv) m <<- Inv
      
      getInv <- function() m
            
      list(set = set, get = get, setInv = setInv,
      getInv = getInv)
      
}


cacheSolve <- function(x, ...) 
{
 
      #Calculates the inverse of the matrix created
      #by the function makeCacheMatrix() and sets its 
      #value in the cache using the setInv() function.
      #If the inverse has already been calculated it 
      #will simply get it from the cache.
      
      
      m <- x$getInv()
      
      if(!is.null(m)) 
      {
            
            message("getting cached data")
            return(m)
      }

      data <- x$get()

      m <- solve(data, ...)

      x$setInv(m)

      m
      
}


##DETAILED NOTES AND EXAMPLES:

#Calls the function makeCacheMatrix(), leaving x with its default value (an
#empty matrix) and assigns the list of functions that are returned by the 
#function makeCacheMatrix() to a variable called "FunList":

FunList <- makeCacheMatrix()

#When we call the function get(), it looks for the free variable "x" in the
#environment in which the function get() was defined, that is the environment
#of the function makeCacheMatrix(), called in line #67.
#It returns the same object that was given as an argument for the function
#makeCacheMatrix(), an empty matrix: 

FunList$get()

#We can take a look at its environment and look for the value of "x" too, 
#which shows us that the function is looking for the value of x in the 
#environment of the function makeCacheMatrix():

makeCacheMatrix.get <- FunList$get

ls(environment(makeCacheMatrix.get))

get("x", environment(makeCacheMatrix.get))

#If we call the function set() and give it a new matrix as an argument, say
#a 2 by 2 matrix, what we are doing is reassigning the value of x,
#this time within the environment of the function set(), thus the next time
#we call the function get() it will return the new matrix:

FunList$set(y = matrix(1:4, 2, 2))  

#Returns nothing, but changes the environment in which R is going to look for
#the variable x.

FunList$get() #Now we get the new matrix

#We can also confirm this by looking at the "x" object that is in the 
#get() function's environment:

get("x", environment(makeCacheMatrix.get))

#The same process applies to the other two functions, getInv() and setInv().
#For the function getInv(), the variable "m" is a free variable, which is 
#initially defined as NULL within the makeCacheMatrix() function's environment:

FunList$getInv() 

#Returns NULL, "m" was defined as a local variable in the function 
#makeCacheMatrix(). If we look for the object "m" within the environment of 
#the function getInv() we will see that its value is NULL, as defined by 
#the function makeCacheMatrix(), called in line #67:

makeCacheMatrix.getInv <- FunList$getInv

get("m", environment(makeCacheMatrix.getInv))

#If we call the function setInv() and give it as an argument the inverse of the
#matrix we are working with, what we are doing is reassigning the value of m
#this time within the environment of the function setInv(), thus the next time
#we call the function getInv() it will return the new value for m:

FunList$setInv(Inv = solve(FunList$get()))  

#Returns nothing, but changes the environment in which R is going to look for
#the variable m.

FunList$getInv() 

#We can also confirm this by looking at the "m" object that is in the 
#getInv() function's environment:

get("m", environment(makeCacheMatrix.getInv))

#All this without cluttering the gloval environment with the values for
#the x and m variables:

ls()

#Now if we call the cacheSolve() function, and we have already defined the 
#inverse of our matrix (within the environment of the function setInv), R won't
#have to compute the inverse again and will just look for it  in the getInv() 
#function's environment:

cacheSolve(FunList) 

#If the inverse is being computed for the first time by the cacheSolve() 
#function, it will assign the inverse to the variable "m" within the getInv()
#function's environment

###
#To wrap this up, here's a final example with a large matrix:

FunList <- makeCacheMatrix() 

#Set x to be this big square matrix
FunList$set(matrix(sample(1:1E6), 1E3, 1E3))

#Compute the inverse for the first time using cacheSolve(). The function
#will also assign it to the variable "m", within the setInv() function's
#enviroment:

cacheSolve(FunList)[1:10, 1:3]

#If we call this again, the function won't compute the inverse and will
#return the chached inverse:

cacheSolve(FunList)[1:10, 1:3]
