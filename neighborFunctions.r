
##### Set up




#### functions

## param: takes a square distance matrix
## takes a matrix and makes it into a Q matrix 
## returns Q matrix, new node and dist from original to that node 

makeQ <- function(dist) {
  #get the size
  n <- nrow(dist)
  
  #get row and column sums
  rows <- rowSums(dist)
  cols <- colSums(dist)
  
  #set up Q return 
  Q <- matrix(data = NA, nrow = n, ncol = n)
  
  # runs through all rows 
  for (a in 1:n) {
    for(b in 1:n) { 
      
      #skip over diagonals 
      if(a == b) {
        Q[a,b] = 0
        
      #uses formula from wikipedia
      }else{
        
      Q[a,b] = (n-2)* dist[a,b] - rows[a] - cols[b]
      }
    }
  }

 return(Q)
}
  

## param: takes a Q matrix, and original distance matrix
## takes smallest element, and joins those nodes
## returns: distance from original nodes to new node

joinNode <- function(Q, dist) {
  
  #takes first location for min, returns row and col index
  Qmin <- which( Q == min(Q), arr.ind = TRUE)[1,]
  n <- nrow(dist)
  
  aDist <- dist[Qmin][1]
  
  rows <- rowSums(dist)
  cols <- colSums(dist)
  
  firstD <- .5* aDist + 1/(2*(n-2)) *(rows[1] - rows[2])
  secondD <- aDist - firstD
  
  return (list)
}

## Takes Q matrix, and original distance matrix, 
## and makes new matrix that is n-1 from dist size, adding in 
newDist <- function(Q, dist) {
  n <- nrow(dist)
  Qmin <- which( Q == min(Q), arr.ind = TRUE)[1,]
  a <- Qmin[2]
  b <- Qmin[1]
  
  #drops row and col to make smaller matrix
  newD <- dist[-b,-a]
  newN<- nrow(newD)
  diff <- nrow(dist) - nrow(newD)
  
  #update left behind row/col but leave other distances the same 
  for(c in 1:nrow(newD) ) {
    #add 1 to account for removed row
    newD[a, c] <- .5*(dist[a, c+1] + dist[b, c+1] - dist[a,b])
    newD[c,a] <- newD[a, c]
  }
  
  return(newD)
}


## *~*~*~*~*~ Main function 

neighborTree <- function(dist) {
  #set up first distance
  orignialDist <- dist
  newDist <- dist
  
  print(dist)
  #Generally, run until no more new nodes can be made. 
  while(nrow(newDist) > 2)  {
    #Get Q
    Q <- makeQ(newDist)
    print(Q)
    
    #nodes? 
    
    #get new distance 
    newDist <- newDist(Q, newDist)
    print(newDist)
  }
  
  
}

### wiki example matrix 

wiki <- matrix(
  c(0,5,9,9,8,
    5,0,10,10,9,
    9,10,0,8,7,
    9,10,8,0,3,
    8,9,7,3,0),
  ncol = 5,
  byrow = TRUE,
  dimnames = list(c('a','b','c','d','e'), c('a','b','c','d','e'))
)

firstOne<- makeQ(wiki)
firstOne

second <- newDist(Q= firstOne,dist = wiki)
second

secondQ <- makeQ(dist = second)
secondQ

third <- newDist(secondQ, second)
third

thirdQ <- makeQ(third)
thirdQ

fourth <- newDist(thirdQ, third)
fourth

neighborTree(wiki)
