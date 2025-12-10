
##### Set up




#### functions

## param: takes a square distance matrix
## takes a matrix and makes it into a Q matrix 
## returns Q matrix 

makeQ <- function(dist) {
  #get the size
  n <- nrow(dist)
  
  #get row and column sums
  rows <- rowSums(dist)
  cols <- colSums(dist)
  
  #set up Q return 
  Q <- matrix(data = NA, nrow = n, ncol = n)
  row.names(Q) <- row.names(dist)
  colnames(Q) <- colnames(dist)
                   
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
## takes smallest element, and joins those nodes, calculates distances
## returns: distance from original nodes to new node,
## to a larger as a dataframe that can be added 

joinNode <- function(Q, dist) {
  
  #takes first (smallest) location for min, returns row and col index
  Qmin <- which( Q == min(Q), arr.ind = TRUE)[1,]
  n <- nrow(dist)
  
  a <- Qmin[1]
  b <- Qmin[2]
  
  abDist <- dist[a,b]
  
  # Nodes 
  nodeA <- rownames(dist)[a]
  nodeB <- rownames(dist)[b]
  
  #row/col sums
  rows <- rowSums(dist)
  cols <- colSums(dist)
  
  #edge distance - from wikipedia
  firstD <- .5* abDist + 1/(2*(n-2)) *(rows[a] - rows[b])
  secondD <- abDist - firstD
  
  newNode <- paste(nodeA, "_",nodeB, sep = '')
  
  #edges that have been made 
  nodeDF <- data.frame(parent = c(newNode, newNode),
                       child = c(nodeA, nodeB),
                       distance = c(firstD, secondD))
  
  return(nodeDF)
}

## params: Takes Q matrix, and original distance matrix, 
## and makes new matrix that is n-1 from dist size, adding in 
## returns the new distance matrix 
newDist <- function(Q, dist) {
  
  #get smallest distance, and return what nodes those are 
  n <- nrow(dist)
  Qmin <- which( Q == min(Q), arr.ind = TRUE)[1,]
  a <- Qmin[1]
  b <- Qmin[2]
  
  #Get new names
  nodeA <- rownames(dist)[a]
  nodeB <- rownames(dist)[b]
  newNode <- paste(nodeA, "_",nodeB, sep = '')
  
  #new distances
  newLength <- c()
  oldNames <- rownames(dist)[-c(a,b)]
  
  for (i in oldNames) {
    ind <- which(rownames(dist) == i) 
    newLength <- c(newLength, .5* (dist[a, ind] + dist[b, ind] - dist[a,b]))
  }
  
  #build out distance matrix 
  
  newD<- matrix(0, n - 1, n - 1)
  rownames(newD) <- c(newNode, oldNames)
  colnames(newD) <- c(newNode, oldNames)
  
  #new dist
  newD[1, -1] <- newLength
  newD[-1, 1] <- newLength
  
  #old dist
  oldDist <- dist[-c(a,b), -c(a,b)]
  newD[-1,-1] <- oldDist
  
  
  return(newD)
}


# graph output from the tree

graphTree <- function(tree, vsize = 20) {
  library(igraph)
  
  g <- graph_from_data_frame(tree, directed = FALSE)
  
  #add distances to plot
  E(g)$label <- round(E(g)$distance, 3)   
  
  plot(
    g,
    vertex.size = vsize,
    edge.arrow.mode = 0,                  
    layout = layout_as_tree(g, root = V(g)[1]) 
  )
  
  
}



## *~*~*~*~*~ Main function 

neighborTree <- function(dist) {
  #set up first distance
  orignialDist <- dist
  newD <- dist
  
  # #Set up matrix to hold final tree info
   tree <- data.frame(
     parent = character(),
     child = character(), 
     distance = numeric()
   )
  # 
  

  
  print(dist)
  #Generally, run until no more new nodes can be made. 
  
  while(nrow(newD) > 2)  {
    #Get Q
    Q <- makeQ(newD)
    
    #nodes 
    joinInfo <- joinNode(Q, newD)
    tree <- rbind(tree, joinInfo)
    
    print(tree)
    
    #get new distance 
    newD <- newDist(Q, newD)
    
    print(newD)
  }
  
  #add last nodes
  lastNodes <- rownames(newD)
  lastDist <- newD[1,2]
  newNode <- paste(lastNodes[1],"_",lastNodes[2], sep = '')
  
  tree<- rbind(tree, 
               data.frame(parent=newNode, child=lastNodes[1], distance=lastDist),
               data.frame(parent=newNode, child=lastNodes[2], distance=lastDist))
  
  return(tree)
  
  
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

firstNewNode <- joinNode(firstOne, wiki)
firstNewNode

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

tree <- neighborTree(wiki)

graphTree(tree)
