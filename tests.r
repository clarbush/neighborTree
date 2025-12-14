context("Check to see if trees work")
source("neighborFunctions.R")

library(testthat)
library(ape)
library(igraph)

## testing example, since wikipedia also has correct outputs for every step 
wiki <- matrix(
  c(0,5,9,9,8,
    5,0,10,10,9,
    9,10,0,8,7,
    9,10,8,0,3,
    8,9,7,3,0),
  ncol = 5,
  byrow = TRUE,
  dimnames = list(
    c('a','b','c','d','e'),
    c('a','b','c','d','e')
  )
)


## example first Q from wikipedia - mainly just as a reference 
wikiFirstQOutput <- matrix(
  c(0,-50,-38,-34,-34,
    -50,0,-38,-34,-34,
    -38,-38,0,-40,-40,
    -34,-34,-40,0,-48,
    -34,-34,-40,-48,0),
  ncol = 5,
  byrow = TRUE,
  dimnames = list(
    c('a','b','c','d','e'),
    c('a','b','c','d','e')
  )
)

##############################################################################
## Make Q tests

test_that("MakeQ should return correctly, at least in structure", {
  Q <- makeQ(wiki)
  
  #Square matrix, with same names on rows and cols 
  expect_true(is.matrix(Q))
  expect_equal(dim(Q), dim(wiki))
  expect_equal(rownames(Q), rownames(wiki))
  expect_equal(colnames(Q), colnames(wiki))
  
  #zero on diagonals
  expect_true(all(diag(Q) == 0))
  
})

test_that("makeQ output matches wiki output", {
  Q <- makeQ(wiki)
  
  # From wikipedia first output, should be -50 lowest
  expect_equal(Q["a","b"], -50)
  expect_equal(Q["b","a"], -50)
  
  #make sure highest is also what i'd expect 
  expect_equal(Q["e","a"], -34)
  expect_equal(Q["a","e"], -34)
})

##############################################################################
## Join Node tests

test_that("joinNode returns as expected (dataframe, three lists)", {
  Q <- makeQ(wiki)
  edges <- joinNode(Q, wiki)
  
  expect_s3_class(edges, "data.frame")
  
  #each should have two new edges, with 2 parents 
  expect_equal(nrow(edges), 2)
  
  #all three named 
  expect_true(all(c("parent","child","distance") %in% names(edges)))
})


test_that("joinNode joins the correct nodes from wiki example with right distances", {
  Q <- makeQ(wiki)
  edges <- joinNode(Q, wiki)
  
  #same as above, should be -50 in a,b
  expect_true(all(edges$child %in% c("a","b")))
  expect_true(all(edges$parent == "b_a"))
  
  # distances should not be further away or closer than original distances 
  original <- wiki["a","b"]
  expect_equal(sum(edges$distance), original)
  
  #exact values pulled from wiki 
  expect_equal(edges$distance[1], 3)
  expect_equal(edges$distance[2], 2)
})

##############################################################################
## New Dist tests

test_that("newDist drops one row off Q", {
  Q <- makeQ(wiki)
  newD <- newDist(Q, wiki)
  
  expect_equal(nrow(newD), nrow(wiki) - 1)
  expect_equal(ncol(newD), ncol(wiki) - 1)
})


##CHANGE THIS IF I FIGURE OUT BETTER NODE NAMES 
test_that("newDist has new node names", {
  Q <- makeQ(wiki)
  newD <- newDist(Q, wiki)
  
  expect_true("b_a" %in% rownames(newD))
})

test_that("newDist still upholds distance matrix expectations ", {
  Q <- makeQ(wiki)
  newD <- newDist(Q, wiki)
  
  expect_equal(newD, t(newD))
  expect_true(all(diag(newD) == 0))
})


##############################################################################
# Main function tests

test_that("neighborTree returns a valid tree data frame", {
  tree <- neighborTree(wiki)
  
  expect_s3_class(tree, "data.frame")
  expect_true(all(c("parent","child","distance") %in% names(tree)))
})

test_that("neighborTree produces correct number of edges", {
  n <- nrow(wiki)
  tree <- neighborTree(wiki)
  
  # should be 2n - 2 edges
  expect_equal(nrow(tree), 2 * n - 2)
})




