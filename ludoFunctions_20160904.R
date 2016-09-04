

#Roll dice 
setwd("C:/Users/testadmin/Desktop/Udacity/Ludo")

pawnPositionsPlayer_1<-c(2,11,5,3)
pawnPositionsPlayer_2<-c(1,1,1,3)

transMatrixPlayer_1<-readTransitionMatrix('trans1.csv')
transMatrixPlayer_2<-readTransitionMatrix('trans2.csv')

rollDice <-function(){
  return(sample(1:6, size = 1, replace = TRUE))
}

orderPawnsToMove <- function(){
  #assign random order for pawns to play in turn
  indices <- sample(1:4, size = 4, replace = FALSE)
  return(indices)
}

getPositionVector <-function(pawnID,pawnPositionsOfPlayer,maxPos){
  #creates vector with length equal to maxPos
  #vector is later used in markov chain to move pawn across the board
  positionVector<-rep(0,maxPos)
  
  #assign one where pawn is located
  positionVector[pawnPositionsOfPlayer[pawnID]]<-1
  
  return(positionVector)
}

readTransitionMatrix <-function(fileName){
  #This function reads the transition matrix
  df<- read.csv(fileName,header=FALSE,sep=',')
  trans1<-as.matrix(df)
  return(trans1)
}

movePawn <- function(transMatrix,positionVector,numSteps){
  #this function move a selected pawn across the borad if possible
  #the transition matrix is used in combination with the position vector
  for (i in 1:numSteps){
    positionVectorNew<-positionVector %*% transMatrix
    positionVector<-positionVectorNew  
  } 
  # if move impossible then vector with zeros is returned
  return(positionVector)
}

updatePawnPositions <-function(pawnPositions,positionVector,pawnID){
  #this function will update the pawn positions
  #inputs are the original position and positionvector of the move pawn
  pawnPositions[pawnID]<-which(positionVector==1)
  return(pawnPositions)
  
}

executeTurn <- function(numSteps,pawnPositions,transMatrix,orderToMove=NULL){
  #this function executes a turn
  #it will move a random pawn across the board
  #if unable to move pawn then next pawn is tried
  #output is new pawnPosition for the player
  
  # random select which pawn we try to move
  if(is.null(orderToMove)) orderToMove<-orderPawnsToMove()
  
  for (i in 1:4){
    
    #take i from random list
    pawnID<-orderToMove[i]
    positionVector<-getPositionVector(pawnID,pawnPositions,20)
    
    #move the pawn across the board
    positionVectorNew<-movePawn(transMatrix,positionVector,numSteps)
    
    # if pawn was moved successfully then exit for loop, else try next pawn
    if (any(positionVectorNew==1)) break
  }
  
  #if move was possible then update position,else keep old position
  if (any(positionVectorNew==1)){
    pawnPositionsNew<-updatePawnPositions(pawnPositions,positionVectorNew,pawnID)  
  } else {
    pawnPositionsNew<-pawnPositions
  }
  
  return(list(pawnPositionsNew,pawnID))
}

updateBoard <-function(pawnPositionsPlayer_1,pawnPositionsPlayer_2,playerInTurn,pawnID){
  #This function updates the board after a turn of a player where pawn with pawnID is moved
  
  if (playerInTurn==1){
    # move pawn player 2 back to base
    indices<-which(pawnPositionsPlayer_2==pawnPositionsPlayer_1[pawnID])  
    if (any(indices>0)) pawnPositionsPlayer_2[indices[1]]<-11
    
    else{
      indices<-which(pawnPositionsPlayer_1==pawnPositionsPlayer_2[pawnID])  
      if (any(indices>0)) pawnPositionsPlayer_1[indices[1]]<-1
    }
  }
  
  return(list(pawnPositionsPlayer_1,pawnPositionsPlayer_2))
}

isGameOver<-function(pawnPositionsPlayer_1,pawnPositionsPlayer_2,positionsHome){
  #function to determine whether game has ended true/false
  
  if (all(pawnPositionsPlayer_1==positionsHome[1])){
    gameOver<-TRUE
    winner<-1
  } else if (all(pawnPositionsPlayer_2==positionsHome[2])){
    gameOver<-TRUE
    winner<-2
  } else{
      gameOver<-FALSE
      winner<-NULL
  }
  return(list(gameOver,winner))
}




#Testing functions
test_isGamesOver2<-function(){
  pawnPositionsPlayer_2<-c(18,11,6,7)
  pawnPositionsPlayer_1<-c(19,19,19,19)
  positionsHome<-c(19,9)
  output<-isGameOver(pawnPositionsPlayer_1,pawnPositionsPlayer_2,positionsHome)
  exp_output<-list(TRUE,1)
  return(identical(output,exp_output))
}  

test_isGamesOver1<-function(){
  pawnPositionsPlayer_2<-c(18,11,6,7)
  pawnPositionsPlayer_1<-c(5,8,3,9)
  positionsHome<-c(19,9)
  output<-isGameOver(pawnPositionsPlayer_1,pawnPositionsPlayer_2,positionsHome)
  exp_output<-list(FALSE,NULL)
  return(identical(output,exp_output))
}

test_updateBoard3<-function(){
  pawnPositionsPlayer_2<-c(18,11,6,7)
  pawnPositionsPlayer_1<-c(5,8,3,9)
  playerInTurn<-1
  pawnID<-3
  
  output<-updateBoard(pawnPositionsPlayer_1,pawnPositionsPlayer_2,playerInTurn,pawnID)
  exp_output<-list(pawnPositionsPlayer_1,c(18,11,6,7))
  
  return(identical(output,exp_output))
}

test_updateBoard2<-function(){
  pawnPositionsPlayer_2<-c(18,11,3,3)
  pawnPositionsPlayer_1<-c(5,8,3,9)
  playerInTurn<-1
  pawnID<-3
  
  output<-updateBoard(pawnPositionsPlayer_1,pawnPositionsPlayer_2,playerInTurn,pawnID)
  exp_output<-list(pawnPositionsPlayer_1,c(18,11,11,3))
  
  return(identical(output,exp_output))
}

test_updateBoard1<-function(){
  pawnPositionsPlayer_2<-c(18,11,5,3)
  pawnPositionsPlayer_1<-c(5,8,3,9)
  playerInTurn<-1
  pawnID<-3
  
  output<-updateBoard(pawnPositionsPlayer_1,pawnPositionsPlayer_2,playerInTurn,pawnID)
  exp_output<-list(pawnPositionsPlayer_1,c(18,11,5,11))
  
  return(identical(output,exp_output))
}

test_executeTurn1<-function(){
  numSteps<-3
  pawnPositions<-c(18,11,5,3)
  transMatrix<-readTransitionMatrix('trans1.csv')
  orderToMove<-c(1,2,3,4)
  output<-executeTurn(numSteps,pawnPositions,transMatrix,orderToMove)
  exp_output<-list(c(18,15,5,3),2)
  return(identical(output,exp_output))
}

test_executeTurn2<-function(){
  # no move possible so start position is returned
  numSteps<-100
  pawnPositions<-c(18,11,5,3)
  transMatrix<-readTransitionMatrix('trans1.csv')
  orderToMove<-c(1,2,3,4)
  output<-executeTurn(numSteps,pawnPositions,transMatrix,orderToMove)
  exp_output<-list(c(18,11,5,3),4)
  return(identical(output,exp_output))
}

test_updatePawnPositions<-function(){
  #function to test updatePawnPositions
  
  pawnPositions<-c(2,11,5,3)
  positionVector<-rep(0,20)
  positionVector[19]<-1
  transMatrix<-readTransitionMatrix('trans1.csv')
  pawnID<-1
  
  output<-updatePawnPositions(pawnPositions,positionVector,pawnID)
  
  exp_output<-c(19,11,5,3)
  
  return(identical(output,exp_output))
}

test_all<-function(){
  result<-test_updatePawnPositions()
  result[length(result)+1]<-test_executeTurn2()
  result[length(result)+1]<-test_executeTurn1()
  result[length(result)+1]<-test_updateBoard1()
  result[length(result)+1]<-test_updateBoard2()
  result[length(result)+1]<-test_isGamesOver1()
  result[length(result)+1]<-test_isGamesOver2()
  return(result)
}