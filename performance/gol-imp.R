library(Matrix)

game.of.life.stenze <- function(side, steps){
  m <- Matrix(nrow = side, ncol = side, sparse = TRUE)
  m[] <- rbinom(side^2, 1, 0.1)
  tt <- Diagonal(side)
  md <- Diagonal(side - 1)
  md <- rbind(0, cbind(md, 0))
  tt <- tt + md + t(md)
  tt[1, side] <- 1
  tt[side, 1] <- 1
  for (i in 1:steps)
  {
    tm <- tt %*% m %*% tt - m
    tm[tm == 1] = 0
    tm[tm > 3] = 0
    tm <- tm + m
    tm[tm < 3] = 0
    tm[tm > 0] = 1
    m <- tm
  }
}

game.of.life.petrkeil <- function(side, steps){
  X <- matrix(nrow = side, ncol = side)
  X[] <- rbinom(side^2, 1, 0.1)
  for (i in 1:steps)
  {
    allW = cbind(rep(0, side), X[ , -side])
    allNW = rbind(rep(0, side), cbind(rep(0, side - 1), X[-side, -side]))
    allN = rbind(rep(0, side), X[-side, ])
    allNE = rbind(rep(0, side), cbind(X[-side, -1], rep(0, side - 1)))
    allE = cbind(X[,-1],rep(0,side))
    allSE = rbind(cbind(X[-1,-1],rep(0, side - 1)), rep(0, side))
    allS = rbind(X[-1,],rep(0, side))
    allSW = rbind(cbind(rep(0, side - 1), X[-1, -side]), rep(0, side))
    X2 <- allW + allNW + allN + allNE + allE + allSE + allS + allSW
    X3 <- X
    X3[X == 0 & X2 == 3] <- 1
    X3[X == 1 & X2 < 2] <- 0
    X3[X == 1 & X2 > 3] <- 0
    X <- X3
  }
}

game.of.life.rosetta <- function(side, steps){
  board <- matrix(nrow = side, ncol = side)
  board[] <- rbinom(side^2, 1, 0.1)
  count.neighbours <- function(x, i, j) 
  {   
    sum(x[max(1, i - 1):min(nrow(x), i + 1),
          max(1, j - 1):min(ncol(x), j + 1)]) - x[i, j]
  }
  evolve <- function(board)
  { 
    newboard <- board
    for (i in seq_len(nrow(board)))
    {
      for (j in seq_len(ncol(board)))
      {
        newboard[i,j] <- determine.new.state(board, i, j)         
      }   
    }
    newboard
  }
  determine.new.state <- function(board, i, j)
  {
    N <- count.neighbours(board,i,j)
    (N == 3 || (N == 2 && board[i,j]))
  }
  for (i in 1:steps)
  {
    board <- evolve(board)
  }
}
