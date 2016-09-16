library(Matrix)

# Constucting game of life objects
constructObject <- function(object) {
  if (object == 1) {
    # position of the glider
    i <- 2
    j <- 2
    # setting living cells
    m[i+1, j] <<- 1
    m[i+2, j+1] <<- 1
    m[i,   j+2] <<- 1
    m[i+1, j+2] <<- 1
    m[i+2, j+2] <<- 1
  }
  if (object == 2) {
    # position of the LWSS
    i <- 2
    j <- 5
    # setting living cells
    m[i+1, j] <<- 1
    m[i+2, j] <<- 1
    m[i+3, j] <<- 1
    m[i+4, j] <<- 1
    m[i,   j+1] <<- 1
    m[i+4, j+1] <<- 1
    m[i+4, j+2] <<- 1
    m[i,   j+3] <<- 1
    m[i+3, j+3] <<- 1
  }
  if (object == 3) {
    # position of the pulsar
    i <- sliderValue/2 - 5
    j <- sliderValue/2 - 5
    # setting living cells
    m[i+2,  j] <<- 1
    m[i+3,  j] <<- 1
    m[i+4,  j] <<- 1
    m[i+8,  j] <<- 1
    m[i+9,  j] <<- 1
    m[i+10, j] <<- 1
    m[i,    j+2] <<- 1
    m[i+5,  j+2] <<- 1
    m[i+7,  j+2] <<- 1
    m[i+12, j+2] <<- 1
    m[i,    j+3] <<- 1
    m[i+5,  j+3] <<- 1
    m[i+7,  j+3] <<- 1
    m[i+12, j+3] <<- 1
    m[i,    j+4] <<- 1
    m[i+5,  j+4] <<- 1
    m[i+7,  j+4] <<- 1
    m[i+12, j+4] <<- 1
    m[i+2,  j+5] <<- 1
    m[i+3,  j+5] <<- 1
    m[i+4,  j+5] <<- 1
    m[i+8,  j+5] <<- 1
    m[i+9,  j+5] <<- 1
    m[i+10, j+5] <<- 1
    m[i+2,  j+7] <<- 1
    m[i+3,  j+7] <<- 1
    m[i+4,  j+7] <<- 1
    m[i+8,  j+7] <<- 1
    m[i+9,  j+7] <<- 1
    m[i+10, j+7] <<- 1
    m[i,    j+8] <<- 1
    m[i+5,  j+8] <<- 1
    m[i+7,  j+8] <<- 1
    m[i+12, j+8] <<- 1
    m[i,    j+9] <<- 1
    m[i+5,  j+9] <<- 1
    m[i+7,  j+9] <<- 1
    m[i+12, j+9] <<- 1
    m[i,    j+10] <<- 1
    m[i+5,  j+10] <<- 1
    m[i+7,  j+10] <<- 1
    m[i+12, j+10] <<- 1
    m[i+2,  j+12] <<- 1
    m[i+3,  j+12] <<- 1
    m[i+4,  j+12] <<- 1
    m[i+8,  j+12] <<- 1
    m[i+9,  j+12] <<- 1
    m[i+10, j+12] <<- 1
  }
  if (object == 4) {
    # position of the R-pentomino
    i <- 5
    j <- 5
    # setting living cells
    m[i+1,j] <<- 1
    m[i+2,j] <<- 1
    m[i,  j+1] <<- 1
    m[i+1,j+1] <<- 1
    m[i+1,j+2] <<- 1
  }
}

# Constructing the matrix of the game
constructMatrix <- function(sliderValue, object) {
  width <- sliderValue
  height <- width
  m <<- Matrix(rep(0, width*height), width, sparse = TRUE)
  constructObject(object)
}