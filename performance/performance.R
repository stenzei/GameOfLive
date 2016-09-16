rm(list = ls())
gc()

library(doParallel)
library(foreach)
source("gol-imp.R")

dta <- NULL
for (s in seq(100, 1000, by = 100)) {
  print(s)
  # Run in parallel
  registerDoParallel(8)
  res <- foreach(i = c(1:24), .combine = rbind) %dopar% {
    if (s <= 300) {
      t1 <- system.time(game.of.life.rosetta(s, steps = 50))[[3]]
    } else {
      t1 <- -1
    }
    t2 <- system.time(game.of.life.petrkeil(s, steps = 50))[[3]]
    t3 <- system.time(game.of.life.stenze(s, steps = 50))[[3]]
    c(s, t1, t2, t3)
  }
  stopImplicitCluster()
  print(res)
  dta <- rbind(dta, res)
}
write.csv(dta, file = "perform.csv")
