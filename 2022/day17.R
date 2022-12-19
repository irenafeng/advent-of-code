
# Goal: Advent of Code, DAY 17 - https://adventofcode.com/2022/day/17
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)
library(dplyr)

# INPUT
day17_input <- #data.frame(input=c(">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"))
  fread("C:\\Users\\irenafeng\\Downloads\\AOC\\day17-input.csv", sep="", quote=FALSE)

input <- day17_input$input[1] %>%
  str_split("") %>%
  unlist()

leftright <- list(">" = 1, "<" = -1)
input <- sapply(input, function(x)leftright[[x]])
names(input) <- NULL

# rock appears:
# left edge is 2 units away from left wall --> 4 when including wall
# bottom edge is 3 units above highest point/floor --> x
rock_appear <- function(num_rocks, room) {
  if (num_rocks == 1) { x <- nrow(room) - 4 }
  else { x <- min(which(room == 1, arr.ind=TRUE)[,1]) - 4}
  
  # which shape to return
  if (num_rocks %% 5 == 1) { shape <- list(c(x,4), c(x,5), c(x,6), c(x,7)) }
  else if (num_rocks %% 5 == 2) { shape <- list(c(x-2,5), c(x-1,4), c(x-1,5), c(x-1,6), c(x,5)) }
  else if (num_rocks %% 5 == 3) { shape <- list(c(x-2,6), c(x-1,6), c(x,4), c(x,5), c(x,6)) }
  else if (num_rocks %% 5 == 4) { shape <- list(c(x-3,4), c(x-2,4), c(x-1,4), c(x,4)) }
  else if (num_rocks %% 5 == 0) { shape <- list(c(x-1,4), c(x-1,5), c(x,4), c(x,5)) }
  
  return(shape)
}

# rock falls:
# first pushed by jet of hot gas, then falls one unit down
# does not move if overlaps with anything
# if can't move downward, landed <- TRUE and then new rock appears
rock_fall <- function(room, shape, num_moves) {
  landed <<- FALSE
  
  # push rock with jet of hot gas
  input_index <- (num_moves-1) %% length(input) + 1
  direction <- input[input_index]
  new_position <- lapply(shape, function(x){x + c(0, direction)})
  if (all(new_position %>% sapply(function(x){room[x[1],x[2]]}) == 0)) { shape <- new_position }
  
  # downward movement
  new_position <- lapply(shape, function(x){x + c(1,0)})
  if (all(new_position %>% sapply(function(x){room[x[1],x[2]]}) == 0)) { shape <- new_position }
  else { landed <<- TRUE }
  
  return(shape)
}

# turn the landed shape into 1's in the chamber
rock_landed <- function(room, shape) {
  for (pt in shape) { room[pt[1],pt[2]] <- 1 }
  return(room)
}


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

# chamber using 9 to represent walls, floor
width <- 7
depth <- 4000 # arbitrary value
chamber <- matrix(0, nrow=depth, ncol=(width+2))
chamber[nrow(chamber),] <- 9
chamber[,1] <- 9
chamber[,ncol(chamber)] <- 9

num_rocks <- 0
num_moves <- 1
landed <- TRUE

# 2022 rocks stopped, before 2023 starts falling
while (num_rocks < 2023) {
  if (landed) {
    num_rocks <- num_rocks + 1
    shape <- rock_appear(num_rocks, chamber)
  }
  
  shape <- rock_fall(chamber, shape, num_moves)
  num_moves <- num_moves + 1
  
  if (landed) { chamber <- rock_landed(chamber, shape) }
}

tower_height <- max(which(chamber == 1, arr.ind=TRUE)[,1]) -
  min(which(chamber == 1, arr.ind=TRUE)[,1]) + 1

cat(paste0("Part 1: After 2022 rocks have stopped falling, the tower will be ", tower_height, " units tall."))


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------







