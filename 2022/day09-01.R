
# Goal: Advent of Code, DAY 9 - https://adventofcode.com/2022/day/9
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)

# INPUT
day9_input <- data.frame(motions=c("R 5","U 8","L 8","D 3","R 17","D 10","L 25","U 20"))
  #data.frame(motions=c("R 4","U 4","L 3","D 1","R 4","D 1","L 5","R 2"))
  #fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day09-input.csv", sep="")

day9_input$directions <- NA
day9_input$steps <- NA

for (motions in 1:nrow(day9_input)) {
  day9_input$directions[motions] <- strsplit(day9_input$motions[motions], " ")[[1]][1]
  day9_input$steps[motions] <- strsplit(day9_input$motions[motions], " ")[[1]][2]
}

day9_input$steps <- as.numeric(day9_input$steps)

grid_RL_steps <- vector()
grid_UD_steps <- vector()

grid_dim_RL <- 1
grid_dim_UD <- 1

for (motions in 1:nrow(day9_input)) {
  if (day9_input$directions[motions] == "R") {
    grid_dim_RL <- grid_dim_RL + day9_input$steps[motions]
    grid_RL_steps <- append(grid_RL_steps, grid_dim_RL)
  } else if (day9_input$directions[motions] == "L") {
    grid_dim_RL <- grid_dim_RL - day9_input$steps[motions]
    grid_RL_steps <- append(grid_RL_steps, grid_dim_RL)
  } else if (day9_input$directions[motions] == "U") {
    grid_dim_UD <- grid_dim_UD + day9_input$steps[motions]
    grid_UD_steps <- append(grid_UD_steps, grid_dim_UD)
  } else if (day9_input$directions[motions] == "D") {
    grid_dim_UD <- grid_dim_UD - day9_input$steps[motions]
    grid_UD_steps <- append(grid_UD_steps, grid_dim_UD)
  }
}

grid_dim_RL <- max(grid_RL_steps) + abs(min(grid_RL_steps)) + 1
grid_dim_UD <- max(grid_UD_steps) + abs(min(grid_UD_steps)) + 1

# if (any(grid_RL_steps <= 0) | any(grid_UD_steps <= 0)) {
#   stop("negative grid dimensions")
# }

grid_area <- grid_dim_RL * grid_dim_UD


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

grid_motions <- matrix(rep(".", grid_area), nrow=grid_dim_UD, ncol=grid_dim_RL)
grid_visited <- grid_motions

s_row <- max(grid_UD_steps)
s_col <- abs(min(grid_RL_steps)) + 1
grid_motions[s_row, s_col] <- "s"

T_row <- s_row
T_col <- s_col
grid_motions[s_row, s_col] <- "T"
grid_visited[s_row, s_col] <- "#"

H_row <- s_row
H_col <- s_col
grid_motions[s_row, s_col] <- "H"

#print(grid_motions)

# going through motions
for (i in 1:nrow(day9_input)) {
  print("-------------")
  print(day9_input$motions[i])
  
  for (steps in 1:day9_input$steps[i]) {
    grid_motions[H_row, H_col] <- "." # moving H away so setting its original point to "."
    
    if (day9_input$directions[i] == "R") { H_col <- H_col + 1 }
    else if (day9_input$directions[i] == "L") { H_col <- H_col - 1 }
    else if (day9_input$directions[i] == "U") { H_row <- H_row - 1 }
    else if (day9_input$directions[i] == "D") { H_row <- H_row + 1 }
    
    grid_motions[H_row, H_col] <- "H" # set new H location to "H"
    
    if (abs(T_col-H_col) <= 1 && abs(T_row-H_row) <= 1) { # if statement for "don't move T"
      print("close enough; don't move")
    } else { # else statement = need to move
      print("need to move")
      grid_motions[T_row, T_col] <- "." # moving T away so setting its original point to "."
      
      if (T_row == H_row) { T_col <- (T_col + H_col)/2 } # if H/T are in the same row, then move T_col towards H
      else if (T_col == H_col) { T_row <- (T_row + H_row)/2 } # if H/T are in the same col, then move T_row towards H
      else { # T will have to move diagonally
        if (abs(T_col-H_col) <= 1) {
          T_row <- (T_row + H_row)/2
          T_col <- H_col
        } else if (abs(T_row-H_row) <= 1) {
          T_col <- (T_col + H_col)/2
          T_row <- H_row
        }
      }
    }
    
    grid_motions[T_row, T_col] <- "T" # set new T location to "T"
    grid_visited[T_row, T_col] <- "#" # add new T location to visited grid
    
    #print(grid_motions)
  }
}

num_spots_visited <- sum(grid_visited == "#", na.rm=TRUE)
cat(paste0("Part 1: The tail of the rope visits ", num_spots_visited, " positions at least once."))
