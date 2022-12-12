
# Goal: Advent of Code, DAY 12 - https://adventofcode.com/2022/day/12
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)

# INPUT
day12_input <- #data.frame(input=c("Sabqponm","abcryxxl","accszExk","acctuvwj","abdefghi"))
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day12-input.csv", sep="", quote="")

elevation_lookup <- c(1:26,1,26)
names(elevation_lookup) <- c(letters,"S","E")

input_string <- capture.output(cat(paste(day12_input$input),sep=""))
input_string <- strsplit(input_string,"")[[1]]
num_row_matrix <- nrow(day12_input)
num_col_matrix <- length(input_string) / num_row_matrix
input_matrix <- matrix(input_string, nrow=num_col_matrix, ncol=num_row_matrix)
input_matrix <- t(input_matrix)

heightmap <- elevation_lookup[input_matrix] %>% matrix(nrow = nrow(input_matrix))

S_row <- which(input_matrix=="S") %% num_row_matrix
S_col <- ceiling(which(input_matrix=="S") / num_row_matrix)

E_row <- which(input_matrix=="E") %% num_row_matrix
E_col <- ceiling(which(input_matrix=="E") / num_row_matrix)


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

# return a list of the coordinates of adjacent cells
adjacent <- function(x,y) {
  adjacent <- list()
  if (x > 1) { adjacent <- append(adjacent, list(c(x-1,y))) } # cell to the left
  if (x < nrow(input_matrix)) { adjacent <- append(adjacent, list(c(x+1,y))) } # cell to the right
  if (y > 1) { adjacent <- append(adjacent, list(c(x,y-1))) } # cell above
  if (y < ncol(input_matrix)) { adjacent <- append(adjacent, list(c(x,y+1)))}
  
  return(adjacent)
}

to_check_matrix <- matrix(rep(0, length(input_matrix)), nrow=nrow(input_matrix))
to_check_matrix[S_row, S_col] <- 1

path_forward <- matrix(rep(-1, length(input_matrix)), nrow=nrow(input_matrix))
path_forward[S_row, S_col] <- 0

steps <- 1
while (path_forward[E_row, E_col] == -1) {
  unchecked <- which(to_check_matrix == 1, arr.ind=TRUE)
  for (i in 1:nrow(unchecked)) {
    xcoord <- unchecked[i, 1]
    ycoord <- unchecked[i, 2]
    height <- heightmap[xcoord, ycoord]
    
    for (adjacent_cell in adjacent(xcoord, ycoord)) {
      adj_xcoord <- adjacent_cell[1]
      adj_ycoord <- adjacent_cell[2]
      if ((heightmap[adj_xcoord,adj_ycoord] <= height+1) & (path_forward[adj_xcoord,adj_ycoord] == -1)) {
        path_forward[adj_xcoord,adj_ycoord] <- steps
        to_check_matrix[adj_xcoord,adj_ycoord] <- 1
      }
    }
    
    to_check_matrix[xcoord,ycoord] <- 0
  }
  
  steps <- steps+1
  #print(path_forward)
}

num_steps <- path_forward[E_row, E_col]
cat(paste0("Part 1: The fewest steps required is ", num_steps, " steps to move from the current position to the location for the best signal."))


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------

to_check_matrix <- matrix(rep(0, length(input_matrix)), nrow=nrow(input_matrix))
to_check_matrix[E_row, E_col] <- 1

path_scenic <- matrix(rep(-1, length(input_matrix)), nrow=nrow(input_matrix))
path_scenic[E_row, E_col] <- 0

steps <- 1
while (min(path_scenic) == -1) {
  unchecked <- which(to_check_matrix == 1, arr.ind=TRUE)
  if (nrow(unchecked) == 0) { break } # no more options to check/can't get to height of 1
  else {
    for (i in 1:nrow(unchecked)) {
      xcoord <- unchecked[i, 1]
      ycoord <- unchecked[i, 2]
      height <- heightmap[xcoord, ycoord]
      
      for (adjacent_cell in adjacent(xcoord, ycoord)) {
        adj_xcoord <- adjacent_cell[1]
        adj_ycoord <- adjacent_cell[2]
        if ((heightmap[adj_xcoord,adj_ycoord] >= height-1) & (path_scenic[adj_xcoord,adj_ycoord] == -1)) {
          path_scenic[adj_xcoord,adj_ycoord] <- steps
          to_check_matrix[adj_xcoord,adj_ycoord] <- 1
        }
      }
      
      to_check_matrix[xcoord,ycoord] <- 0
    }
    
    steps <- steps+1
    #print(path_scenic)
  }
}

steps_to_lowest <- path_scenic[which(heightmap == 1, arr.ind=TRUE)]
steps_to_lowest <- steps_to_lowest[which(steps_to_lowest != -1)]
fewest_steps <- min(steps_to_lowest)
cat(paste0("Part 2: The fewest steps required is ", fewest_steps, " steps to move from any square with elevation `a` to the location for the best signal."))



