
# Goal: Advent of Code, DAY 14 - https://adventofcode.com/2022/day/14
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)

# INPUT
day14_input <- #data.frame(input=c("498,4 -> 498,6 -> 496,6","503,4 -> 502,4 -> 502,9 -> 494,9"))
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day14-input.csv", sep="", quote=FALSE)

input <- vector(mode="list",length=nrow(day14_input))
for (i in 1:nrow(day14_input)) {
  day14_input$input[i] <- substr(day14_input$input[i],2,nchar(day14_input$input[i])-1)
  input[[i]] <- strsplit(day14_input$input[i], " -> ")[[1]]
}

bigcave <- matrix(rep(".",1000*200), nrow=200)

for (i in 1:length(input)) {
  rocks <- strsplit(input[[i]], ",")
  rocks <- lapply(rocks, as.numeric)
  
  for (i in 1:(length(rocks)-1)) {
    if (rocks[[i]][1] == rocks[[i+1]][1]) { # same x's
      bigcave[rocks[[i]][2]:rocks[[i+1]][2], rocks[[i]][1]] <- "#" # draw in column
    } else {
      bigcave[rocks[[i]][2], rocks[[i]][1]:rocks[[i+1]][1]] <- "#" # draw in row
    }
  }
}


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

cave <- bigcave
sand_path <- list(c(0,500))

while (TRUE) {
  grain <- sand_path[[length(sand_path)]]
  y <- grain[1]
  x <- grain[2]
  if (y == nrow(cave)) { break } # if grain is on the bottom of the cave
  
  # the sand will take the same path every time except for that last spot that gets filled in
  else if (cave[y+1, x] == ".") { sand_path <- append(sand_path, list(c(y+1, x))) } # if space below is free
  else if (cave[y+1, x-1] == ".") { sand_path <- append(sand_path, list(c(y+1, x-1))) } # down-left
  else if (cave[y+1, x+1] == ".") { sand_path <- append(sand_path, list(c(y+1, x+1))) } # down-right
  else {
    cave[y,x] <- "o"
    sand_path[[length(sand_path)]] <- NULL # delete last spot
  }
}

num_sand <- sum(cave=="o")
cat(paste0("Part 1: ", num_sand, " units of sand come to rest before flowing into the abyss."))


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------

# add the floor in
y_values <- which(bigcave=="#", arr.ind=TRUE)[, 1]
floor_height <- max(y_values) + 2

cave <- bigcave
cave[floor_height,] <- "#"

sand_path <- list(c(0,500))

while (TRUE) {
  if (length(sand_path) == 0) { break } # no possible points left in path
  else {
    grain <- sand_path[[length(sand_path)]]
    y <- grain[1]
    x <- grain[2]
    
    # the sand will take the same path every time except for that last spot that gets filled in
    if (cave[y+1, x] == ".") { sand_path <- append(sand_path, list(c(y+1, x))) } # if space below is free
    else if (cave[y+1, x-1] == ".") { sand_path <- append(sand_path, list(c(y+1, x-1))) } # down-left
    else if (cave[y+1, x+1] == ".") { sand_path <- append(sand_path, list(c(y+1, x+1))) } # down-right
    else {
      cave[y,x] <- "o"
      sand_path[[length(sand_path)]] <- NULL # delete last spot
    }
  }
}

num_sand <- sum(cave=="o") + 1
cat(paste0("Part 2: ", num_sand, " units of sand come to rest in total."))
