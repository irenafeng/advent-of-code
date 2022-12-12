
# Goal: Advent of Code, DAY 8 - https://adventofcode.com/2022/day/8
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)

# INPUT
day8_input <- #data.frame(grid=c("30373","25512","65332","33549","35390"))
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day08-input.csv", sep="")

all_trees <- capture.output(cat(day8_input$grid,sep=""))
num_trees <- nchar(all_trees)
grid_dim <- sqrt(num_trees)

all_trees <- strsplit(all_trees,"")[[1]]
dim(all_trees) <- c(grid_dim, grid_dim)


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

visible_trees <- 0
hidden_trees <- 0

for (tree in 1:num_trees) {
  tree_height <- all_trees[tree]
  coord1 <- tree %% grid_dim
  coord2 <- ceiling(tree/grid_dim)
  if (coord1 == 0) { coord1 <- grid_dim }
  
  # if border, increment visible_trees
  if (coord1 == 1 | coord1 == grid_dim) { visible_trees <- visible_trees + 1 } # top/bottom borders
  else if (coord2 == 1 | coord2 == grid_dim) { visible_trees <- visible_trees + 1 } # left/right borders
  
  # remaining trees in the middle
  else {
    visibility_left <- all_trees[coord1, 1:(coord2-1)]
    visibility_right <- all_trees[coord1, (coord2+1):grid_dim]
    visibility_up <- all_trees[1:(coord1-1), coord2]
    visibility_down <- all_trees[(coord1+1):grid_dim, coord2]
    
    if (all(tree_height > visibility_left)) { visible_trees <- visible_trees + 1 }
    else if (all(tree_height > visibility_right)) { visible_trees <- visible_trees + 1 }
    else if (all(tree_height > visibility_up)) { visible_trees <- visible_trees + 1 }
    else if (all(tree_height > visibility_down)) { visible_trees <- visible_trees + 1 }
    else { hidden_trees <- hidden_trees + 1 }
  }
}

cat(paste0("Part 1: ", visible_trees, " trees are visible from outside the grid."))


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------

scenic_scores <- matrix(vector(mode="numeric",length=num_trees),nrow=grid_dim,ncol=grid_dim)

for (tree in 1:num_trees) {
  tree_height <- all_trees[tree]
  coord1 <- tree %% grid_dim
  coord2 <- ceiling(tree/grid_dim)
  if (coord1 == 0) { coord1 <- grid_dim }
  
  scenic_left <- 0
  scenic_right <- 0
  scenic_up <- 0
  scenic_down <- 0
  
  # if border, increment visible_trees
  if (coord1 == 1 | coord1 == grid_dim) { total_scenic_score <- 0 } # top/bottom borders
  else if (coord2 == 1 | coord2 == grid_dim) { total_scenic_score <- 0 } # left/right borders
  
  # remaining trees in the middle
  else {
    visibility_left <- rev(all_trees[coord1, 1:(coord2-1)])
    visibility_right <- all_trees[coord1, (coord2+1):grid_dim]
    visibility_up <- rev(all_trees[1:(coord1-1), coord2])
    visibility_down <- all_trees[(coord1+1):grid_dim, coord2]
    
    # scenic score left
    for (i in 1:length(visibility_left)) {
      if (visibility_left[i] < tree_height) { scenic_left <- scenic_left + 1 }
      else {
        scenic_left <- scenic_left + 1
        break
      }
    }
    
    # scenic score right
    for (i in 1:length(visibility_right)) {
      if (visibility_right[i] < tree_height) { scenic_right <- scenic_right + 1 }
      else {
        scenic_right <- scenic_right + 1
        break
      }
    }
    # scenic score up
    for (i in 1:length(visibility_up)) {
      if (visibility_up[i] < tree_height) { scenic_up <- scenic_up + 1 }
      else {
        scenic_up <- scenic_up + 1
        break
      }
    }
    # scenic score down
    for (i in 1:length(visibility_down)) {
      if (visibility_down[i] < tree_height) { scenic_down <- scenic_down + 1 }
      else {
        scenic_down <- scenic_down + 1
        break
      }
    }
    
    total_scenic_score <- scenic_left * scenic_right * scenic_up * scenic_down
  }
  
  scenic_scores[coord1, coord2] <- total_scenic_score
}

max_scenic_score <- max(scenic_scores)
cat(paste0("Part 2: The highest scenic score possible for any tree is ", max_scenic_score, "."))
