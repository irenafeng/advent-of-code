
# Goal: Advent of Code, DAY 2 - https://adventofcode.com/2022/day/2
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)

# INPUT
day2_input <- #data.frame( elf=c("A", "B", "C"), me=c("Y", "X", "Z"))
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day02-input.csv")


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

# A, X - Rock - 1pt
# B, Y - Paper - 2pt
# C, Z - Scissors - 3pt

# loss - 0pt
# draw - 3pt
# win - 6pt

total_points <- 0

for (i in 1:nrow(day2_input)) {
  if (day2_input$me[i] == "X") {
    total_points <- total_points + 1
    if (day2_input$elf[i] == "A") { total_points <- total_points + 3
    } else if (day2_input$elf[i] == "C") { total_points <- total_points + 6 }
    
  } else if (day2_input$me[i] == "Y") {
    total_points <- total_points + 2
    if (day2_input$elf[i] == "B") { total_points <- total_points + 3
    } else if (day2_input$elf[i] == "A") { total_points <- total_points + 6 }
    
  } else if (day2_input$me[i] == "Z") {
    total_points <- total_points + 3
    if (day2_input$elf[i] == "C") { total_points <- total_points + 3
    } else if (day2_input$elf[i] == "B") { total_points <- total_points + 6 }
  }
}

cat("Part 1: Following the strategy guide exactly, you would get a total score of", total_points, "points.")


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------

# X - lose
# Y - draw
# Z - win

total_points_new <- 0

for (j in 1:nrow(day2_input)) {
  if (day2_input$me[j] == "X") {
    if (day2_input$elf[j] == "A") { total_points_new <- total_points_new + 3
    } else if (day2_input$elf[j] == "B") { total_points_new <- total_points_new + 1
    } else if (day2_input$elf[j] == "C") { total_points_new <- total_points_new + 2 }
    
  } else if (day2_input$me[j] == "Y") {
    total_points_new <- total_points_new + 3
    if (day2_input$elf[j] == "A") { total_points_new <- total_points_new + 1
    } else if (day2_input$elf[j] == "B") { total_points_new <- total_points_new + 2
    } else if (day2_input$elf[j] == "C") { total_points_new <- total_points_new + 3 }
    
  } else if (day2_input$me[j] == "Z") {
    total_points_new <- total_points_new + 6
    if (day2_input$elf[j] == "A") { total_points_new <- total_points_new + 2
    } else if (day2_input$elf[j] == "B") { total_points_new <- total_points_new + 3
    } else if (day2_input$elf[j] == "C") { total_points_new <- total_points_new + 1 }
    
  }
}

cat("Part 2: Following the strategy guide exactly with the new instructions, you would get a total score of", total_points_new, "points.")
