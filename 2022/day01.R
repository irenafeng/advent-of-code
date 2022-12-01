
# Goal: Advent of Code, DAY 1 - https://adventofcode.com/2022/day/1
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)

# INPUT
day1_input <- fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day01-input.csv")


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

elves <- list()
elfnum <- 1
elves[[elfnum]] <- vector()

for (i in 1:nrow(day1_input)) {
  
  if (is.na(day1_input[i,V1])) {
    elfnum <- elfnum + 1
    elves[[elfnum]] <- vector()
  } else {
    elves[[elfnum]] <- append(elves[[elfnum]], day1_input[i,V1])
  } 
}

caloric_sums <- vector()
for (i in 1:length(elves)) {
  caloric_sums <- append(caloric_sums, sum(elves[[i]]))
}

max_elf <- which.max(caloric_sums)
max_elf_calories <- caloric_sums[max_elf]

cat("Part 1: The Elf carrying the most Calories is carrying ", max_elf_calories, " total Calories.")

# ----------------------------------------------------------------------------
# (2) code, part 2
# ----------------------------------------------------------------------------

caloric_sums_sorted <- sort(caloric_sums, decreasing = TRUE)
max_3_elves_calories <- caloric_sums_sorted[1] + caloric_sums_sorted[2] + caloric_sums_sorted[3]

cat("Part 2: The top 3 Elves carrying the most Calories are carrying ", max_3_elves_calories, " Calories in total.")
