
# Goal: Advent of Code, DAY 3 - https://adventofcode.com/2022/day/3
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)

# INPUT
day3_input <- #data.frame( rucksack=c("vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw") )
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day03-input.csv")

day3_input$comp1 <- NA
day3_input$comp2 <- NA

all_letters <- letters[1:26]
all_letters <- append(all_letters,LETTERS[1:26])
priority_conversion_table <- data.frame(letters=all_letters, priorities=1:52)


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

for (i in 1:nrow(day3_input)) {
  day3_input$comp1[i] <- substr(day3_input$rucksack[i], 1, nchar(day3_input$rucksack[i])/2)
  day3_input$comp2[i] <- substr(day3_input$rucksack[i], nchar(day3_input$rucksack[i])/2+1, nchar(day3_input$rucksack[i]))
}

contents <- list(1:nrow(day3_input))
for (j in 1:nrow(day3_input)) {
  contents[[j]] <- list(strsplit(day3_input$comp1[j], ""),strsplit(day3_input$comp2[j], ""))
}

intersections <- vector()
for (k in 1:nrow(day3_input)) {
  intersections <- append(intersections, intersect(contents[[k]][[1]][[1]], contents[[k]][[2]][[1]]))
}

intersections_df <- data.frame(shared=intersections)
intersections_df <- merge(intersections_df, priority_conversion_table,
                          by.x="shared",
                          by.y="letters")

cat("Part 1: For the item types that appear in both compartments of each rucksack, the sum of their priorities is", sum(intersections_df$priorities), ".")


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------

num_groups <- nrow(day3_input)/3
elf_groups <- data.frame(elf1=vector("character", num_groups),
                         elf2=vector("character", num_groups),
                         elf3=vector("character", num_groups))

for(m in 1:nrow(day3_input)) {
  if (m%%3 == 1) { elf_groups$elf1[ceiling(m/3)] <- day3_input$rucksack[m]
  } else if (m%%3 == 2) { elf_groups$elf2[ceiling(m/3)] <- day3_input$rucksack[m]
  } else if (m%%3 == 0) { elf_groups$elf3[ceiling(m/3)] <- day3_input$rucksack[m] }
}

contents_groups <- list(1:num_groups)
for (n in 1:num_groups) {
  contents_groups[[n]] <- list(strsplit(elf_groups$elf1[n], ""),strsplit(elf_groups$elf2[n], ""), strsplit(elf_groups$elf3[n], ""))
}

intersections_groups <- vector()
for (p in 1:num_groups) {
  intersect_temp <- intersect(contents_groups[[p]][[1]][[1]],contents_groups[[p]][[2]][[1]])
  intersections_groups <- append(intersections_groups, intersect(intersect_temp,contents_groups[[p]][[3]][[1]]))
}

intersections_groups_df <- data.frame(shared=intersections_groups)
intersections_groups_df <- merge(intersections_groups_df, priority_conversion_table,
                          by.x="shared",
                          by.y="letters")

cat("Part 2: For the item types that correspond to the badges of each three-Elf group, the sum of their priorities is", sum(intersections_groups_df$priorities), ".")
