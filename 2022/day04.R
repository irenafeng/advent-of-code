
# Goal: Advent of Code, DAY 4 - https://adventofcode.com/2022/day/4
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)

# INPUT
day4_input <- #data.frame(elf1=c("\"2-4,6-8\"","\"2-3,4-5\"","\"5-7,7-9\"","\"2-8,3-7\"","\"6-6,4-6\"","\"2-6,4-8\""))
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day04-input.csv", quote="")

day4_input_edit <- data.frame(pairs=day4_input$elf1)
for (k in 1:nrow(day4_input_edit)) {
  day4_input_edit$pairs[k] <- substr(day4_input_edit$pairs[k],2,nchar(day4_input_edit$pairs[k])-1)
}


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

pair_ranges <- data.frame( elf1_start=vector("character", nrow(day4_input_edit)),
                           elf1_end=vector("character", nrow(day4_input_edit)),
                           elf2_start=vector("character", nrow(day4_input_edit)),
                           elf2_end=vector("character", nrow(day4_input_edit)) )

for (i in 1:nrow(day4_input_edit)) {
  pair_split <- strsplit(day4_input_edit$pairs[i], ",")
  elf1_range <- pair_split[[1]][1]
  elf2_range <- pair_split[[1]][2]
  
  # pair_ranges[i] <- c( strsplit(elf1_range, "-")[[1]][1], strsplit(elf1_range, "-")[[1]][2], strsplit(elf2_range, "-")[[1]][1], strsplit(elf2_range, "-")[[1]][2] )
  pair_ranges$elf1_start[i] <- strsplit(elf1_range, "-")[[1]][1]
  pair_ranges$elf1_end[i] <- strsplit(elf1_range, "-")[[1]][2]
  pair_ranges$elf2_start[i] <- strsplit(elf2_range, "-")[[1]][1]
  pair_ranges$elf2_end[i] <- strsplit(elf2_range, "-")[[1]][2]
}

pair_ranges$elf1_start <- as.numeric(pair_ranges$elf1_start)
pair_ranges$elf1_end <- as.numeric(pair_ranges$elf1_end)
pair_ranges$elf2_start <- as.numeric(pair_ranges$elf2_start)
pair_ranges$elf2_end <- as.numeric(pair_ranges$elf2_end)

pairs_nested <- 0

for (j in 1:nrow(pair_ranges)) {
  if (pair_ranges$elf1_start[j] <= pair_ranges$elf2_start[j] && pair_ranges$elf1_end[j] >= pair_ranges$elf2_end[j]) {
    pairs_nested <- pairs_nested + 1
  } else if (pair_ranges$elf1_start[j] >= pair_ranges$elf2_start[j] && pair_ranges$elf1_end[j] <= pair_ranges$elf2_end[j]) {
    pairs_nested <- pairs_nested + 1
  }
}

cat(paste0("Part 1: There are ", pairs_nested, " assignment pairs where one range fully contains the other."))


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------

pairs_overlap <- 0

for (m in 1:nrow(pair_ranges)) {
  if (pair_ranges$elf1_start[m] <= pair_ranges$elf2_start[m] && pair_ranges$elf1_end[m] >= pair_ranges$elf2_start[m]) {
    pairs_overlap <- pairs_overlap + 1
  } else if (pair_ranges$elf2_start[m] <= pair_ranges$elf1_start[m] && pair_ranges$elf2_end[m] >= pair_ranges$elf1_start[m]) {
    pairs_overlap <- pairs_overlap + 1
  }
}

cat(paste0("Part 2: There are ", pairs_overlap, " assignment pairs where the ranges overlap."))
