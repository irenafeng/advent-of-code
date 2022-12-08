
# Goal: Advent of Code, DAY 6 - https://adventofcode.com/2022/day/6
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)

# INPUT
day6_input <- paste(readLines("C:\\Users\\irenafeng\\Downloads\\advent of code\\day06-input.txt"), collapse="\n")


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

lastchar <- 0
for (i in 1:nchar(day6_input)) {
  char_marker <- substr(day6_input,i,i+3)
  char_marker_split <- strsplit(char_marker, NULL)[[1]]
  if ( length(char_marker_split) == length(unique(char_marker_split)) ) {
    lastchar <- i + 3
    break
  }
}

cat(paste0("Part 1: Before the first start-of-packet marker is detected, ", lastchar, " characters need to be processed."))


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------

lastchar_message <- 0
for (j in 1:nchar(day6_input)) {
  char_marker <- substr(day6_input,j,j+13)
  char_marker_split <- strsplit(char_marker, NULL)[[1]]
  if ( length(char_marker_split) == length(unique(char_marker_split)) ) {
    lastchar_message <- j + 13
    break
  }
}

cat(paste0("Part 2: Before the first start-of-message marker is detected, ", lastchar_message, " characters need to be processed."))

