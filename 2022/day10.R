
# Goal: Advent of Code, DAY 10 - https://adventofcode.com/2022/day/10
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)

# INPUT
day10_input <- #data.frame(program=c("noop","addx 3","addx -5"))
  #data.frame(program=c("addx 15","addx -11","addx 6","addx -3","addx 5","addx -1","addx -8","addx 13","addx 4","noop","addx -1","addx 5","addx -1","addx 5","addx -1","addx 5","addx -1","addx 5","addx -1","addx -35","addx 1","addx 24","addx -19","addx 1","addx 16","addx -11","noop","noop","addx 21","addx -15","noop","noop","addx -3","addx 9","addx 1","addx -3","addx 8","addx 1","addx 5","noop","noop","noop","noop","noop","addx -36","noop","addx 1","addx 7","noop","noop","noop","addx 2","addx 6","noop","noop","noop","noop","noop","addx 1","noop","noop","addx 7","addx 1","noop","addx -13","addx 13","addx 7","noop","addx 1","addx -33","noop","noop","noop","addx 2","noop","noop","noop","addx 8","noop","addx -1","addx 2","addx 1","noop","addx 17","addx -9","addx 1","addx 1","addx -3","addx 11","noop","noop","addx 1","noop","addx 1","noop","noop","addx -13","addx -19","addx 1","addx 3","addx 26","addx -30","addx 12","addx -1","addx 3","addx 1","noop","noop","noop","addx -9","addx 18","addx 1","addx 2","noop","noop","addx 9","noop","noop","noop","addx -1","addx 2","addx -37","addx 1","addx 3","noop","addx 15","addx -21","addx 22","addx -6","addx 1","noop","addx 2","addx 1","noop","addx -10","noop","noop","addx 20","addx 1","addx 2","addx 2","addx -6","addx -11","noop","noop","noop"))
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day10-input.csv", sep="")

for (i in 1:nrow(day10_input)) {
  day10_input$command[i] <- strsplit(day10_input$program[i], " ")[[1]][1]
  day10_input$increment[i] <- strsplit(day10_input$program[i], " ")[[1]][2]
}

num_cycles <- nrow(day10_input) + sum(day10_input$command=="addx", na.rm=TRUE)


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

cycle <- 0
X <- 1
program_row <- 1
addx <- 0

signal_strengths <- vector(mode="numeric",length=num_cycles)

for (i in 1:num_cycles) {
  command <- day10_input$command[program_row]
  increment <- as.numeric(day10_input$increment[program_row])
  
  signal_strengths[i] <- i * X # based on X value DURING cycle i
  
  if (command=="addx" && addx==0) { # first iteration of addx
    addx <- 1
    next
  } else if (command=="addx" && addx==1) { # second iteration of addx
    addx <- 0
    X <- X + increment
    program_row <- program_row + 1
  } else if (command=="noop") {
    program_row <- program_row + 1
  }
}

sum_signalstrength <- signal_strengths[20] + signal_strengths[60] + signal_strengths[100] + signal_strengths[140] + signal_strengths[180] + signal_strengths[220]
cat(paste0("Part 1: The sum of the signal strengths during the 20th, 60th, 100th, 140th, 180th, and 220th cycles is ", sum_signalstrength, "."))


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------

cycle <- 0
X <- 1
program_row <- 1
addx <- 0

CRT <- vector(mode="character", length=num_cycles)

for (i in 1:num_cycles) {
  CRT_position <- (i-1) %% 40
  
  command <- day10_input$command[program_row]
  increment <- as.numeric(day10_input$increment[program_row])
  
  if (abs(CRT_position - X) <= 1) {
    CRT[i] <- "#"
  } else {
    CRT[i] <- "."
  }
  
  if (command=="addx" && addx==0) { # first iteration of addx
    addx <- 1
    next
  } else if (command=="addx" && addx==1) { # second iteration of addx
    addx <- 0
    X <- X + increment
    program_row <- program_row + 1
  } else if (command=="noop") {
    program_row <- program_row + 1
  }
}

CRT_string <- ""

for (i in 1:(length(CRT)/40)) {
  string_start <- 40*i - 39
  string_end <- 40*i
  CRT_string_temp <- paste(CRT[string_start:string_end],sep="",collapse="")
  
  CRT_string <- paste(CRT_string, CRT_string_temp, sep="")
  CRT_string <- paste(CRT_string, "\n", sep="")
}

# display results
cat(CRT_string)