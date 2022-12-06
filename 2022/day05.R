
# Goal: Advent of Code, DAY 5 - https://adventofcode.com/2022/day/5
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)

# INPUT
day5_input <- #data.frame(instructions=c("move 1 from 2 to 1","move 3 from 1 to 3","move 2 from 2 to 1","move 1 from 1 to 2"))
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day05-input.csv", sep="")

stacks_input <- #data.frame(stacks=c("    [D]    ","[N] [C]    ","[Z] [M] [P]"," 1   2   3"))
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day05-input-stacks.csv", sep="", header=FALSE)

colnames(stacks_input)[1] = "stacks"
num_stacks <- as.numeric(substr(stacks_input$stacks[nrow(stacks_input)],nchar(stacks_input$stacks[nrow(stacks_input)]),nchar(stacks_input$stacks[nrow(stacks_input)])))

# create stacks matrix
num_rows <- nrow(stacks_input) - 1
num_boxes <- num_stacks * num_rows
stacks <- matrix("", ncol = num_stacks, nrow = num_rows)

# populate stacks matrix
for (i in 1:num_rows) {
  for (j in 1:num_stacks) {
    char <- 4*j - 2
    stacks[i,j] <- substr(stacks_input$stacks[i],char,char)
    if ( stacks[i,j] == " ") { stacks[i,j] <- NA }
  }
}

# flip stacks matrix upside down
stacks <- apply(stacks,2,rev)

stacks_df_orig <- data.frame(stacks=vector(mode="character",length=num_stacks))
for (col in 1:ncol(stacks)) {
  stack_sequence <- ""
  for (row in 1:nrow(stacks)) {
    if (!is.na(stacks[row,col])) { stack_sequence <- capture.output(cat(paste0(stack_sequence, stacks[row,col]))) }
  }
  
  print(stack_sequence)
  stacks_df_orig$stacks[col] <- stack_sequence
}


instructions_df <- data.frame( num_to_move=vector(mode="numeric",length=nrow(day5_input)),
                               move_start=vector(mode="numeric",length=nrow(day5_input)),
                               move_end=vector(mode="numeric",length=nrow(day5_input)) )

for (instruction in 1:nrow(day5_input)) {
  instruction_split <- strsplit(day5_input$instructions[instruction]," ")
  instructions_df$num_to_move[instruction] <- as.numeric(instruction_split[[1]][2])
  instructions_df$move_start[instruction] <- as.numeric(instruction_split[[1]][4])
  instructions_df$move_end[instruction] <- as.numeric(instruction_split[[1]][6])
}


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

stacks_df <- stacks_df_orig

# moving things around
for (k in 1:nrow(instructions_df)) {
  num_to_move <- instructions_df$num_to_move[k]
  move_start <- instructions_df$move_start[k]
  move_end <- instructions_df$move_end[k]
  
  moved_end_stack <- nchar(stacks_df$stacks[move_start])
  moved_start_stack <- moved_end_stack - num_to_move + 1
  moved <- substr(stacks_df$stacks[move_start], moved_start_stack, moved_end_stack)
  
  # reverse what needs to be moved
  moved_split <- strsplit(moved, NULL)[[1]]
  reverse_moved <- paste(rev(moved_split), collapse="")
  
  # remove from origin
  if ( num_to_move == moved_end_stack ) { stacks_df$stacks[move_start] <- ""
  } else { stacks_df$stacks[move_start] <- substr(stacks_df$stacks[move_start], 1, moved_start_stack-1) }
  
  # add to destination
  stacks_df$stacks[move_end] <- capture.output(cat(paste0(stacks_df$stacks[move_end], reverse_moved)))
}

rearranged_top <- ""
for (top in 1:nrow(stacks_df)) {
  top_box <- substr(stacks_df$stacks[top], nchar(stacks_df$stacks[top]), nchar(stacks_df$stacks[top]))
  rearranged_top <- capture.output(cat(paste0(rearranged_top, top_box)))
}

cat(paste0("Part 1: After rearranging, the crates at the top of each stack are ", rearranged_top, "."))


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------

stacks_df_9001 <- stacks_df_orig

# moving things around
for (k in 1:nrow(instructions_df)) {
  num_to_move <- instructions_df$num_to_move[k]
  move_start <- instructions_df$move_start[k]
  move_end <- instructions_df$move_end[k]
  
  moved_end_stack <- nchar(stacks_df_9001$stacks[move_start])
  moved_start_stack <- moved_end_stack - num_to_move + 1
  moved <- substr(stacks_df_9001$stacks[move_start], moved_start_stack, moved_end_stack)
  
  # remove from origin
  if ( num_to_move == moved_end_stack ) { stacks_df_9001$stacks[move_start] <- ""
  } else { stacks_df_9001$stacks[move_start] <- substr(stacks_df_9001$stacks[move_start], 1, moved_start_stack-1) }
  
  # add to destination
  stacks_df_9001$stacks[move_end] <- capture.output(cat(paste0(stacks_df_9001$stacks[move_end], moved)))
}

rearranged_top_9001 <- ""
for (top in 1:nrow(stacks_df_9001)) {
  top_box <- substr(stacks_df_9001$stacks[top], nchar(stacks_df_9001$stacks[top]), nchar(stacks_df_9001$stacks[top]))
  rearranged_top_9001 <- capture.output(cat(paste0(rearranged_top_9001, top_box)))
}

cat(paste0("Part 2: After rearranging using the new crane, the crates at the top of each stack are ", rearranged_top_9001, "."))

