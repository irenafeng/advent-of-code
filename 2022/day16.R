
# Goal: Advent of Code, DAY 16 - https://adventofcode.com/2022/day/16
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)
library(dplyr)
library(dtplyr)
library(stringr)

# INPUT
day16_input <- #data.frame(input=c("Valve AA has flow rate=0; tunnels lead to valves DD, II, BB","Valve BB has flow rate=13; tunnels lead to valves CC, AA","Valve CC has flow rate=2; tunnels lead to valves DD, BB","Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE","Valve EE has flow rate=3; tunnels lead to valves FF, DD","Valve FF has flow rate=0; tunnels lead to valves EE, GG","Valve GG has flow rate=0; tunnels lead to valves FF, HH","Valve HH has flow rate=22; tunnel leads to valve GG","Valve II has flow rate=0; tunnels lead to valves AA, JJ","Valve JJ has flow rate=21; tunnel leads to valve II"))
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day16-input.csv", sep="", quote="")

total_time <- 30

input <- data.frame(valve=vector(mode="character", length=nrow(day16_input)),
                    flowrate=vector(mode="numeric", length=nrow(day16_input)))

leadingto <- vector(mode="list", length=nrow(day16_input))

for (i in 1:nrow(day16_input)) {
  if (startsWith(day16_input$input[i], "\"")) { day16_input$input[i] <- substr(day16_input$input[i], 2, nchar(day16_input$input[i])-1) }
  
  input$valve[i] <- substr(day16_input$input[i],7,8)
  input$flowrate[i] <- as.numeric(strsplit(day16_input$input[i],"[=;]+")[[1]][2])
  
  if (grepl(",", day16_input$input[i], fixed=TRUE)) {
    tunnels <- strsplit(day16_input$input[i], "valves ")[[1]][2]
    leadingto[[i]] <- strsplit(tunnels, ", ")[[1]]
  } else {
    tunnels <- strsplit(day16_input$input[i], "valve ")[[1]][2]
    leadingto[[i]] <- strsplit(tunnels, ", ")[[1]]
  }
}

paths <- tibble(from=input$valve, to=leadingto)
input <- input %>% filter(flowrate > 0)


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

progress <- data.frame(cur="AA",
                       opened="",
                       per_turn=0,
                       total=0)

for (minute in 1:total_time) {
  #print(progress)
  
  opening_valves <- progress %>%
    filter(!str_detect(opened, cur)) %>%
    mutate(opened=paste0(opened, " ", cur)) %>%
    inner_join(input, by=c("cur"="valve")) %>%
    filter(flowrate!=0) %>%
    mutate(per_turn = per_turn + flowrate,
           total = total - flowrate,
           flowrate = NULL)
  
  rows_current <- nrow(progress)
  next_valves <- progress %>%
    left_join(paths, by=c("cur"="from"))
  
  for (r in 1:rows_current) {
    opened_current <- next_valves$opened[r]
    per_turn_current <- next_valves$per_turn[r]
    total_current <- next_valves$total[r]
    next_valves_current <- next_valves$to[r]
    
    for (v in 1:length(next_valves_current[[1]])) {
      next_valves <- next_valves %>%
        add_row(cur=next_valves_current[[1]][v],
                opened=opened_current,
                per_turn=per_turn_current,
                total=total_current,
                to=next_valves_current) #just a placeholder
    }
  }
  #print("create next valves")
  
  next_valves <- next_valves[(rows_current+1):nrow(next_valves),] %>%
    mutate(to=NULL)
  
  next_valves$per_turn <- as.numeric(next_valves$per_turn)
  next_valves$total <- as.numeric(next_valves$total)
  
  progress <- rbind(next_valves, opening_valves) %>%
    mutate(total = total + per_turn) %>%
    lazy_dt() %>%
    group_by(cur, opened, per_turn) %>%
    summarise(total = max(total), .groups="drop") %>%
    collect()
  
  if (minute > 15) {
    drop_too_low <- max(progress$total) - (31-minute)*50
    progress <- progress %>% filter(total > drop_too_low)
  }
  
  print(paste0(minute, "; ", nrow(progress)))
}












# if by some grace of god all the non-zero valves were open from the very start
max_possible <- total_time * sum(input$flowrate)

# maybe generate a list of all possible paths... might be very long though
# each path would be a vector of valves
# if [i] and [i+1] are the same, count as open on next minute/change open to TRUE

# calculate sum(input$flowrate * input$open) at the beginning of each minute
# have a running sum of these sums
# you'd have some where you turn the valve closed actually but the sums will account for that

# to get flowrate of current valve: input$flowrate[which(input$valve==cur)]
# to change current valve open/close: input$open[which(input$valve==cur)] <- !input$open[which(input$valve==cur)]
# tunnels that the current valve leads to: leadingto[[which(input$valve==cur)]]

# options at any given moment
# - if `open` == FALSE, stay to open; cur <- cur
# - go to any of the other valves; leadingto[[which(input$valve==cur)]][1:length(leadingto[[which(input$valve==cur)]])]

# !!! review sand; day 14
# do it something similar

tunnel_path <- list("AA") # starts at AA
min_left <- 29
index <- 1

while (TRUE) {
  cur <- tunnel_path[[length(tunnel_path)]]
  flow <- input$flowrate[which(input$valve==cur)]
  open <- input$open[which(input$valve==cur)]
  options <- leadingto[[which(input$valve==cur)]]
  
  if (min_left == -1) { break }
  
  # if flowrate is NOT zero
  else if (flow != 0) {
    if (!open) { options <- append(options, cur) }
    
  }
}

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