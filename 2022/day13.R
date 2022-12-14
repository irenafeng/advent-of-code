
# Goal: Advent of Code, DAY 13 - https://adventofcode.com/2022/day/13
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)
library(stringr)

# INPUT
day13_input <- #data.frame(input=c("[1,1,3,1,1]","[1,1,5,1,1]","","[[1],[2,3,4]]","[[1],4]","","[9]","[[8,7,6]]","","[[4,4],4,4]","[[4,4],4,4,4]","","[7,7,7,7]","[7,7,7]","","[]","[3]","","[[[]]]","[[]]","","[1,[2,[3,[4,[5,6,7]]]],8,9]","[1,[2,[3,[4,[5,6,0]]]],8,9]"))
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day13-input.csv", sep="", quote="")

pairs <- (nrow(day13_input)+1) / 3
pair1 <- vector(mode="character", length=pairs)
pair2 <- vector(mode="character", length=pairs)

pair_increment <- 1
for (input in 1:nrow(day13_input)) {
  if (startsWith(day13_input$input[input],"\"")) { day13_input$input[input] <- substr(day13_input$input[input], 2, nchar(day13_input$input[input])-1)}
  
  day13_input$input[input] <- str_replace_all(day13_input$input[input],"\\[","list\\(")
  day13_input$input[input] <- str_replace_all(day13_input$input[input],"\\]","\\)")
  
  if (day13_input$input[input] == "") { next }
  else if (input %% 3 == 1) {
    pair1[pair_increment] <- day13_input$input[input]
  } else {
    pair2[pair_increment] <- day13_input$input[input]
    pair_increment <- pair_increment + 1
  }
}

p1 <- vector(mode="list",length=pairs)
p2 <- vector(mode="list",length=pairs)
for (pair in 1:pairs) {
  temp1 <- eval(parse(text=pair1[pair]))
  temp2 <- eval(parse(text=pair2[pair]))
  
  p1[[pair]] <- temp1
  p2[[pair]] <- temp2
}

pair_comparison <- function(x,y) {
  # smallest thing = empty list
  if (length(x) == 0 && length(y) == 0) { return(NA) }
  else if (length(x) == 0) { return(TRUE) } # left is smaller
  else if (length(y) == 0) { return(FALSE) } # right is smaller
  
  # if the first thing in each list is just an integer
  if (is.numeric(x[[1]]) && is.numeric(y[[1]])) {
    if (x[[1]] < y[[1]]) { return(TRUE) } # left is smaller and we can just return
    else if (x[[1]] > y[[1]]) { return(FALSE) } # right is smaller and we can just return
    else {
      x[[1]] <- NULL
      y[[1]] <- NULL
      return(pair_comparison(x,y)) # run it again with [[1]] removed until something happens to hit return
    }
  }
  
  # if the first thing in each list is a list
  if (is.list(x[[1]]) && is.list(y[[1]])) {
    list_compared <- pair_comparison(x[[1]],y[[1]])
    if (is.na(list_compared)) {
      x[[1]] <- NULL
      y[[1]] <- NULL
      return(pair_comparison(x,y))
    } else {
      return(list_compared)
    }
  }
  
  # if one is a list and the other is an integer
  # if left is an integer
  if (is.numeric(x[[1]]) && is.list(y[[1]])) {
    x[[1]] <- list(x[[1]])
    return(pair_comparison(x,y))
  }
  
  # if right is an integer
  if (is.list(x[[1]]) && is.numeric(y[[1]])) {
    y[[1]] <- list(y[[1]])
    return(pair_comparison(x,y))
  }
}


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

trues <- 0
for (i in 1:pairs) {
  if (pair_comparison(p1[[i]], p2[[i]])) { trues <- trues + i }
}

cat(paste0("Part 1: For the pairs that are already in the right order, the sum of the indices is ", trues, "."))


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------

all_packets <- c(p1, p2)
divider1 <- list(list(2))
divider2 <- list(list(6))

# index for [[2]] is how many packets it's greater than + 1
greater1 <- sapply(all_packets, pair_comparison, y=divider1)
index1 <- sum(greater1, 1)

# index for [[6]] is how many packets it's greater than + 2 (since divider1 is inserted as well)
greater2 <- sapply(all_packets, pair_comparison, y=divider2)
index2 <- sum(greater2, 2)

decoder_key <- index1 * index2
cat(paste0("Part 2: The decoder key for the distress signal is ", decoder_key, "."))
