
# Goal: Advent of Code, DAY 11 - https://adventofcode.com/2022/day/11
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)

# INPUT
day11_input <- #data.frame(input=c("Monkey 0:","  Starting items: 79, 98","  Operation: new = old * 19","  Test: divisible by 23","    If true: throw to monkey 2","    If false: throw to monkey 3","","Monkey 1:","  Starting items: 54, 65, 75, 74","  Operation: new = old + 6","  Test: divisible by 19","    If true: throw to monkey 2","    If false: throw to monkey 0","","Monkey 2:","  Starting items: 79, 60, 97","  Operation: new = old * old","  Test: divisible by 13","    If true: throw to monkey 1","    If false: throw to monkey 3","","Monkey 3:","  Starting items: 74","  Operation: new = old + 3","  Test: divisible by 17","    If true: throw to monkey 0","    If false: throw to monkey 1"))
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day11-input.csv", sep="", quote="")

for (row in 1:nrow(day11_input)) {
  if (startsWith(day11_input$input[row], "\"")) { day11_input$input[row] <- substr(day11_input$input[row], 2, nchar(day11_input$input[row])-1)}
  day11_input$input[row] <- trimws(day11_input$input[row])
}


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

num_rounds <- 20

monkey_index <- vector()
starting_items <- list()
operation <- vector()
test <- vector()
test_if_true <- vector()
test_if_false <- vector()

for (row in 1:nrow(day11_input)) {
  if (day11_input$input[row] == "") { # skip blank rows
    next
  } else if (startsWith(day11_input$input[row], "Monkey ")) { # eg) Monkey 0:
    index_temp <- strsplit(day11_input$input[row], " ")[[1]][2]
    monkey_index[length(monkey_index)+1] <- substr(index_temp, 1, nchar(index_temp)-1)
  } else if (startsWith(day11_input$input[row], "Starting items: ")) { # eg) Starting items: 79, 98
    items_temp <- strsplit(day11_input$input[row], ", ")[[1]]
    items_temp[1] <- strsplit(items_temp[1], " ")[[1]][3]
    starting_items <- append(starting_items, "")
    starting_items[[length(starting_items)]] <- items_temp
  } else if (startsWith(day11_input$input[row], "Operation: ")) { # eg) Operation: new = old * 19 [!]use eval(parse(text="[your text here]"))
    operation[length(operation)+1] <- substr(day11_input$input[row],18,nchar(day11_input$input[row]))
  } else if (startsWith(day11_input$input[row], "Test: ")) { # eg) Test: divisible by 23
    test[length(test)+1] <- strsplit(day11_input$input[row], "by ")[[1]][2]
  } else if (startsWith(day11_input$input[row], "If true: ")) { # eg) If true: throw to monkey 2
    test_if_true[length(test_if_true)+1] <- strsplit(day11_input$input[row], "monkey ")[[1]][2]
  } else if (startsWith(day11_input$input[row], "If false: ")) { # eg) If false: throw to monkey 3
    test_if_false[length(test_if_false)+1] <- strsplit(day11_input$input[row], "monkey ")[[1]][2]
  }
}

monkey_index <- as.numeric(monkey_index)
test <- as.numeric(test)
test_if_true <- as.numeric(test_if_true)
test_if_false <- as.numeric(test_if_false)
for (i in 1:length(starting_items)) {
  starting_items[[i]] <- as.numeric(starting_items[[i]])
}

num_monkeys <- length(monkey_index)
inspections <- vector(mode="numeric", length=num_monkeys)

for (round in 1:num_rounds) {
  for (monkey in 1:num_monkeys) {
    items <- starting_items[[monkey]]
    num_items <- length(items)
    
    if (num_items != 0) {
      for (item in 1:num_items) {
        old <- items[item]
        new <- eval(parse(text=operation[monkey]))
        new <- floor(new/3)
        
        test_result <- new %% test[monkey] == 0
        if (test_result) { target_monkey <- test_if_true[monkey] + 1 }
        else { target_monkey <- test_if_false[monkey] + 1 }
        
        starting_items[[target_monkey]][length(starting_items[[target_monkey]])+1] <- new
      }
    }
    starting_items[[monkey]] <- vector(mode="numeric", length=0)
    inspections[monkey] <- inspections[monkey] + num_items
  }
}
inspections_ordered <- inspections[order(inspections, decreasing = TRUE)]
monkeybusiness <- inspections_ordered[1] * inspections_ordered[2]

cat(paste0("Part 1: The level of monkey business after ", num_rounds, " rounds of shenanigans is ", monkeybusiness, "." ))
