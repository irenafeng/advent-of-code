
# Goal: Advent of Code, DAY 7 - https://adventofcode.com/2022/day/7
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)

# INPUT
day7_input <- #data.frame(output=c("$ cd /","$ ls","dir a","14848514 b.txt","8504156 c.dat","dir d","$ cd a","$ ls","dir e","29116 f","2557 g","62596 h.lst","$ cd e","$ ls","584 i","$ cd ..","$ cd ..","$ cd d","$ ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"))
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day07-input.csv", sep="")


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

location <- ""
files <- data.frame(file=vector(),
                    size=vector())
folders <- data.frame(folder=vector(),
                      size=vector())
folders[nrow(folders)+1,] <- c("",0)


for (i in 1:nrow(day7_input)) {
  command <- day7_input$output[i]
  print("----------------------------------------------------------------------------")
  print(command)
  
  if (startsWith(command,"$ ")) { # command we input
    if (startsWith(command, "$ cd ")) { # changing directory, doesn't need an 'else' statement for "$ ls"
      dir <- substr(command, 6, nchar(command))
      if (dir == "/") { # master directory
        location <- ""
      } else if (dir == "..") { # going up a directory
        location <- capture.output(cat(strsplit(location,"/")[[1]][1:length(strsplit(location,"/")[[1]])-1],sep="/"))
      } else { # going into a directory
        location <- capture.output(cat(location,dir,sep="/"))
      }
    }
  } else { # output we get from a command
    file_size <- strsplit(day7_input$output[i], " ")[[1]][1]
    file_name <- strsplit(day7_input$output[i], " ")[[1]][2]
    full_file_name <- capture.output(cat(location,file_name,sep="/"))
    
    if (file_size == "dir") { # if the file listed is a directory, add to folders
      folders[nrow(folders)+1,] <- c(full_file_name,0)
      # print(folders)
    } else { # if the file listed is a file with a size
      file_size <- strtoi(file_size)
      files[nrow(files)+1,] <- c(full_file_name, file_size)
      
      files <- transform(files,size=as.numeric(size))
      folders <- transform(folders,size=as.numeric(size))
      
      folders[folders$folder==location,]$size <- folders[folders$folder==location,]$size + file_size
      
      # print("location:")
      # print(location)
      
      # if (!identical(location, "")) { # if we aren't in master directory right now
      #   print("if statement not in master")
      #   upper_location_temp <- capture.output(cat(strsplit(location,"/")[[1]][1:length(strsplit(location,"/")[[1]])-1],sep="/"))
      #   print(upper_location_temp)
      # }

      upper_location_temp <- capture.output(cat(strsplit(location,"/")[[1]][1:length(strsplit(location,"/")[[1]])-1],sep="/"))
      # print(upper_location_temp)
      
      while (!identical(upper_location_temp, character(0))) {
        print("made it into while loop")
        folders[folders$folder==upper_location_temp,]$size <- folders[folders$folder==upper_location_temp,]$size + file_size
        upper_location_temp <- capture.output(cat(strsplit(upper_location_temp,"/")[[1]][1:length(strsplit(upper_location_temp,"/")[[1]])-1],sep="/"))
        print(upper_location_temp)
        # print(folders)
      }
      
      if (length(strsplit(location,"/")[[1]]) >= 2) {
        folders[folders$folder=="",]$size <- folders[folders$folder=="",]$size + file_size
        print("got in if #1 end")
      }
      
      # if (identical(upper_location_temp, character(0))) {
      #   folders[folders$folder=="",]$size <- folders[folders$folder=="",]$size + file_size
      #   print("got in if #2 end")
      # }
      
      # print(folders)
      # print(files)
    }
  }
}

dir_file_sizes_100000 <- subset(folders, folders$size <= 100000)
sum_directories_100000 <- sum(dir_file_sizes_100000$size)

cat(paste0("Part 1: The total sizes of directories of size at most 100000 is ", sum_directories_100000, "."))


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------

SPACE_NEEDED_TOTAL <- 30000000
SPACE_LEFT <- 70000000 - folders[folders$folder=="",]$size
SPACE_NEEDED <- SPACE_NEEDED_TOTAL - SPACE_LEFT

ordered_folders <- folders[order(folders$size),]
ordered_folders_space_needed <- subset(ordered_folders, ordered_folders$size >= SPACE_NEEDED)
dir_to_delete_size <- ordered_folders_space_needed[1,2]

cat(paste0("Part 2: The size of the smallest directory that could be deleted to free up space is ", dir_to_delete_size, "."))
