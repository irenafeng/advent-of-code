
# Goal: Advent of Code, DAY 18 - https://adventofcode.com/2022/day/18
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)
library(dplyr)
library(tidyr)

# INPUT
day18_input <- #data.frame(input=c("2,2,2","1,2,2","3,2,2","2,1,2","2,3,2","2,2,1","2,2,3","2,2,4","2,2,6","1,2,5","3,2,5","2,1,5","2,3,5"))
  fread("C:\\Users\\irenafeng\\Downloads\\AOC\\day18-input.csv", sep="", quote=FALSE)

input <- tibble(x=vector(mode="numeric", length=nrow(day18_input)),
                    y=vector(mode="numeric", length=nrow(day18_input)),
                    z=vector(mode="numeric", length=nrow(day18_input)))
for (i in 1:nrow(day18_input)) {
  if (startsWith(day18_input$input[i], "\"")) { day18_input$input[i] <- substr(day18_input$input[i], 2, nchar(day18_input$input[i])-1) }
  
  input$x[i] <- strsplit(day18_input$input[i], ",")[[1]][1] %>% as.numeric()
  input$y[i] <- strsplit(day18_input$input[i], ",")[[1]][2] %>% as.numeric()
  input$z[i] <- strsplit(day18_input$input[i], ",")[[1]][3] %>% as.numeric()
}


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

neighbors <- tibble(next_x=c(1,-1,0,0,0,0),
                    next_y=c(0,0,1,-1,0,0),
                    next_z=c(0,0,0,0,1,-1))

neighboring_pts <- input %>%
  left_join(neighbors, by=character()) %>%
  mutate(x = x+next_x,
         y = y+next_y,
         z = z+next_z)

outside <- anti_join(neighboring_pts, input, by=c("x","y","z"))
surface_area <- nrow(outside)

cat(paste0("Part 1: The surface area of the scanned lava droplet is ", surface_area, "."))


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------

# Naturage's "poor man's solution"
# x [0:19], y [0:19], z [1:19]
# using -1 and 21 to encompass that?
exterior <- expand_grid(x=c(-1,21),y=c(-1,21),z=c(-1,21))
num_exterior <- nrow(exterior)
neighbors2 <- neighbors %>%
  add_row(next_x=0,
          next_y=0,
          next_z=0)

while (TRUE) {
  exterior <- exterior %>%
    left_join(neighbors2, by=character()) %>%
    mutate(x = x+next_x,
           y = y+next_y,
           z = z+next_z)
  
  exterior <- exterior %>%
    select(-starts_with("next")) %>%
    distinct(x,y,z) %>%
    anti_join(input, by=c("x","y","z")) %>%
    filter(x %in% -1:21 & y %in% -1:21 & z %in% -1:21)
  
  if (num_exterior == nrow(exterior)) { break }
  else { num_exterior <- nrow(exterior) }
}

sides_external <- outside %>%
  semi_join(exterior, by=c("x","y","z"))

exterior_SA <- nrow(sides_external)
cat(paste0("Part 2: The exterior surface area of the scanned lava droplet is ", exterior_SA, "."))