
# Goal: Advent of Code, DAY 15 - https://adventofcode.com/2022/day/15
# 1) import files
# 2) code, part 1
# 3) code, part 2


# ----------------------------------------------------------------------------
# (1) import files
# ----------------------------------------------------------------------------

library(data.table)
library(dplyr)

# INPUT
day15_input <- #data.frame(input=c("Sensor at x=2, y=18: closest beacon is at x=-2, y=15","Sensor at x=9, y=16: closest beacon is at x=10, y=16","Sensor at x=13, y=2: closest beacon is at x=15, y=3","Sensor at x=12, y=14: closest beacon is at x=10, y=16","Sensor at x=10, y=20: closest beacon is at x=10, y=16","Sensor at x=14, y=17: closest beacon is at x=10, y=16","Sensor at x=8, y=7: closest beacon is at x=2, y=10","Sensor at x=2, y=0: closest beacon is at x=2, y=10","Sensor at x=0, y=11: closest beacon is at x=2, y=10","Sensor at x=20, y=14: closest beacon is at x=25, y=17","Sensor at x=17, y=20: closest beacon is at x=21, y=22","Sensor at x=16, y=7: closest beacon is at x=15, y=3","Sensor at x=14, y=3: closest beacon is at x=15, y=3","Sensor at x=20, y=1: closest beacon is at x=15, y=3"))
  fread("C:\\Users\\irenafeng\\Downloads\\advent of code\\day15-input.csv", sep="", quote=FALSE)

sensor_x <- vector()
sensor_y <- vector()
beacon_x <- vector()
beacon_y <- vector()

for (i in 1:nrow(day15_input)) {
  day15_input$input[i] <- substr(day15_input$input[i], 2, nchar(day15_input$input[i])-1)
  
  sensor_string <- strsplit(day15_input$input[i], ": ")[[1]][1] # Sensor at x=2, y=18
  beacon_string <- strsplit(day15_input$input[i], ": ")[[1]][2] # closest beacon is at x=-2, y=15
  
  sensor_x <- append(sensor_x, strsplit(sensor_string, "[,=]+")[[1]][2])
  sensor_y <- append(sensor_y, strsplit(sensor_string, "[,=]+")[[1]][4])
  
  beacon_x <- append(beacon_x, strsplit(beacon_string, "[,=]+")[[1]][2])
  beacon_y <- append(beacon_y, strsplit(beacon_string, "[,=]+")[[1]][4])
}

sensor_x <- as.numeric(sensor_x)
sensor_y <- as.numeric(sensor_y)
beacon_x <- as.numeric(beacon_x)
beacon_y <- as.numeric(beacon_y)

distances <- vector()

for (i in 1:nrow(day15_input)) {
  distance <- abs(sensor_x[i] - beacon_x[i]) + abs(sensor_y[i] - beacon_y[i])
  distances <- append(distances, distance)
}


# ----------------------------------------------------------------------------
# (2) code, part 1
# ----------------------------------------------------------------------------

occupied_x <- vector()
y_of_interest <- 2000000 # the row we care about

beacons_in_y <- beacon_x[which(beacon_y==y_of_interest)]
num_beacons_in_y <- length(unique(beacons_in_y))

for (i in 1:nrow(day15_input)) {
  sx <- sensor_x[i]
  sy <- sensor_y[i]
  dis <- distances[i]
  
  if (abs(sy - y_of_interest) <= dis) {
    num_pts <- abs(abs(sy - y_of_interest) - dis) * 2 + 1
    min_x <- sx - floor(num_pts/2)
    max_x <- sx + floor(num_pts/2)
    
    occupied_x <- append(occupied_x, min_x:max_x)
  }
}

pt1_ans <- length(unique(occupied_x)) - num_beacons_in_y
cat(paste0("Part 1: In row ", y_of_interest, ", ", pt1_ans, " positions cannot contain a beacon."))


# ----------------------------------------------------------------------------
# (3) code, part 2
# ----------------------------------------------------------------------------

part2 <- data.frame(sensor_x = sensor_x,
                    sensor_y = sensor_y,
                    beacon_x = beacon_x,
                    beacon_y = beacon_y,
                    distance = distances,
                    t_min = sensor_x + sensor_y - distances,
                    t_max = sensor_x + sensor_y + distances,
                    u_min = sensor_x - sensor_y - distances,
                    u_max = sensor_x - sensor_y + distances)

possible_t <- c()
possible_u <- c()

for (i in 1:(nrow(part2)-1)) {
  for (j in (i+1):nrow(part2)) {
    tmin1 <- part2$t_min[i] - 1
    tmin2 <- part2$t_min[j] - 1
    tmax1 <- part2$t_max[i] + 1
    tmax2 <- part2$t_max[j] + 1
    
    umin1 <- part2$u_min[i] - 1
    umin2 <- part2$u_min[j] - 1
    umax1 <- part2$u_max[i] + 1
    umax2 <- part2$u_max[j] + 1
    
    t_minmax <- c(tmin1, tmin2, tmax1, tmax2)
    u_minmax <- c(umin1, umin2, umax1, umax2)
    duplicate_rows <- t_minmax[duplicated(t_minmax)]
    duplicate_cols <- u_minmax[duplicated(u_minmax)]
    
    if (length(duplicate_rows) > 0) { possible_t <- append(possible_t, duplicate_rows) }
    if (length(duplicate_cols) > 0) { possible_u <- append(possible_u, duplicate_cols) }
  }
}

possible_pts <- expand.grid(t = unique(possible_t),
                            u = unique(possible_u))

possible_pts_filtered <- data.frame(t = vector(), u = vector())
for (i in 1:nrow(possible_pts)) {
  t <- possible_pts$t[i]
  u <- possible_pts$u[i]
  
  if (t %% 2 == u %% 2) { possible_pts_filtered[nrow(possible_pts_filtered)+1,] <- c(t, u) }
}

possible_pts_filtered$x <- (possible_pts_filtered$t + possible_pts_filtered$u) / 2
possible_pts_filtered$y <- (possible_pts_filtered$t - possible_pts_filtered$u) / 2

max_search <- 4000000
xy_filtered <- data.frame(x = vector(), y = vector())
for (i in 1:nrow(possible_pts_filtered)) {
  x <- possible_pts_filtered$x[i]
  y <- possible_pts_filtered$y[i]
  
  if (x >= 0 & y >= 0 & x <= max_search & y <= max_search) { xy_filtered[nrow(xy_filtered)+1,] <- c(x, y)}
}

x_values <- xy_filtered$x
y_values <- xy_filtered$y


part2_mod <- part2 %>%
  select(sensor_x, sensor_y, distance) %>%
  left_join(xy_filtered, by = character()) %>%
  mutate(pt_dist_to_sensor = abs(sensor_x - x) + abs(sensor_y - y)) %>%
  mutate(far_enough = (pt_dist_to_sensor > distance)) %>%
  group_by(x, y) %>%
  summarise(hidden_from = sum(far_enough), .groups="drop") %>%
  filter(hidden_from == nrow(part2))

ans_x <- part2_mod$x[1]
ans_y <- part2_mod$y[1]

freq <- ans_x * 4000000 + ans_y
cat(paste0("Part 2: The tuning frequency of the distress beacon is ", freq, "."))


