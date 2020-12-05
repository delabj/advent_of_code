# --- Day 5: Binary Boarding ---
# Problem 1 ===================================================================
# You board your plane only to discover a new problem: you dropped your
# boarding pass! You aren't sure which seat is yours, and all of the flight
# attendants are busy with the flood of people that suddenly made it through
# passport control.
#
# You write a quick program to use your phone's camera to scan all of the
# nearby boarding passes (your puzzle input); perhaps you can find your seat
# through process of elimination.
#
# Instead of zones or groups, this airline uses binary space partitioning to
# seat people. A seat might be specified like FBFBBFFRLR, where F means
# "front", B means "back", L means "left", and R means "right".
#
# The first 7 characters will either be F or B; these specify exactly one of
# the 128 rows on the plane (numbered 0 through 127). Each letter tells you
# which half of a region the given seat is in. Start with the whole list of
# rows; the first letter indicates whether the seat is in the front (0 through
# 63) or the back (64 through 127). The next letter indicates which half of
# that region the seat is in, and so on until you're left with exactly one row.
#
# For example, consider just the first seven characters of FBFBBFFRLR:
#
# Start by considering the whole range, rows 0 through 127.
# F means to take the lower half, keeping rows 0 through 63.
# B means to take the upper half, keeping rows 32 through 63.
# F means to take the lower half, keeping rows 32 through 47.
# B means to take the upper half, keeping rows 40 through 47.
# B keeps rows 44 through 47.
# F keeps rows 44 through 45.
# The final F keeps the lower of the two, row 44.
# The last three characters will be either L or R; these specify exactly one of
# the 8 columns of seats on the plane (numbered 0 through 7). The same process
# as above proceeds again, this time with only three steps. L means to keep the
# lower half, while R means to keep the upper half.
#
# For example, consider just the last 3 characters of FBFBBFFRLR:
#
# Start by considering the whole range, columns 0 through 7.
# R means to take the upper half, keeping columns 4 through 7.
# L means to take the lower half, keeping columns 4 through 5.
# The final R keeps the upper of the two, column 5.
# So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.
#
# Every seat also has a unique seat ID: multiply the row by 8, then add the
# column. In this example, the seat has ID 44 * 8 + 5 = 357.
#
# Here are some other boarding passes:
#
# BFFFBBFRRR: row 70, column 7, seat ID 567.
# FFFBBBFRRR: row 14, column 7, seat ID 119.
# BBFFBBFRLL: row 102, column 4, seat ID 820.
# As a sanity check, look through your list of boarding passes. What is the
# highest seat ID on a boarding pass?

# Problem 1 function ==========================================================

#' Check that the length is a power of 2
#'
#' @param min The lower end of the range
#' @param max The upper end of the range
#'
#' @return the power of 2
check_len_power_of_2 <- function(min, max){
  len <- length(min:max)
  if(!(log2(len)%%1==0) | len==0){
    usethis::ui_stop(
      paste0(
        "The Length of ",
        min,
        ":",
        max,
        " is not a power of 2"))
  }
  return(log2(len))
}

#' Get the value from a binary space
#'
#' @param text, the text with the binary sequence
#' @param off the Character that represents off
#' @param on the character that represents on
#'
#' @return the base ten integer represented by the string
get_value <- function(text, off="F", on="B"){

  binary_row <- gsub(pattern = off, 0, text)
  binary_row <- gsub(pattern = on, 1, binary_row)

  return(strtoi(binary_row,2))
}

#' Get the seat positions given an id
#'
#' @param id the string ID
#' @param row_min the minimum row number
#' @param row_max the maximum row number
#' @param col_min the minimum column number
#' @param col_max the maximum column number
#' @param back the string represeting a seat in the back
#' @param front the string representing a seat in the front
#' @param left the string representing a seat on the left
#' @param right the strin representing a seat on the right
#'
#' @details This function assums that the seats start at 0 and
#' increment going from front (row min) to back (row max) and
#' right (col min) to left (col min)
#'
#' @return A named list of coords

get_seat_positions <- function(
  id,
  row_min = 0,
  row_max = 127,
  col_min = 0,
  col_max = 7,
  back="B",
  front = "F",
  left = "L",
  right = "R"
  ){


  num_rows <- check_len_power_of_2(row_min, row_max)
  num_cols <- check_len_power_of_2(col_min, col_max)
  row_string <- substr(id, 0, num_rows)
  col_string <- substr(id, num_rows+1, nchar(id))
  row_num <- get_value(row_string, front, back)
  col_num <- get_value(col_string, left,  right)

  return(list("row" = row_num, "col" = col_num))

}

#' Get a seat ID given the row and column
#'
#' @param row the row number of the seat
#' @param col the column number of the seat
#' @param row_fac the factor to scale rows by
#' @param col_fac the factor to scale columns by
#'
#' @return a seat ID number
get_seat_id <- function(row, col, row_fac=8, col_fac=1){
  seat_id <- row*row_fac + col*col_fac
  return(seat_id)
}




# Problem 1 Results ===========================================================
boarding_passes <- read.csv(
  "~/GitHub/advent_of_code/input/day_05.txt",
  sep="",
  stringsAsFactors = FALSE,
  header = FALSE
  )


postions <- (get_seat_positions(id = boarding_passes$V1))
boarding_passes$col <- postions$col
boarding_passes$row <- postions$row
boarding_passes$seat_id <- get_seat_id(boarding_passes$row, boarding_passes$col)
max(boarding_passes$seat_id)

# tidyverse?
library(tidyverse)
boarding_passes %>%
  dplyr::mutate(
    col_str = stringr::str_sub(V1, start = 8),
    row_str = stringr::str_sub(V1, start = 0, end = 7),
    col_val = get_value(col_str, "L", "R"),
    row_val = get_value(row_str, "F", "B"),
    seat_id = get_seat_id(row_val, col_val, 8, 1)
           ) %>%
  summarise(max_id = max(seat_id))



# Problem 2 ===================================================================
# Ding! The "fasten seat belt" signs have turned on. Time to find your seat.
#
# It's a completely full flight, so your seat should be the only missing
# boarding pass in your list. However, there's a catch: some of the seats at
# the very front and back of the plane don't exist on this aircraft, so they'll
# be missing from your list as well.
#
# Your seat wasn't at the very front or back, though; the seats with IDs +1 and
# -1 from yours will be in your list.
#
# What is the ID of your seat?



# Problem 2 Testing ===========================================================

# Cheese it with this
plot(boarding_passes$row, boarding_passes$col)

get_seat_id(row = 78, col = 5)




# tidyverse?
library(tidyverse)
boarding_passes %>%
  dplyr::mutate(
    col_str = stringr::str_sub(V1, start = 8),
    row_str = stringr::str_sub(V1, start = 0, end = 7),
    col_val = get_value(col_str, "L", "R"),
    row_val = get_value(row_str, "F", "B"),
    seat_id = get_seat_id(row_val, col_val, 8, 1)
  ) %>%
  arrange(row_val, col_val) %>%
  group_by(col_val) %>%
  mutate(diff_row_above = row_val-lag(row_val),
         diff_row_below= lead(row_val)-row_val) %>%
  filter(diff_row_above > 1 | diff_row_below > 1) %>%
  summarise(
    row = mean(row_val),
    col = mean(col_val)
  ) %>%
  mutate(get_seat_id(row, col))
