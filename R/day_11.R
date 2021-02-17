
# Problem 1 ===================================================================
# Your plane lands with plenty of time to spare. The final leg of your journey
# is a ferry that goes directly to the tropical island where you can finally
# start your vacation. As you reach the waiting area to board the ferry, you
# realize you're so early, nobody else has even arrived yet!
#
# By modeling the process people use to choose (or abandon) their seat in the
# waiting area, you're pretty sure you can predict the best place to sit. You
# make a quick map of the seat layout (your puzzle input).
#
# The seat layout fits neatly on a grid. Each position is either floor (.), an
# empty seat (L), or an occupied seat (#). For example, the initial seat layout
# might look like this:
#
#   L.LL.LL.LL
#   LLLLLLL.LL
#   L.L.L..L..
#   LLLL.LL.LL
#   L.LL.LL.LL
#   L.LLLLL.LL
#   ..L.L.....
#   LLLLLLLLLL
#   L.LLLLLL.L
#   L.LLLLL.LL
#   Now, you just need to model the people who will be arriving shortly.
# Fortunately, people are entirely predictable and always follow a simple set
# of rules. All decisions are based on the number of occupied seats adjacent to
# a given seat (one of the eight positions immediately up, down, left, right,
# or diagonal from the seat). The following rules are applied to every seat
# simultaneously:
#
# If a seat is empty (L) and there are no occupied seats adjacent to it, the
# seat becomes occupied.
# If a seat is occupied (#) and four or more seats adjacent to it are also
# occupied, the seat becomes empty.
# Otherwise, the seat's state does not change.
# Floor (.) never changes; seats don't move, and nobody sits on the floor.
#
# After one round of these rules, every seat in the example layout becomes occupied:
#
#     #.##.##.##
#     #######.##
#     #.#.#..#..
#     ####.##.##
#     #.##.##.##
#     #.#####.##
#     ..#.#.....
#     ##########
#     #.######.#
#     #.#####.##
# After a second round, the seats with four or more occupied adjacent seats
# become empty again:
#
#     .LL.L#.##
#     LLLLLL.L#
#     .L.L..L..
#     #LLL.LL.L#
#     #.LL.LL.LL
#     #.LLLL#.##
#     ..L.L.....
#     #LLLLLLLL#
#     #.LLLLLL.L
#     #.#LLLL.##
#     This process continues for three more rounds:
#
#     #.##.L#.##
#     #L###LL.L#
#     L.#.#..#..
#     #L##.##.L#
#     #.##.LL.LL
#     #.###L#.##
#     ..#.#.....
#     #L######L#
#     #.LL###L.L
#     #.#L###.##
#     #.#L.L#.##
#     #LLL#LL.L#
#     L.L.L..#..
#     #LLL.##.L#
#     #.LL.LL.LL
#     #.LL#L#.##
#     ..L.L.....
#     #L#LLLL#L#
#     #.LLLLLL.L
#     #.#L#L#.##
#     #.#L.L#.##
#     #LLL#LL.L#
#     L.#.L..#..
#     #L##.##.L#
#     #.#L.LL.LL
#     #.#L#L#.##
#     ..L.L.....
#     #L#L##L#L#
#     #.LLLLLL.L
#     #.#L#L#.##
#     At this point, something interesting happens: the chaos stabilizes and
# further applications of these rules cause no seats to change state! Once
# people stop moving around, you count 37 occupied seats.
#
# Simulate your seating area by applying the seating rules repeatedly until
# no seats change state. How many seats end up occupied?

# Problem 1 functions =========================================================

seat_swap <- function(char, on = "#", off = "L", ignore = "."){

  if(char == ignore){
    return(ignore)
  }
  else if( char == on){
    return(off)
  }
  else{
    return(on)
  }

}

check_swap <- function(board, col, row, on = "#", off = "L", ignore = "."){

  min <- 1
  max_row <- nrow(board)
  max_col <- ncol(board)
  # Get 4 points around the chair

  points <- tribble(
    ~point  , ~col_n, ~row_n,
    "left"  ,  col-1,    row,
    "right" ,  col+1,    row,
    "up"    ,    col,  row-1,
    "down"  ,    col,  row+1,
    "up_l"  ,  col-1,  row-1,
    "down_l",  col-1,  row+1,
    "up_r"  ,  col+1,  row-1,
    "down_r",  col+1,  row+1,
  )

  valid_points <- points$row_n >= min &
    points$row_n <= max_row &
    points$col_n >= min &
    points$col_n <= max_col

  valid_points <- points[valid_points,]


  # Check surrounding seats

  surrounding_seats <- c()
  for(i in 1:nrow(valid_points)){
    tmp_row <- valid_points$row_n[i]
    tmp_col <- valid_points$col_n[i]


    tmp_seat <- board[tmp_row, tmp_col]
    print(paste0("adj how: ",valid_points$point[i],
                 " row: ", tmp_row,
                 " col: ", tmp_col,
                 "is ", tmp_seat))
    surrounding_seats <- c(surrounding_seats, tmp_seat)
  }

  # Ignore missing seats
  surrounding_seats <- surrounding_seats[surrounding_seats != ignore]

  # Check other swap conditions
  if(board[col, row] %in% c(off, ignore) ){
    test <- all(surrounding_seats %in% c(off))
  } else{
    test <- sum(surrounding_seats %in% c(on)) >= 4
  }


  test
}


update_board <- function(board, on = "#", off = "L", ignore = "."){

  new_board <- board

  print(nrow(board))
  for(r in 1:nrow(board)){
    for(c in 1:(ncol(board))){
      print(paste0("Row: ", r, "Col: ", c))
      swap_seat <- check_swap(board, col = c, row = r)
      print(swap_seat)
      if(swap_seat){
        new_board[r,c] <- seat_swap(board[r,c])
      }
    }

  }

  return(new_board)
}


sim_seats <- function(board){
  last_board <- board
  new_board  <- update_board(board)

  identical(board, new_board)
  i <- 1
  while(!identical(last_board, new_board)){
    print(paste0("swap ", i))
    i = i+1
    last_board <- new_board
    new_board  <- update_board(new_board)


  }
}






# Problem 1 testing ===========================================================
library(tidyverse)
seats <- read.fwf(
  "input/day_11.txt",
  widths = rep(1, 98),
  stringsAsFactors = FALSE,
  header = FALSE
  )


sample <- read.fwf(
  "input/day_11_sample.txt",
  widths = rep(1, 10),
  stringsAsFactors = FALSE,
  header = FALSE
)

seat_swap(seats[1,1])
check_swap(seats, 1,1)
sim_seats(sample)

new_sample1 <- update_board(sample)
new_sample2 <- update_board(new_sample1)
new_sample3 <- update_board(new_sample2)
new_sample4 <- update_board(new_sample3)
new_sample5 <- update_board(new_sample4)
