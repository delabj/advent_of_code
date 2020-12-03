# --- Day 3: Toboggan Trajectory ---
# Problem 1 ===================================================================
#   With the toboggan login problems resolved, you set off toward the airport.
# While travel by toboggan might be easy, it's certainly not safe: there's very
# minimal steering and the area is covered in trees. You'll need to see which
# angles will take you near the fewest trees.
#
# Due to the local geology, trees in this area only grow on exact integer
# coordinates in a grid. You make a map (your puzzle input) of the open squares
# (.) and trees (#) you can see. For example:
#
# ..##.......
# #...#...#..
# .#....#..#.
# ..#.#...#.#
# .#...##..#.
# ..#.##.....
# .#.#.#....#
# .#........#
# #.##...#...
# #...##....#
# .#..#...#.#
# These aren't the only trees, though; due to something you read about once
# involving arboreal genetics and biome stability, the same pattern repeats to
# the right many times:
#
#   ..##.........##.........##.........##.........##.........##.......  --->
# #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
# .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
# ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
# .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
# ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
# .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
# .#........#.#........#.#........#.#........#.#........#.#........#
# #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
# #...##....##...##....##...##....##...##....##...##....##...##....#
# .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
# You start on the open square (.) in the top-left corner and need to reach the
# bottom (below the bottom-most row on your map).
#
# The toboggan can only follow a few specific slopes (you opted for a cheaper
# model that prefers rational numbers); start by counting all the trees you
# would encounter for the slope right 3, down 1:
#
# From your starting position at the top-left, check the position that is right
# 3 and down 1. Then, check the position that is right 3 and down 1 from there,
# and so on until you go past the bottom of the map.
#
# The locations you'd check in the above example are marked here with O where
# there was an open square and X where there was a tree:
#
# ..##.........##.........##.........##.........##.........##.......  --->
# #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
# .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
# ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
# .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
# ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
# .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
# .#........#.#........X.#........#.#........#.#........#.#........#
# #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
# #...##....##...##....##...#X....##...##....##...##....##...##....#
# .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
# In this example, traversing the map using this slope would cause you to
# encounter 7 trees.
#
# Starting at the top-left corner of your map and following a slope of right 3
# and down 1, how many trees would you encounter?


# Problem 1 Functions =========================================================

#' Calculate the new position
#'
#' @param x = x position
#' @param y = y position
#' @param rise = rise
#' @param run = run
new_position <- function(x, y, rise, run, repeats){


  new_y <-  y+rise
  new_x <-  (x+run) %% repeats

  new_x <- if_else(new_x == 0, 31,new_x)

  return(c(new_x, new_y))

}

#' Check position for tree
#' @param field a matrix of the field pattern
#' @param x x position
#' @param y y position
#' @return boolean
is_tree <- function(field,  x, y, tree_char){
  space <- field[y,x]

  if(space == tree_char){
    print("hit a tree")
    return(TRUE)
  }
  else{
    return(FALSE)
  }

}


#' Count the number of trees encountered given an initial position and slope
#'
#' @param field a matrix of the field pattern
#' @param initial_x initial x position (index starts at 1)
#' @param initial_y initial y position (index starts at 1)
#' @param rise change in y (+ for up, - for down)
#' @param run change in x (+ for left, - for right)
#'
#' @details position (1,1) is the top left corner.
count_trees <- function(field, initial_x, initial_y, rise, run, tree_char ="#" ){

  x_repeat <- ncol(field)
  y_length <- nrow(field)
  rise = rise * (-1) # we want this to increase when we go down.
  current_x <- initial_x
  current_y <- initial_y

  num_tree <- ifelse(is_tree(field, current_x, current_x, tree_char),1,0)


  while (current_y < y_length) {
    #make the move
    new_pos <- new_position(current_x, current_y, rise, run, x_repeat)

    num_tree <-num_tree+ ifelse(is_tree(field, new_pos[1], new_pos[2], tree_char),1,0)

    current_x <- new_pos[1]
    current_y <- new_pos[2]
  }

  return(num_tree)



}

# Problem 1 Test ==============================================================

scan(
  file = "input/day_03.txt",
  what = "character"
)-> data

str_split_fixed(data, "", 31) %>% as.matrix()->mat

count_trees(mat, 1,1 ,rise = -1,run = 3 )

# Problem 2 ===================================================================
# Time to check the rest of the slopes - you need to minimize the probability
# of a sudden arboreal stop, after all.
#
# Determine the number of trees you would encounter if, for each of the
# following slopes, you start at the top-left corner and traverse the map all
# the way to the bottom:
#
# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.
# In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s) respectively; multiplied together, these produce the answer 336.


# Problem 2 Function ==========================================================

#' test_multiple_slope_results
#' @param field the field
#' @param initial_x initial x position
#' @param initial_y initial y position
#' @param rise_list list of rise values
#' @param run_list list of run values
#' @param tree_char character of a tree
#'
#' @return A list of hit tree totals.
test_multiple_slope_results <- function(field, initial_x, initial_y, rise_list, run_list, tree_char="#"){
  # Could also quickly tweak to allow multiple starting positions

  hits <- c()

  for (i in 1:length(rise)) {
    print(
      paste0("testing rise: ", rise_list[i], " and run: ", run_list[i])
    )

    trees <- count_trees(
      field,
      initial_x,
      initial_y,
      rise = rise_list[i],
      run = run_list[i]
    )

    hits <- c(hits,
              trees
              )

  }

  return(hits)


}


# Problem 2 Test ==============================================================
run <-  c( 1, 3, 5, 7, 1)
rise <- c(-1,-1,-1,-1,-2)

hit_list <- test_multiple_slope_results(mat, 1,1, rise, run)

prod(hit_list)

