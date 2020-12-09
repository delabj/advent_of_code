# Problem 1 ===================================================================
# With your neighbor happily enjoying their video game, you turn your attention
# to an open data port on the little screen in the seat in front of you.
#
# Though the port is non-standard, you manage to connect it to your computer
# through the clever use of several paperclips. Upon connection, the port
# outputs a series of numbers (your puzzle input).
#
# The data appears to be encrypted with the eXchange-Masking Addition System
# (XMAS) which, conveniently for you, is an old cypher with an important
# weakness.
#
# XMAS starts by transmitting a preamble of 25 numbers. After that, each number
# you receive should be the sum of any two of the 25 immediately previous
# numbers. The two numbers will have different values, and there might be more
# than one such pair.
#
# For example, suppose your preamble consists of the numbers 1 through 25 in a
# random order. To be valid, the next number must be the sum of two of those
# numbers:
#
# 26 would be a valid next number, as it could be 1 plus 25 (or many other
# pairs, like 2 and 24).
# 49 would be a valid next number, as it is the sum of 24 and 25.
# 100 would not be valid; no two of the previous 25 numbers sum to 100.
# 50 would also not be valid; although 25 appears in the previous 25 numbers,
# the two numbers in the pair must be different.
# Suppose the 26th number is 45, and the first number (no longer an option, as
# it is more than 25 numbers ago) was 20. Now, for the next number to be valid,
# there needs to be some pair of numbers among 1-19, 21-25, or 45 that add up
# to it:
#
# 26 would still be a valid next number, as 1 and 25 are still within the
# previous 25 numbers.
# 65 would not be valid, as no two of the available numbers sum to it.
# 64 and 66 would both be valid, as they are the result of 19+45 and 21+45
# respectively.
# Here is a larger example which only considers the previous 5 numbers
# (and has a preamble of length 5):
#
# 35
# 20
# 15
# 25
# 47
# 40
# 62
# 55
# 65
# 95
# 102
# 117
# 150
# 182
# 127
# 219
# 299
# 277
# 309
# 576
# In this example, after the 5-number preamble, almost every number is the sum
# of two of the previous 5 numbers; the only number that does not follow this
# rule is 127.
#
# The first step of attacking the weakness in the XMAS data is to find the
# first number in the list (after the preamble) which is not the sum of two of
# the 25 numbers before it. What is the first number that does not have this
# property?

# Problem 1 Function ==========================================================

#' Find if two items in a list sum to a given number
#'
#' @param nums A list of numbers to check
#' @param sum number to sum to
#'
#' @return A list of either 2 numbers of NA
#'
#' @details Assumes that only one pair is needed
find_sum_components <- function(nums, sum){

  for(i in 1:length(nums)){
    compliment <- sum-nums[i]
    if(compliment %in% nums[-i]){
      n1 <- nums[i]
      n2 <- compliment
      break()
    }
    else{
      n1 <- NA
      n2 <- NA
    }
  }


  return(c(n1, n2))
}



#' check if a pair of compliments exsit for a set of numbers preceding
#'
#' @param list the list of numbers
#' @param lead the number to check before the current number
check_for_sum_lead <- function(list, lead){
  index <- lead + 1
  start <- index - lead
  stop  <- lead

  list_len <- length(list)
  for(i in index:list_len){
    numbers <- list[start:stop]
    sum <- list[index]

    tested <- find_sum_components(numbers, sum)
    if(is.na(tested[1])){
      return(list[index])
    }
    else{

    }

    print(paste("index:", index,
          "start:", start,
          "stop:" , stop))

    index = index +1
    start = start +1
    stop  = stop  +1

    if(index > list_len){
      break()
    }
  }
  print("end")

  tested
}

check_for_sum_lead(test,25)


# Part 2 ======================================================================

# Find contiguous sum to that number

#' Find contiguous groups of numbers that sum to a given value
#' @param list the list of numbers
#' @param sum_to the number to sum to
find_contiguous <- function(
  list,
  sum_to
){

  sums_foreword <- cumsum(list)
  sums_backwards <- cumsum(rev(list))
  sum_all <- sum(list)


  # find where foreword matches all - backwards and the sum to
  # if the cumulative sum foreword - the cumulative sum backwards
  # equal the total sum - the number of interest then they make up
  # the range.
  sums_check <- sum_all - sum_to - sums_backwards


  matched <-  (match(sums_foreword, sums_check))
  no_na   <- which(!is.na(matched))
  start   <- no_na[1]
  end     <- length(list) - matched[start]
  print(paste("Start", start, "end", end))
  start <- start+1
  list[start:end]
}

find_contiguous(test, 2020)
min(find_contiguous(test, 1639024365))+max(find_contiguous(test, 1639024365))
