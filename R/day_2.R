# --- Day 2: Password Philosophy ---
# Problem Statement ===========================================================
#   Your flight departs in a few days from the coastal airport; the easiest way
# down to the coast from here is via toboggan.
#
# The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day.
# "Something's wrong with our computers; we can't log in!" You ask if you can
# take a look.
#
# Their password database seems to be a little corrupted: some of the
# passwords wouldn't have been allowed by the Official Toboggan Corporate
# Policy that was in effect when they were chosen.
#
# To try to debug the problem, they have created a list (your puzzle input) of
# passwords (according to the corrupted database) and the corporate policy when
# that password was set.
#
# For example, suppose you have the following list:
#
# 1-3 a: abcde
# 1-3 b: cdefg
# 2-9 c: ccccccccc
# Each line gives the password policy and then the password. The password
# policy indicates the lowest and highest number of times a given letter must
# appear for the password to be valid. For example, 1-3 a means that the
# password must contain a at least 1 time and at most 3 times.
#
# In the above example, 2 passwords are valid. The middle password, cdefg, is
# not; it contains no instances of b, but needs at least 1. The first and third
# passwords are valid: they contain one a or nine c, both within the limits of
# their respective policies.
#
# How many passwords are valid according to their policies?


# Solution function ===========================================================

#' Count the number of rows
#'
#' @description this function assumes that n and m are in the correct order and
#' that
#'
#' @param times A vector of the number of times formatted as `n-m`
#' @param char A vector of the characters to search for
#'             formatted with a colon after it.
#' @param text A vector of the texts to search
#'
#' @return A number: The count that totals the number of rows with character
#'         occurrences between n and m
count_text_with_char_between <- function(times, char, text){

  df <- data.frame(
    times = times,
    char = gsub(":", "", char),
    text = text,
    stringsAsFactors = FALSE
  )

  tmp <- stringr::str_split_fixed(df$times, "-", n = 2)
  df$n <- as.numeric(tmp[,1])
  df$m <- as.numeric(tmp[,2])
  df$occur <- stringr::str_count(df$text,df$char)


  df$valid <- ifelse((df$occur>=df$n) & (df$occur <= df$m), TRUE, FALSE)

  df <- filter(df, valid)

  return(nrow(df))



}

# Test Part 1 =================================================================


# Read in the data
data <- read.csv(
  "input/day_2.txt",
  sep = " ",
  header = FALSE,
  stringsAsFactors = FALSE
  )
names(data) <- c("allowed_times", "letter", "text_to_check")

# Run the function
count_text_with_char_between(times =data$allowed_times, data$letter, data$text_to_check)



# This Tidyverse pipeline works.
data %>%
    separate(
      allowed_times,
      c("n","m"),
      sep = "-",
      remove = FALSE
      ) %>%
    mutate(
      n = as.numeric(n),
      m = as.numeric(m)
      ) %>%
    transmute(id = row_number(),
      text = text_to_check,
      char = gsub(":", "", letter),
      min_val = if_else(m>n, n, m),
      max_val = if_else(n>m, n, m),
      occur = str_count(text, char)
    ) %>%
    filter(
      occur >= min_val,
      occur <= max_val
      ) %>%
    nrow()

# Part 2 ======================================================================
# While it appears you validated the passwords correctly, they don't seem to be
# what the Official Toboggan Corporate Authentication System is expecting.
#
# The shopkeeper suddenly realizes that he just accidentally explained the
# password policy rules from his old job at the sled rental place down the
# street! The Official Toboggan Corporate Policy actually works a little
# differently.
#
# Each policy actually describes two positions in the password, where 1 means
# the first character, 2 means the second character, and so on. (Be careful;
# Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of
# these positions must contain the given letter. Other occurrences of the
# letter are irrelevant for the purposes of policy enforcement.
#
# Given the same example list from above:
#
# 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
# 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
# 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
# How many passwords are valid according to the new interpretation of the
# policies?


# Part 2 Function =============================================================

#' Check if Character is in exactly one given position
#'
#' @param times A vector of the number of times formatted as `n-m`
#' @param char A vector of the characters to search for
#'             formatted with a colon after it.
#' @param text A vector of the texts to search
#'
#' @return A number: The count of rows with exactly one match in positions
check_char_in_exactly_1_pos <- function(times, char, text){

  df <- data.frame(
    times = times,
    char = gsub(":", "", char),
    text = text,
    stringsAsFactors = FALSE
  )

  tmp <- stringr::str_split_fixed(df$times, "-", n = 2)
  df$n <- as.numeric(tmp[,1])
  df$m <- as.numeric(tmp[,2])

  # I notice I'm repeating myself, I should think about creating subfunctions if
  # this was not a one off.

  df$valid <- xor(
    substr(df$text, df$n, df$n) == df$char,
    substr(df$text, df$m, df$m) == df$char
    )

  df <- filter(df, valid)
  return(nrow(df))


}

# Test Part 2 =================================================================


check_char_in_exactly_one_pos(times =data$allowed_times, data$letter, data$text_to_check)



# This Tidyverse pipeline works

data %>%
  separate(allowed_times, c("n","m"), sep = "-",remove = FALSE) %>%
  mutate(
    n = as.numeric(n),
    m = as.numeric(m)
  ) %>%
  mutate(
    text = text_to_check,
    char = gsub(":", "", letter),
    pos_n = substr(text,n, n),
    pos_m = substr(text,m, m),
    pos_n_match = ifelse(pos_n == char, 1, 0),
    pos_m_match = ifelse(pos_m == char, 1, 0),
    total_matches = pos_n_match+pos_m_match
  ) %>%
  filter(total_matches == 1) %>%
  nrow()
