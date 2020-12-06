# --- Day 6: Custom Customs ---
# Problem 1 ===================================================================
# As your flight approaches the regional airport where you'll switch to a much
# larger plane, customs declaration forms are distributed to the passengers.
#
# The form asks a series of 26 yes-or-no questions marked a through z. All you
# need to do is identify the questions for which anyone in your group answers
# "yes". Since your group is just you, this doesn't take very long.
#
# However, the person sitting next to you seems to be experiencing a language
# barrier and asks if you can help. For each of the people in their group, you
# write down the questions for which they answer "yes", one per line. For
# example:
#
# abcx
# abcy
# abcz
# In this group, there are 6 questions to which anyone answered "yes":
# a, b, c, x, y, and z. (Duplicate answers to the same question don't count
# extra; each question counts at most once.)
#
# Another group asks for your help, then another, and eventually you've
# collected answers from every group on the plane (your puzzle input). Each
# group's answers are separated by a blank line, and within each group, each
# person's answers are on a single line. For example:
#
# abc
#
# a
# b
# c
#
# ab
# ac
#
# a
# a
# a
# a
#
# b
# This list represents answers from five groups:
#
# The first group contains one person who answered "yes" to 3 questions: a, b,
# and c.
# The second group contains three people; combined, they answered "yes" to 3
# questions: a, b, and c.
# The third group contains two people; combined, they answered "yes" to 3
# questions: a, b, and c.
# The fourth group contains four people; combined, they answered "yes" to only
# 1 question, a.
# The last group contains one person who answered "yes" to only 1 question, b.
# In this example, the sum of these counts is 3 + 3 + 3 + 1 + 1 = 11.
#
# For each group, count the number of questions to which anyone answered
# "yes". What is the sum of those counts?



# Problem 1 function ==========================================================

#' Count unqiue questions answered by group
#'
#' @param text the string of the question
#'
#' @return the number of unique questions
count_questions <- function(text  ){
  chars <-  unlist(strsplit(text, "") )
  chars <- unique(chars)
  keep <- chars %in% letters



  length(chars[keep])
}



#' Read in the input as a data.frame
#'
#' @param path path to the file
#'
#' @return a data.frame
read_questions <- function(path){

  text <- readr::read_file(path)

  df <- data.frame(
    text = unlist(strsplit(text, "\r\n\r\n"))
  )

  df$text <- gsub("\r\n", "-",df$text)

  return(df)

}

# Problem 1 Testing ===========================================================

test <- data.frame(
  text =c("abc","a-b-c", "ab-ac", "a-a-a-a", "b"),
  stringsAsFactors = FALSE
  )
test$num_questions <- unlist(purrr::map(test$text, count_questions))
sum(test$num_questions)

qs <- read_questions(path = "input/day_06.txt")


qs$num_questions <- unlist(purrr::map(qs$text, count_questions))

sum(qs$num_questions)



# Problem 2 ===================================================================


# Problem 2 function ==========================================================


#' Count questions all group members have answered
#'
#' @param text text with questions
#'
#' @return a number
count_questions_all <- function(text){
  chars <-  data.frame(
    answers = unlist(strsplit(text, "-") )
  )
  vec <- sapply(
    X=as.character(chars$answers),
    FUN = strsplit,
    split="")

  length(purrr::reduce(vec,intersect))


}



# Problem 2 Testing ===========================================================

qs$num_all <- unlist(purrr::map(qs$text, count_questions_all))

sum(qs$num_all)

nmax <- max(stringr::str_count(qs$text, "-"))+2


