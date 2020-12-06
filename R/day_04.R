# --- Day 4: Passport Processing ---
# Problem 1 ===================================================================
# You arrive at the airport only to realize that you grabbed your North Pole
# Credentials instead of your passport. While these documents are extremely
# similar, North Pole Credentials aren't issued by a country and therefore
# aren't actually valid documentation for travel in most of the world.
#
# It seems like you're not the only one having problems, though; a very long
# line has formed for the automatic passport scanners, and the delay could
# upset your travel itinerary.
#
# Due to some questionable network security, you realize you might be able to
# solve both of these problems at the same time.
#
# The automatic passport scanners are slow because they're having trouble
# detecting which passports have all required fields. The expected fields
# are as follows:
#
# byr (Birth Year)
# iyr (Issue Year)
# eyr (Expiration Year)
# hgt (Height)
# hcl (Hair Color)
# ecl (Eye Color)
# pid (Passport ID)
# cid (Country ID)
# Passport data is validated in batch files (your puzzle input). Each passport
# is represented as a sequence of key:value pairs separated by spaces or
# newlines. Passports are separated by blank lines.
#
# Here is an example batch file containing four passports:
#
#   ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
#   byr:1937 iyr:2017 cid:147 hgt:183cm
#
# iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
# hcl:#cfa07d byr:1929
#
#   hcl:#ae17e1 iyr:2013
#   eyr:2024
# ecl:brn pid:760753108 byr:1931
# hgt:179cm
#
# hcl:#cfa07d eyr:2025 pid:166559648
#   iyr:2011 ecl:brn hgt:59in
#
# The first passport is valid - all eight fields are present. The second
# passport is invalid - it is missing hgt (the Height field).
#
# The third passport is interesting; the only missing field is cid, so it looks
# like data from North Pole Credentials, not a passport at all! Surely, nobody
# would mind if you made the system temporarily ignore missing cid fields.
# Treat this "passport" as valid.
#
# The fourth passport is missing two fields, cid and byr. Missing cid is fine,
# but missing any other field is not, so this passport is invalid.
#
# According to the above rules, your improved system would report 2 valid
# passports.
#
# Count the number of valid passports - those that have all required fields.
# Treat cid as optional. In your batch file, how many passports are valid?


# Problem 1 Function ==========================================================


#' Read in the passport data
#'
#' @description passports  span multiple lines and are separated by a line break
#'
#' @param path path to the file
#'
#' @return a data.frame
read_pasports <- function(path){

  text <- readr::read_file(path)

  df <- data.frame(
    text = unlist(strsplit(text, "\r\n\r\n"))
  )

  df$text <- gsub("\r\n", " ",df$text)

  return(df)

}


# Problem 1 Test ==============================================================

data <- read_pasports(path = "input/day_04.txt")

# I think a pipeline is easist here + I get the values
data %>%
  dplyr::mutate(id=row_number()) %>%
  tidyr::separate(text,  sep = " ", into = paste0("col", seq_len(nmax))) %>%
  tidyr::pivot_longer(cols = c(-id)) %>%
  dplyr::select(-name)%>%
  tidyr::separate(value, into=c("type", "value"), sep = ":")%>%
  dplyr::filter(type %in% c("byr","iyr","eyr", "hgt", "hcl", "ecl", "pid")) %>%
  tidyr::pivot_wider(names_from = type, values_from = value) %>%
  tidyr::drop_na() %>%
  nrow()

# Problem 2 ===================================================================

# The line is moving more quickly now, but you overhear airport security
# talking about how passports with invalid data are getting through. Better add
# some data validation, quick!
#
# You can continue to ignore the cid field, but each other field has strict
# rules about what values are valid for automatic validation:
#
#   byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.
# Your job is to count the passports where all required fields are both present
# and valid according to the above rules.


# Problem 2 pipeline ===========================================================

# These could be individual functions but I think in this case this is easier as a 1 off.


nmax <- max(stringr::str_count(bulk_text_df$text, " "))+1
bulk_text_df %>%
  dplyr::mutate(id=row_number()) %>%
  tidyr::separate(text,  sep = " ", into = paste0("col", seq_len(nmax))) %>%
  tidyr::pivot_longer(cols = c(-id)) %>%
  dplyr::select(-name)%>%
  tidyr::drop_na()%>%
  tidyr::separate(value, into=c("type", "value"), sep = ":")%>%
  dplyr::filter(type %in% c("byr","iyr","eyr", "hgt", "hcl", "ecl", "pid")) %>%
  tidyr::pivot_wider(names_from = type, values_from = value) %>%
  na.omit() %>%
  # Set the columns to proper types
  dplyr::mutate(
    byr = as.numeric(byr),
    iyr = as.numeric(iyr),
    eyr = as.numeric(eyr),
    hgt_unit = stringr::str_extract(hgt, "[:alpha:]+"),
    hgt = as.numeric(stringr::str_extract(hgt, "[0-9]+")),
    hcl,
    ecl,
    pid
  ) %>%
  dplyr::mutate(
    hgt_valid = dplyr::case_when(
      hgt_unit == "in" & hgt >= 59 & hgt <= 76 ~ TRUE,
      hgt_unit == "cm" & hgt >= 150 & hgt <= 193 ~ TRUE,
      TRUE ~ FALSE
    ) ,
    hcl_valid = dplyr::if_else(
      stringr::str_detect(hcl, pattern = "^#(?:[0-9a-f-A-F]){6}$"),
      TRUE,
      FALSE)
    ,
    pid_valid = dplyr::if_else(
      stringr::str_detect(pid, pattern = "^[0-9]{9}$"),
      TRUE,
      FALSE
    )
  )%>%
  dplyr::filter(
    byr >= 1920, byr <= 2002,
    iyr >= 2010, iyr <= 2020,
    eyr >= 2020, eyr <= 2030,
    hgt_valid,
    hcl_valid,
    ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
    pid_valid
  ) %>%
  nrow()



