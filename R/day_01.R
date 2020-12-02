# Problem =====================================================================

# After saving Christmas five years in a row, you've decided to take a vacation
# at a nice resort on a tropical island. Surely, Christmas will go on without you.
#
# The tropical island has its own currency and is entirely cash-only. The
# gold coins used there have a little picture of a starfish; the locals just
# call them stars. None of the currency exchanges seem to have heard of them,
# but somehow, you'll need to find fifty of these coins by the time you arrive
# so you can pay the deposit on your room.
#
# To save your vacation, you need to get all fifty stars by December 25th.
#
# Collect stars by solving puzzles. Two puzzles will be made available on each
# day in the Advent calendar; the second puzzle is unlocked when you complete
# the first. Each puzzle grants one star. Good luck!
#
# Before you leave, the Elves in accounting just need you to fix your expense
# report (your puzzle input); apparently, something isn't quite adding up.
#
# Specifically, they need you to find the two entries that sum to 2020 and then
# multiply those two numbers together.
#
# For example, suppose your expense report contained the following:
#
# 1721
# 979
# 366
# 299
# 675
# 1456
# In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying
# them together produces 1721 * 299 = 514579, so the correct answer is 514579.
#
# Of course, your expense report is much larger. Find the two entries that sum
# to 2020; what do you get if you multiply them together?


# Solution Functions===========================================================

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

    if((sum-nums[i]) %in% nums[-i]){
      n1 <- nums[i]
      n2 <- sum-nums[i]
      break()
    }
    else{
      n1 <- NA
      n2 <- NA
    }
  }


  return(c(n1, n2))
}



#' Find the product of 2 numbers in a list that sum to another number
#' @param nums A list of numbers to check
#' @param sum number to sum to
#'
#' @return A number
get_product_of_components <- function(nums, sum){
  components <- find_sum_components(nums, sum)
  if(is.na(components[1])){usethis::ui_stop("No component pairs found")}
  product <- components[1]*components[2]
  return(product)
}


# Solution Work ===============================================================


product_desired <- 2020

nums_to_check <- c(1438, 781, 1917, 1371, 1336, 1802, 1566, 1878, 737, 1998,
                   1488, 1372, 1715, 1585, 1676, 1810, 1692, 1329,
                   1916, 1854,  1307, 1347, 1445, 1475, 1435, 1270, 1949, 1957,
                   1602, 1931, 1505, 1636, 1539, 1803, 1011, 1821, 1021, 1461,
                   1755, 1332, 1576, 1923, 1899, 1574, 1641, 1357, 1509, 1877,
                   1875, 1228, 1725, 1808, 1678, 1789, 1719, 1691, 1434, 1538,
                   2002, 1569, 1403, 1146, 1623, 1328, 1876, 520, 1930, 1633,
                   1990, 1330, 1402, 1880, 1984, 1938, 1898, 1908, 1661, 1335,
                   1424, 1833, 1731, 1568, 1659, 1554, 1323, 1344, 1999, 1716,
                   1851, 1313, 1531, 190, 1834, 1592, 1890, 1649, 1430, 1599,
                   869, 1460, 1009, 1771, 1818, 1853, 1544, 1279, 1997, 1896,
                   1272, 1772, 1375, 1373, 1689, 1249, 1840, 1528, 1749, 1364,
                   1670, 1361, 1408, 1828, 1864, 1826, 1499, 1507, 336, 1532,
                   1349, 1519, 1437, 1720, 1817, 1920, 1388, 1288, 1290, 1823,
                   1690, 1331, 1564, 1660, 1598, 1479, 1673, 1553, 1991, 66,
                   1571, 1453, 1398, 1814, 1679, 1652, 1687, 1951, 1334, 1319,
                   1605, 1757, 1517, 1724, 2008, 1601, 1909, 1286, 1780, 1901,
                   1961, 1798, 1628, 1831, 1277, 1297, 1744, 1946, 1407, 1856,
                   1922, 1476, 1836, 1240, 1591, 1572, 2000, 1813, 1695, 1723,
                   1238, 1588, 1881, 1850, 1298, 1411, 1496, 744, 1477, 1459,
                   1333, 1902)
answer <- get_product_of_components(nums_to_check, product_desired)



# Problem Twist ===============================================================
# The Elves in accounting are thankful for your help; one of them even offers
# you a starfish coin they had left over from a past vacation. They offer you a
# second one if you can find three numbers in your expense report that meet the
# same criteria.
#
# Using the above example again, the three entries that sum to 2020 are 979,
# 366, and 675. Multiplying them together produces the answer, 241861950.
#
# In your expense report, what is the product of the three entries that sum to
# 2020?


# Twist Solution ==============================================================


#' A lazy method that's almost recursive
#'
#' @param nums a list of numbers to search
#' @param sum number to sum to
#'
#' @return a list with either a tuple of numbers or NA
find_3_sum_components <- function(nums, sum){


  for(i in 1:length(nums)){
    new_sum  <-  sum - nums[i]
    comp_2 <- find_sum_components(nums[-i], new_sum)
    if(!is.na(comp_2[1])){

      n1 <- nums[i]
      n2 <- comp_2[1]
      n3 <- comp_2[2]
      break()
    }else{
      n1 <- NA
      n2 <- NA
      n3 <- NA
    }


  }

  return(c(n1,n2,n3))


}


prod(find_3_sum_components(nums_to_check, 2020))
