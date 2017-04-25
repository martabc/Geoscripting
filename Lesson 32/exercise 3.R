## Author: Marta Blanco
## Date: April 24, 2017

## FUNCTION TO CHECK WHETHER A YEAR IS LEAP OR NOT 
is.leap <- function(year){
  
  numCheck <- function(x) {   ## store whether input value is numeric
    is.numeric(x)
  }
  
  if (numCheck(year) == FALSE){   ## check that input value is numeric, by testing against variable "numCheck"
    stop('Sorry, you must input a numeric year')
  }
  
  recentYear <- year >= 1900 & year <= 2200   ## input value must also fall between 1900 and 2200 (incuding them)
  leap <- (year %% 4 == 0 & year %% 100 != 0 | year %% 400 == 0)   ## leap must be divisible by 4, 
                                                                   ## possibly by 400, but not by 100
  
  if (recentYear == FALSE) {   ## main function conditionals
      stop('Please input a year from 1900 to 2200')    ## if input isn't between the given years, return error
    } else if (leap == FALSE) {    ## if the leap year doesn't meet the main conditions, return error
      return('Common')   ## otherwise it may be a commmon year
    } else {
      return('LEAP YEAR!')   ## but if all conditions are met, it is a leap. YAY!
    }
}

## Ideally, a year (numeric) falling between 1900 and 2200 should be inserted, to check for leap vs. common year 
is.leap(100)  ## run function with given input (anything can go inside; errors should populate as needed)
