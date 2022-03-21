# custom functions for my super special manuscript

# this function returns your input number plus two
add.two <- function(x) {return(x + 2)}

# this function also returns your input number plus two
sdfkjsdf <- function(kgguw) {return(kgguw + 2)}

# this function also returns your input string plus the string " two"
add.two2 <- function(x) {return(paste(as.character(x), "two"))}

# this function looks through a random sequence of numbers (1-10000) to find the
# number you give it.. it is a waste of time :)
is.it.x <- function(x) {
  max <- 10000
  y <- sample(1:max,1)
  nums <- sample(1:max)
  for (i in nums) {
    if (i == x) {
      print(paste("yes, it's", i))
      break()
    } else {
      print(paste("no, it's not", i))
    }
  }
}
