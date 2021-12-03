################################################################################
# 2.1

add.two <- function(x) {return(x + 2)}

sdfkjsdf <- function(kgguw) {return(kgguw + 2)}

add.two2 <- function(x) {return(paste(as.character(x), "two"))}

eggs <- TRUE
milk <- TRUE

if (eggs == TRUE) {
  n.milk <- 6
} else {
  n.milk <- 1
}
n.milk <- ifelse(eggs == TRUE, yes = 6, no = 1)

if (milk == TRUE && eggs == TRUE) {
  n.milk <- 6
} else if (milk == FALSE && eggs == TRUE) {
  n.milk <- 0
} else if (milk == TRUE && eggs == EGGS) {
  n.milk <- 1
} else {
  n.milk <- 0
}

# If they have milk, buy one carton of milk.
# If they have eggs, buy six eggs.
if (milk == TRUE) {
  n.milk <- 1
} else {
  n.milk <- 0
}
if (eggs == TRUE) {
  n.eggs <- 6
} else {
  n.eggs <- 0
}

if (milk == TRUE && eggs == TRUE) {
  n.milk <- 6
} else if (milk == TRUE && eggs == FALSE) {
  n.milk <- 1
} else {
  n.milk <- 0
}  

if (milk == TRUE) {
  if (eggs == TRUE) {
    n.milk <- 6
  } else {
    n.milk <- 1
  }
} else {
  n.milk <- 0
}

for (i in c(2,4,8,16)) {print(i)}

for (i in c(1,2,3,4)) {print(2^i)}

i <- 1
while(i <= 4) {
  print(2^i)
  i <- i + 1}


################################################################################
# 2.2

mydata <- read_csv("pilotdata.csv")

mydata <- read_csv("pilotdata.csv",
                   col_types = cols(
                     participantID = col_character()
                   ))


mmdata <- read_excel("MM Data.xlsx", skip = 1)

mmdata.long <- read_excel("MM Data.xlsx", skip = 1) %>%
  pivot_longer(cols = c("Red", "Green", "Blue", "Orange",
                        "Yellow", "Brown"),
               names_to = "Color", values_to = "Number")

write_excel_csv(mmdata.long, "MM_Data-long.xls")


mmdata <- read_csv("MM Data.csv")

mmdata.long <- read_csv("MM Data.csv") %>%
  pivot_longer(cols = c("Red", "Green", "Blue", "Orange",
                        "Yellow", "Brown"),
               names_to = "Color", values_to = "Number")

write_csv(mmdata.long, "MM_Data-long.csv")
