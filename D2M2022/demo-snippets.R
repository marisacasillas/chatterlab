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


################################################################################
# 3.1

testdata <- tibble(
    condition = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    participant = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6),
    score = runif(12),
    notes = c("glasses_none", "glasses_none", "none_none", "none_none",
              "none_incomplete", "none_incomplete", "none_none", "none_none",
              "glasses_none", "glasses_none", "glasses_late", "glasses_late")
)

testdata %>%
    group_by(condition, participant) %>%
    summarize(mean.ptcp.score = mean(score)) %>%
    group_by(condition) %>%
    summarize(mean.cond.score = mean(mean.ptcp.score),
              sem.cond.score = sd(mean.ptcp.score)/sqrt(n()),
              n = n(),
              sd = sd(mean.ptcp.score))

mmdata <- read_csv("MM Data.csv")

mmdata.long <- read_csv("MM Data.csv") %>%
  pivot_longer(cols = c("Red", "Green", "Blue", "Orange",
                        "Yellow", "Brown"),
               names_to = "Color", values_to = "Number")

mmdata.wide <- mmdata.long %>%
  pivot_wider(names_from = "Color",
              values_from = "Number") %>%
  relocate(Weight,.after = last_col())

testdata.sep <- testdata %>%
  separate(notes, c("vision_correction", "other_notes"), sep = "_")

testdata.unite <- testdata.sep %>%
  unite("semicolon_notes", "vision_correction":"other_notes", sep = ";")


missingdata <- tibble(
  condition = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
  participant = c(1, NA, NA, NA, 2, NA, NA, NA, 3, NA, NA),
  trial = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 3, 4),
  score = c(0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1),
)

fixeddata <- missingdata %>%
  fill(participant) %>%
  complete(condition, trial) %>%
  replace_na(list(participant = 3)) %>% # a HACK (will not generalize/will break)
  arrange(condition, participant, trial) # just to make it pretty

all.ptcp.trial.combos <- crossing(
  participant = unique(missingdata$participant),
  trial = c(1:4)) %>%
  filter(!is.na(participant))

fixeddata2 <- missingdata %>%
  fill(participant) %>%
  full_join(all.ptcp.trial.combos) %>%
  # arrange(participant, trial) %>% # superstitious but harmless
  fill(condition)
