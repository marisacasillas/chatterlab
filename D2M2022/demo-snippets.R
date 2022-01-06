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

################################################################################
# 4.2

## 1994 census data 50K income split
library(tidyverse)
adultdata <- read_csv("adult.data.csv")

ggplot(adultdata, aes(age)) + geom_density()
ggplot(adultdata, aes(age)) + geom_histogram()
ggplot(adultdata, aes(age)) + geom_histogram(binwidth = 5)

ggplot(adultdata, aes(`education-num`)) + geom_density()
ggplot(adultdata, aes(`education-num`)) + geom_density(adjust = 2) 

ggplot(adultdata, aes(race)) + geom_density()

ggplot(adultdata, aes(race)) + geom_bar()
ggplot(adultdata, aes(race)) + geom_histogram()
ggplot(adultdata, aes(race)) + geom_histogram(stat = "count")

ggplot(adultdata, aes(occupation)) + geom_bar()
ggplot(filter(adultdata, occupation != "?"), aes(occupation)) +
  geom_bar()
aduldata.occupations.noUNK <- adultdata %>%
  filter(occupation != "?")
ggplot(aduldata.occupations.noUNK, aes(occupation)) + geom_bar()

### recreate these plots
ggplot(adultdata) + geom_histogram(aes(`capital-gain`), binwidth = 5000)
ggplot(adultdata) + geom_density(aes(y = `hours-per-week`))

################################################################################
# 5.1
library(tidyverse)

## One variable
ggplot(iris, aes(x = Sepal.Length)) +
  geom_density()
## Two variables
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()
## Three variables
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                 color = Species)) +
  geom_point()
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                 shape = Species)) +
  geom_point()
## Four variables
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
                 size = Petal.Length)) +
  geom_point() +
  facet_grid(~ Species)


adultdata <- read_csv("adult.data.csv")

ggplot(adultdata, aes(age, `education-num`)) + geom_point()
ggplot(adultdata, aes(age, `education-num`)) + geom_jitter()
ggplot(adultdata, aes(age, `education-num`)) + geom_smooth()
ggplot(adultdata, aes(age, `education-num`)) +
  geom_jitter() +
  geom_smooth()
ggplot(adultdata, aes(age, `education-num`)) +
  geom_smooth() +
  geom_jitter()

# ggplot(adultdata, aes(age, `capital-gain`)) + geom_point()
# ggplot(adultdata, aes(age, `capital-gain`)) + geom_smooth()
# ggplot(adultdata, aes(age, `capital-gain`)) +
#   geom_point() +
#   geom_smooth()
# ggplot(filter(adultdata, `capital-gain` < 99999),
#        aes(age, `capital-gain`)) +
#   geom_point() +
#   geom_smooth()

ggplot(adultdata, aes(age, `education-num`)) + geom_boxplot()
ggplot(adultdata, aes(age, `education-num`, group = age)) +
  geom_violin()
ggplot(adultdata, aes(age, `education-num`, group = age)) +
  geom_jitter() +
  geom_boxplot()
ggplot(adultdata, aes(age, `education-num`, group = age)) +
  geom_jitter(color = "gray50", alpha = 0.1) +
  geom_boxplot()

ggplot(adultdata, aes(education, workclass)) +
  geom_count()
ggplot(adultdata, aes(education, workclass, fill = age)) +
  geom_raster()

## A few things to do for each variable you plan to use in your
## plotting/analysis

### Check for NAs
which(is.na(adultdata$education))
which(is.na(adultdata$`education-num`))
which(is.na(adultdata$age))
for(i in 1:ncol(adultdata)) {print(which(is.na(adultdata[,i])))}

### Look at the data -- NAs are encoded as '?'
View(adultdata)

### Check that the scale, level, and type fits your expectations
### for each variable of interest
head(adultdata)
table(adultdata$income)
table(adultdata$education)
summary(adultdata$age)
ggplot(adultdata, aes(age)) + geom_density()
ggplot(adultdata, aes(`capital-gain`)) + geom_density()
ggplot(adultdata, aes(age, `capital-gain`)) + geom_jitter()
ggplot(adultdata, aes(age, `capital-loss`)) + geom_jitter()

# let's make some data prep changes to make our plots easier to interpret
adultdata <- adultdata %>%
  mutate(
    age.bins = case_when(
      age <= 35 ~ "<= 35",
      age > 35 & age <= 50 ~ "36-50",
      age > 50 & age <= 65 ~ "51-65",
      age > 65 ~ "66+",
      TRUE ~ "uh oh"
    ),
    education.bins = case_when(
      education %in% c("Preschool", "1st-4th",
                       "5th-6th", "7th-8th", "9th", "10th", "11th") ~ 'pre-HS',
      education %in% c("12th", "HS-grad", "Some-college") ~ 'HS',
      education %in% c("Assoc-acdm", "Assoc-voc", "Prof-school") ~ 'Assoc',
      education %in% c("Bachelors", "Masters", "Doctorate") ~ 'BA+',
      TRUE ~ "uh oh"
    ),
    education.bins = fct_relevel(
      education.bins, c('pre-HS', 'HS', 'Assoc', 'BA+')),
    income = case_when(
      grepl("<", income) ~ "<=50K",
      grepl(">", income) ~ ">50K",
      TRUE ~ "uh oh"
    ),
    race = as_factor(race),
    race = factor(race, labels = c("White", "Black", "AsiPacIsl",
                                   "AmerIndFirsNat", "Other")))

ggplot(adultdata, aes(age.bins, `education-num`)) +
  geom_jitter() +
  geom_boxplot()
ggplot(adultdata, aes(age.bins, `education-num`)) +
  geom_jitter(color = "gray50", alpha = 0.1) +
  geom_boxplot(outlier.shape = NA, alpha = 0.0)

ggplot(adultdata, aes(education.bins, age)) +
  geom_jitter(color = "gray50", alpha = 0.1) +
  geom_boxplot(outlier.shape = NA, alpha = 0.0)
ggplot(adultdata, aes(education.bins, age)) +
  geom_jitter(color = "gray50", alpha = 0.1) +
  geom_boxplot(outlier.shape = NA, alpha = 0.0) +
  facet_grid(. ~ race)
ggplot(adultdata, aes(education.bins, age, color = race, fill = race)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(outlier.shape = NA, alpha = 0.0) +
  facet_grid(. ~ race)
ggplot(adultdata, aes(education.bins, age, color = race, fill = race)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(outlier.shape = NA, alpha = 0.0, color = "black") +
  facet_grid(. ~ race)

# now lets explore how income relates to education, race, and sex
ggplot(adultdata, aes(education.bins, fill = income)) +
  geom_bar(position = "fill")
ggplot(adultdata, aes(age.bins, fill = income)) +
  geom_bar(position = "fill")
ggplot(adultdata, aes(sex, fill = income)) +
  geom_bar(position = "fill")

ggplot(adultdata, aes(race, fill = income)) +
  geom_bar(position = "fill") +
  facet_grid(. ~ education.bins) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(adultdata, aes(race, fill = income)) +
  geom_bar(position = "fill") +
  facet_grid(education.bins ~ sex) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## M & M data
mmdata <- read_csv("MM Data.csv")
mmdata.long <- read_csv("MM Data.csv") %>%
  pivot_longer(cols = c("Red", "Green", "Blue", "Orange",
                        "Yellow", "Brown"),
               names_to = "Color", values_to = "Number")

ggplot(mmdata.long, aes(Color, Number, fill = Color)) +
  geom_boxplot()
ggplot(mmdata.long, aes(Color, Number)) +
  geom_dotplot(binaxis = "y")
ggplot(mmdata.long, aes(Color, Number, fill = Color)) +
  geom_boxplot() +
  geom_jitter()
ggplot(mmdata.long, aes(Color, Number, fill = Color)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y")

################################################################################
# 5.2
library(tidyverse)
mmdata <- read_csv("MM Data.csv")
mmdata.long <- read_csv("MM Data.csv") %>%
  pivot_longer(cols = c("Red", "Green", "Blue", "Orange",
                        "Yellow", "Brown"),
               names_to = "Color", values_to = "Number")

ggplot(mmdata.long, aes(Color, Number)) +
  geom_jitter()
ggplot(mmdata.long, aes(Color, Number, color = Color)) +
  geom_jitter()
ggplot(mmdata.long, aes(Color, Number, alpha = Color)) +
  geom_jitter()
ggplot(mmdata.long, aes(Color, Number, shape = Color)) +
  geom_jitter()
ggplot(mmdata.long, aes(Color, Number, size = Color)) +
  geom_jitter()
ggplot(mmdata.long, aes(Color, Number, fill = Color)) +
  geom_jitter()
ggplot(mmdata.long, aes(Color, Number, fill = Color)) +
  geom_jitter(shape = 24)
ggplot(mmdata.long, aes(Color, Number, fill = Color, stroke = Number)) +
  geom_jitter(shape = 24)


ggplot(mpg, aes(x = cyl, y = hwy,
                fill = class, color = class)) +
  geom_jitter(shape = 24) +
  geom_smooth(method = "lm")
ggplot(mpg, aes(x = cyl, y = hwy,
           fill = class)) +
  geom_jitter(shape = 24) +
  geom_smooth(aes(color = class), method = "lm")

ggplot(mpg, aes(x = cyl, y = hwy, fill = class, color = class)) +
  geom_jitter(shape = 24) +
  geom_smooth(method = "lm")
ggplot(mpg, aes(x = cyl, y = hwy,
           fill = class)) +
  geom_jitter(shape = 24) +
  geom_smooth(aes(color = class), method = "lm")

ggplot(mpg, aes(x = cyl, y = hwy)) +
  geom_jitter(aes(color = manufacturer), shape = 24) +
  geom_smooth(aes(color = class), method = "lm")
ggplot(mpg, aes(x = cyl, y = hwy, fill = class, color = class)) +
  geom_jitter(shape = 24) +
  geom_smooth(data = filter(mpg, class != "2seater"),
              aes(x = cyl, y = hwy), method = "lm")

library(jtools)


## BLABLA for later

ggplot(mmdata.long, aes(Color, Number, color = Color, fill = Color)) +
  scale_color_manual(values = c(
    "blue", "brown", "green", "orange", "red", "yellow")) +
  scale_fill_manual(values = c(
    "blue", "brown", "green", "orange", "red", "yellow")) +
  geom_violin(alpha = 0.5) +
  geom_jitter() +
  geom_boxplot(alpha = 0, color = "black", width = 0.5, show.legend = F) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,5)) +
  guides(fill = "none") +
  theme_apa()

mmdata.long.by.bag <- mmdata.long %>%
  group_by(Bag, Weight) %>%
  summarize(
    total.mms = sum(Number)
  )
ggplot(mmdata.long.by.bag, aes(Weight, total.mms, label = Bag)) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "Weight (oz)", y = "# M&M candies") +
  scale_x_continuous(limits = c(45,55), breaks = 45:55) +
  scale_y_continuous(limits = c(40,70), breaks = seq(40,70,10)) +
  geom_text(vjust = 0, nudge_y = 0.5) +
  annotate(
    "text", label = "HELLO!!",
    x = 53, y = 45, size = 8, colour = "red"
  ) +
  theme_apa()


