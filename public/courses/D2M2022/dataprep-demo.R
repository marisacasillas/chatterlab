adultdata <- read_csv("datasets/adult.data.csv")

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
# ggplot(adultdata, aes(age)) + geom_density()
# ggplot(adultdata, aes(`capital-gain`)) + geom_density()
# ggplot(adultdata, aes(age, `capital-gain`)) + geom_jitter()
# ggplot(adultdata, aes(age, `capital-loss`)) + geom_jitter()

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