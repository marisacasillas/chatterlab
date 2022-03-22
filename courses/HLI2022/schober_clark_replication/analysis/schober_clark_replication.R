library(tidyverse)
library(phonfieldwork)

eafs <- list.files("eafs/", "*.eaf")
interaction.data <- tibble()
trial.data <- tibble()
card.data <- tibble()

for (eaf in eafs) {
  eaf.data <- eaf_to_df(paste0("eafs/", eaf)) %>%
    separate(source, into = c("student_name", "deleteme"), sep = "\\.") %>%
    select(-tier, -id, -tier_type, -deleteme) %>%
    mutate(
      trial_number = 0,
      card_id = "NA"
    )
  
  trial.intervals <- eaf.data %>%
    filter(tier_name == "trial_number")
  for (i in 1:nrow(trial.intervals)) {
    # imperfect when speech/action goes over the edge of the
    # trial marker
    row.matches <- which(
      eaf.data$time_start <= trial.intervals$time_end[i] &
        eaf.data$time_end >= trial.intervals$time_start[i])
    eaf.data$trial_number[row.matches] <- trial.intervals$content[i]
  }

  card.intervals <- eaf.data %>%
    filter(tier_name == "card_id")
  for (i in 1:nrow(card.intervals)) {
    # imperfect when speech/action goes over the edge of the
    # trial marker
    row.matches <- which(
      eaf.data$time_start <= card.intervals$time_end[i] &
        eaf.data$time_end >= card.intervals$time_start[i])
    eaf.data$card_id[row.matches] <- card.intervals$content[i]
  }
  
  # leave card/trial NA speech as NA
  # assign NA placements to closest card
  NA.placements <- which(eaf.data$card_id == "NA" &
                           grepl("placement", eaf.data$tier_name))
  for (placement in NA.placements) {
    find.min.dist <- card.intervals %>% mutate(
      from_start = abs(eaf.data$time_start[placement] - time_start),
      from_end = abs(time_end - eaf.data$time_start[placement]))
    min.idx <- case_when(
      min(find.min.dist$from_start) < min(find.min.dist$from_end) ~
        which(find.min.dist$from_start == min(find.min.dist$from_start)),
      min(find.min.dist$from_start) > min(find.min.dist$from_end) ~
        which(find.min.dist$from_end == min(find.min.dist$from_end)),
      min(find.min.dist$from_start) == min(find.min.dist$from_end) ~
        which(find.min.dist$from_end == min(find.min.dist$from_end))
    )
    eaf.data$card_id[placement] <- find.min.dist$content[min.idx]
  }
  
  # collect session data
  interaction.data <- bind_rows(interaction.data,
                                filter(eaf.data,
                                       tier_name != "trial_number" &
                                         tier_name != "card_id"))
  trial.data <- bind_rows(trial.data,
                          select(trial.intervals, -trial_number, -card_id))
  card.data <- bind_rows(card.data,
                         select(card.intervals, -trial_number, -card_id))
}

