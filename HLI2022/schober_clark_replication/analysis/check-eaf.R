library(tidyverse)
library(phonfieldwork)

eafs <- list.files("eafs/", "*.eaf")
interaction.data <- tibble()
trial.data <- tibble()
card.data <- tibble()

card.names <- c("monk", "skater", "tower")

for (eaf in eafs) {
  eaf.data <- eaf_to_df(paste0("eafs/", eaf)) %>%
    # check for valid filename
    mutate(
      valid.source = grepl("^[a-z]+-[0-9]{8}$", source),
      ) %>%
    separate(source,
             into = c("name", "student_id", "file_ext"), sep = "[-.]") %>%
    select(-file_ext)
  
  # check for annotations on all the tiers but overhearer_speech
  tier.names <- unique(eaf.data$tier_name)
  trial.tier <- "trial_number" %in% unique(eaf.data$tier_name)
  card.tier <- "card_id" %in% unique(eaf.data$tier_name)
  matcher.speech <- "matcher_speech" %in% unique(eaf.data$tier_name)
  director.speech <- "director_speech" %in% unique(eaf.data$tier_name)
  completion.point <- "completion_point" %in% unique(eaf.data$tier_name)
  matcher.placement <- "matcher_placement_id" %in% unique(eaf.data$tier_name)
  overhearer.placement <- "overhearer_placement_id" %in% unique(eaf.data$tier_name)
  
  # check for the validity of placement_ids
  placement_ids <- eaf.data %>%
    filter(grepl("placement", tier_name)) %>%
    mutate(
      valid.content = grepl("^[a-z]+\\_\\d{1,2}$", content)
    ) %>%
    separate(content, into = c("card.name", "index"), sep = "[_]") %>%
    mutate(
      valid.pid.name = card.name %in% card.names,
      valid.pid.index = as.numeric(index) < 12
    )

  # print output
  output = tibble(
    `valid filename.structure` = eaf.data$valid.source[1],
    `detected student name` = eaf.data$name[1],
    `detected student id number` = eaf.data$student_id[1],
    `annotations on all critical tiers` = (trial.tier + card.tier +
      matcher.speech + director.speech + completion.point + 
      matcher.placement + overhearer.placement) == 7,
    `valid placement id labels` = !(FALSE %in% placement_ids$valid.pid.name),
    `valid placement id numbers` = !(FALSE %in% placement_ids$valid.pid.index),
  )
  
}

