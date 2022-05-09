library(tidyverse)
library(phonfieldwork)

card.names <- c("A", "B", "C", "D", "E", "F", "G", "H",
                "I", "J", "K", "L", "M", "N", "O", "P")

check.sc89.eaf <- function(eaf_file, eaf_file_name) {
  
  eaf.data <- eaf_to_df(eaf_file) %>%
    # check for valid filename
    mutate(
      source = as.character(eaf_file_name),
      valid.source = grepl(
        "^schober-clark-template-[A-Za-z]+[-AL]*\\.eaf$", source),
      student_name = gsub(
        "^schober-clark-template-([A-Za-z]+)[-AL]*\\.eaf$", "\\1", source
      )
      #  valid.source = grepl("^[A-Za-z]+-[0-9]{8}\\.eaf$", source),
    ) #%>%
    # separate(source,
    #          into = c("student_name", "student_id", "file_ext"), sep = "[-.]") %>%
    # select(-file_ext)
  
  # check for annotations on all the tiers but overhearer_speech
  tier.names <- unique(eaf.data$tier_name)
  trial.tier <- "trial_number" %in% unique(eaf.data$tier_name)
  card.tier <- "card_id" %in% unique(eaf.data$tier_name)
  matcher.speech <- "matcher_speech" %in% unique(eaf.data$tier_name)
  director.speech <- "director_speech" %in% unique(eaf.data$tier_name)
  overhearer.speech <- "overhearer_speech" %in% unique(eaf.data$tier_name)
  completion.point <- "completion_point" %in% unique(eaf.data$tier_name)
  matcher.placement <- "matcher_placement_id" %in% unique(eaf.data$tier_name)
  overhearer.placement <- "overhearer_placement_id" %in% unique(eaf.data$tier_name)
  
  # check for the validity of placement_ids
  placement_ids <- eaf.data %>%
    filter(grepl("placement", tier_name)) %>%
    mutate(
      valid.content = grepl("^[ABCDEFGHIJKLMNOP]\\_\\d{1,2}$", content)
    ) %>%
    separate(content, into = c("card.name", "index"), sep = "[_]") %>%
    mutate(
      valid.pid.name = card.name %in% card.names,
      valid.pid.index = as.numeric(index) <= 12
    )
  
  critical.tier.msg <- case_when(
    (trial.tier + card.tier +
       matcher.speech + director.speech +
       completion.point + matcher.placement +
       overhearer.placement) == 6 ~ "TRUE - Director and Matcher ONLY (no Overhearer yet)",
    (trial.tier + card.tier +
       matcher.speech + director.speech +
       completion.point + matcher.placement +
       overhearer.placement) == 7 ~ "TRUE - Director, Matcher, and Overhearer",
    TRUE ~ "Missing annotations on trial, card, speech, placement, or completion point tiers."
  )
  
  # return check outcomes
  # return(eaf_file_name)
  return(list(
    `valid filename structure` = eaf.data$valid.source[1],
    `detected student name` = eaf.data$student_name[1],
    # `detected student id number` = eaf.data$student_id[1],
    `annotations on all critical tiers` = critical.tier.msg,
    `valid placement id labels` = !(FALSE %in% placement_ids$valid.pid.name),
    `valid placement id numbers` = !(FALSE %in% placement_ids$valid.pid.index)
  ))
}


