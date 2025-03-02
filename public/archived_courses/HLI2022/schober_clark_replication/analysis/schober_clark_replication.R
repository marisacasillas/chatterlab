library(tidyverse)
library(phonfieldwork)

answer.key <- read_csv("key.csv")
answer.placements <- answer.key %>%
  unite(placement_keys,
    c(correct_placed_card, correct_position), sep = "_") %>%
  mutate(
    trial_number = as.character(trial_number)
  )

eafs <- list.files("eafs/", "*.eaf$")
interaction.data <- tibble()
timing.data <- tibble()




for (eaf in eafs) {
  print(eaf)
  eaf.data <- eaf_to_df(paste0("eafs/", eaf)) %>%
    separate(source,
             into = c("1", "2", "3", "student_name", "4"), sep = "[-.]") %>%
    select(-tier, -id, -tier_type, -`1`, -`2`, -`3`, -`4`) %>%
    mutate(
      trial_number = 0,
      card_id = "NA"
    )
  
  # assign annots to trials
  ## fix trial numbers
  trial.intervals <- eaf.data %>%
    filter(tier_name == "trial_number") %>%
    ## fix trial numbers (brittle!)
    mutate(content = str_extract(content, "\\d")) %>%
    mutate(content = case_when(
      content %in% c("1", "3", "6") ~ content,
      TRUE ~ "INCORRECT TRIAL ID"
    ))
  ## find annots placed w/i trial interval
  for (i in 1:nrow(trial.intervals)) {
    row.matches <- which(
      eaf.data$time_start <= trial.intervals$time_end[i] &
        eaf.data$time_end >= trial.intervals$time_start[i])
    eaf.data$trial_number[row.matches] <- trial.intervals$content[i]
  }
  ## for non-assigned annots, put them with the closest trials
  zero.trial.ints <- which(eaf.data$trial_number == 0)
  if (length(zero.trial.ints) >= 1) {
    for (zero in zero.trial.ints) {
      start.search.idx <- findInterval(
        eaf.data$time_start[zero], trial.intervals$time_start)
      start.search <- trial.intervals$time_end[start.search.idx]
      end.search <- trial.intervals$time_start[start.search.idx + 1]
      if (start.search.idx == 0) {
        eaf.data$trial_number[zero] <- "1"
      } else if (start.search.idx == nrow(trial.intervals)) {
        eaf.data$trial_number[zero] <- as.character(
          trial.intervals$content[nrow(trial.intervals)])
      } else {
        eaf.data$trial_number[zero] <- case_when(
          eaf.data$time_start[zero] - start.search <
            end.search - eaf.data$time_start[zero] ~ trial.intervals$content[
              start.search.idx],
          eaf.data$time_start[zero] - start.search >
            end.search - eaf.data$time_start[zero] ~ trial.intervals$content[
              start.search.idx + 1],
          TRUE ~ "OOPS"
        )
      }
  }
  }

  card.intervals <- eaf.data %>%
    filter(tier_name == "card_id") %>%
    ## fix card letters (brittle!)
    mutate(content = str_to_upper(content)) %>%
    mutate(content = str_extract(content, "[A-P]")) %>%
    mutate(content = case_when(
      content %in% c("A", "B", "C", "D", "E", "F",
                     "G", "H", "I", "J", "K", "L",
                     "M", "N", "O", "P") ~ content,
      TRUE ~ "INCORRECT CARD ID"
    ))    

  ## classify speech w/i intervals
  ## include speech w/ partial interval overlap
  for (i in 1:nrow(card.intervals)) {
    row.matches <- which(
      eaf.data$time_start <= card.intervals$time_end[i] &
        eaf.data$time_end >= card.intervals$time_start[i] &
        grepl("speech", eaf.data$tier_name))
    eaf.data$card_id[row.matches] <- card.intervals$content[i]
  }
  ## assign other value to all other speech
  eaf.data$card_id[which(grepl("speech", eaf.data$tier_name) &
                           eaf.data$card_id == "NA")] <- "outside of card segment"
  
  ## classify completion points and placements
  ### completion points
  completion.intervals <- eaf.data %>%
    filter(tier_name == "completion_point")
  ### use the card ID whose end point is the closest to the compl. pt's start
  for (i in 1:nrow(completion.intervals)) {
    eaf.data$card_id[
      which(eaf.data$time_start == completion.intervals$time_start[i] &
              eaf.data$tier_name == "completion_point")] <-
    card.intervals$content[which.min(
      abs(card.intervals$time_end - completion.intervals$time_start[i]))]
  }
    
  ### placements
  placement.intervals <- eaf.data %>%
    filter(grepl("placement", tier_name)) %>%
    ## fix card letters and numbers (brittle!)
    mutate(content = str_to_upper(content)) %>%
    mutate(content = str_extract(content, "[A-P]\\_\\d+"),
           placement_phrase = content) %>%
    separate(content, into = c("placed_card", "position"), sep = "[_]") %>%
    select(tier_name, placement_phrase, placed_card, position, trial_number)
  eaf.data$content[which(grepl("placement", eaf.data$tier_name))] <-
    str_to_upper(eaf.data$content[which(grepl("placement",
                                                eaf.data$tier_name))])
  eaf.data <- eaf.data %>%
    left_join(placement.intervals, by = c("tier_name", "trial_number",
                                          "content" = "placement_phrase"))
  eaf.data$card_id[which(!is.na(eaf.data$placed_card))] <- eaf.data$placed_card[
    which(!is.na(eaf.data$placed_card))]

  
  ### get placement timings
  placement.intervals.linked <- eaf.data %>%
    filter(grepl("placement", tier_name)) %>%
    ## fix card letters and numbers (brittle!)
    mutate(content = str_to_upper(content)) %>%
    mutate(content = str_extract(content, "[A-P]\\_\\d+"),
           placement_phrase = content) %>%
    separate(content, into = c("placed_card", "position"), sep = "[_]") %>%
    select(trial_number, tier_name, time_start, card_id, placed_card) %>%
    rename("time_start_plc" = time_start)
  completion.intervals.tolink <- eaf.data %>%
    filter(tier_name == "completion_point") %>%
    select(trial_number, time_start, card_id) %>%
    rename("time_start_cmpt" = time_start)
  placement.intervals.linked2 <- placement.intervals.linked %>%
    full_join(completion.intervals.tolink,
              by = c("trial_number", "card_id")) %>%
  # we're missing some because of misassignments of the completion points to card ID
  # by complement we're also doubling up on some... so let's get rid of this stuff
    filter(!is.na(time_start_plc) & !is.na(time_start_cmpt)) %>%
    mutate(time_diff = time_start_plc - time_start_cmpt) %>%
    group_by(trial_number, tier_name, card_id) %>%
    summarize(time_diff_sec = min(time_diff, na.rm = TRUE)) %>%
    mutate(student_name = eaf.data$student_name[1])

  # bind to big data table
  interaction.data <- bind_rows(interaction.data, eaf.data)
  timing.data <- bind_rows(timing.data, placement.intervals.linked2)
}

# calculate number of words
interaction.data$n_words <- str_count(interaction.data$content, "\\S+")


##########

# length of director speech trials 1, 3, and 6 in words
trial.lengths.bystudent <- interaction.data %>%
  filter(tier_name == "director_speech") %>%
  group_by(student_name, trial_number) %>%
  summarize(
    total.words = sum(n_words)/12
  )

trial.lengths.overall <- trial.lengths.bystudent %>%
  group_by(trial_number) %>%
  summarize(
    mean.len = mean(total.words),
    median.len = median(total.words),
    sd.len = sd(total.words),
    min.len = min(total.words),
    max.len = max(total.words)
  ) %>%
  mutate(
    upper = mean.len + sd.len,
    lower = mean.len - sd.len
  )

ggplot(trial.lengths.overall, aes(x = trial_number, y = mean.len, group = 1)) +
  geom_text(data = trial.lengths.bystudent, aes(x = trial_number,
                                                   y = total.words,
                                                   group =student_name,
                                                   color = student_name,
                                                   label = student_name),
              alpha = 0.5) +
  geom_line(data = trial.lengths.bystudent, aes(x = trial_number,
                                                  y = total.words,
                                                  group =student_name,
                                                  color = student_name),
              alpha = 0.5) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  geom_line(lwd = 1) +
  theme_minimal() +
  labs(x = "Trial number", y = "Director speech length (words)") +
  guides(color = "none")



# duration of director speech trials 1, 3, and 6 in seconds
trial.durations.bystudent <- interaction.data %>%
  mutate(duration = time_end - time_start) %>%
  filter(tier_name == "director_speech") %>%
  group_by(student_name, trial_number) %>%
  summarize(
    total.dur = sum(duration)
  )

trial.durations.overall <- trial.durations.bystudent %>%
  group_by(trial_number) %>%
  summarize(
    mean.dur = mean(total.dur),
    median.dur = median(total.dur),
    sd.dur = sd(total.dur),
    min.dur = min(total.dur),
    max.dur = max(total.dur)
  ) %>%
  mutate(
    upper = mean.dur + sd.dur,
    lower = mean.dur - sd.dur
  )

ggplot(trial.durations.overall, aes(x = trial_number, y = mean.dur, group = 1)) +
  geom_text(data = trial.durations.bystudent, aes(x = trial_number,
                                                  y = total.dur,
                                                  group =student_name,
                                                  color = student_name,
                                                  label = student_name),
            alpha = 0.5) +
  geom_line(data = trial.durations.bystudent, aes(x = trial_number,
                                                  y = total.dur,
                                                  group =student_name,
                                                  color = student_name),
            alpha = 0.5) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  geom_line(lwd = 1) +
  theme_minimal() +
  labs(x = "Trial number", y = "Director speech duration (sec)") +
  guides(color = "none")






# accuracy
interaction.data.accuracy <- tibble()

for (student in unique(interaction.data$student_name)) {
  st.data <- interaction.data %>%
    filter(student_name == student & grepl("placement", tier_name))
  for (ptcp in unique(st.data$tier_name)) {
    ptcp.data <- st.data %>%
      filter(tier_name == ptcp)
    for (trial in unique(ptcp.data$trial_number)) {
      placements <- ptcp.data %>%
        filter(trial_number == trial) %>%
        pull(content)
      trial.answer.placements <- answer.placements %>%
        filter(trial_number == trial) %>%
        pull(placement_keys)
      correct.placements <- length(which(unique(
        placements) %in% trial.answer.placements))
      interaction.data.accuracy <- bind_rows(
        interaction.data.accuracy,
        tibble(
          student_name = student,
          tier_name = ptcp,
          trial_number = trial,
          n_correct = correct.placements
        )
      )
    }
  }
}

# super specific to this course -- not generalizable,
# but bc suspicious data quality
interaction.data.accuracy <- interaction.data.accuracy %>%
  filter(!(student_name %in% c("MatthewJPark", "DamonHuff", "JonathanXu")))


accuracy.bystudent <- interaction.data.accuracy %>%
  mutate(
    accuracy.prop = n_correct/12,
    placer = ifelse(tier_name == "matcher_placement_id", "Matcher", "Overhearer")
  )

accuracy.overall <- accuracy.bystudent %>%
  group_by(trial_number, placer) %>%
  summarize(
    mean.acc = mean(accuracy.prop),
    median.acc = median(accuracy.prop),
    sd.acc = sd(accuracy.prop),
    min.acc = min(accuracy.prop),
    max.acc = max(accuracy.prop)
  ) %>%
  mutate(
    upper = mean.acc + sd.acc,
    lower =  mean.acc - sd.acc
  )

ggplot(accuracy.overall, aes(x = trial_number, y = mean.acc,
                             group = placer, linetype = placer, color = placer)) +
  coord_cartesian(ylim = c(0, 1)) +
  # ylim(0,1) +
  geom_jitter(data = accuracy.bystudent, aes(
    x = trial_number, y = accuracy.prop, color = placer), alpha = 0.3) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_color_manual(values = c("springgreen4", "chocolate2")) +
  geom_line() +
  theme_minimal() +
  labs(x = "Trial number", y = "Accuracy",
       linetype = "Participant", color = "Participant")
  




# placement timing
timing.data <-timing.data %>%
  mutate(
    placer = ifelse(tier_name == "matcher_placement_id", "Matcher", "Overhearer")
  )

timing.data.bystudent <- timing.data %>%
  group_by(student_name, trial_number, placer) %>%
  summarize(
    mean.time_diff = mean(time_diff_sec)
  )

timing.data.overall <- timing.data.bystudent %>%
  group_by(trial_number, placer) %>%
  summarize(
    mean.td = mean(mean.time_diff),
    median.td = median(mean.time_diff),
    sd.td = sd(mean.time_diff),
    min.td = min(mean.time_diff),
    max.td = max(mean.time_diff)
  )

ggplot(timing.data, aes(x = time_diff_sec, linetype = placer)) +
  geom_density() +
  theme_minimal() +
  labs(x = "Placement timing (sec)", y = "Density",
       linetype = "Participant") +
  xlim(-30, 30)

ggplot(timing.data,
       aes(x = time_diff_sec, linetype = placer, color = trial_number)) +
  geom_density() +
  theme_minimal() +
  facet_grid(~ placer) +
  scale_color_manual(values = c("gray70", "gray50", "black")) +
  labs(x = "Placement timing (sec)", y = "Density",
       color = "Trial", linetype = "Participant") +
  guides(color = "none", linetype = "none") +
  xlim(-30, 30)



## Other stuff
# - time from start of 1 to end of 6 (range; averages)
# x pacts for figures by trial 6
pacts.to.manually.sort <- interaction.data %>%
  filter(tier_name == "director_speech" & trial_number == "6") %>%
  select(student_name, card_id, time_start, content)
write_csv(pacts.to.manually.sort, "pacts.to.manually.sort.csv")

# x completion language (to be manually edited for to 10)
completion.phrases <- interaction.data %>%
  filter(tier_name == "completion_point") %>%
  group_by(content) %>%
  summarize(n = n()) %>%
  arrange(-n)
write_csv(completion.phrases, "completion.phrases.csv")

# x MD & OH or just MD?
n.ptcps <- interaction.data %>%
  select(tier_name, student_name) %>%
  filter(grepl("placement_id", tier_name)) %>%
  group_by(student_name) %>%
  distinct() %>%
  summarize(n = n())

# x missing placements (ignorant of accuracy; completeness of placement)
n.placements.bystudent <- interaction.data %>%
  select(trial_number, tier_name, student_name, content) %>%
  filter(grepl("placement_id", tier_name)) %>%
  distinct() %>%
  group_by(trial_number, tier_name, student_name) %>%
  summarize(n = n()) %>%
  mutate(diff.placements = n - 12,
    extra.placements = ifelse(diff.placements < 0, 1, 0),
    incomplete.placements = ifelse(diff.placements > 0, 1, 0))
  
n.placements.overall <- n.placements.bystudent %>%
  group_by(trial_number, tier_name) %>%
  summarize(
    mean.n.placements = mean(n),
    median.n.placements = median(n),
    min.n.placements = min(n),
    max.n.placements = max(n),
    prop.w.extra = mean(extra.placements),
    prop.w.missing = mean(incomplete.placements)
  ) %>%
  mutate(
    participant = ifelse(tier_name == "matcher_placement_id", "Matcher", "Overhearer")
  ) %>%
  select(participant, trial_number, mean.n.placements, median.n.placements,
         min.n.placements, max.n.placements, prop.w.extra, prop.w.missing)
  
write_csv(n.placements.overall, "n.placements.overall.csv")

ggplot(n.placements.overall, aes(x = trial_number,
                         y = prop.w.extra, fill = participant)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  ylim(0, 0.25) +
  scale_fill_manual(values = c("springgreen4", "chocolate2")) +
  labs(x = "Trial number", y = "Proportion w/ >12 placements",
       fill = "Participant")

ggplot(n.placements.overall, aes(x = trial_number,
                                 y = prop.w.missing, fill = participant)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  ylim(0, 0.25) +
  scale_fill_manual(values = c("springgreen4", "chocolate2")) +
  labs(x = "Trial number", y = "Proportion w/ <12 placements",
       fill = "Participant")
