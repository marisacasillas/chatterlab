mydata.prepped <- mydata %>%
  mutate(
    mpg.x.cyl = mpg * cyl,
    wt.sqrd = wt^2
  ) %>%
  separate(model, c("brand", "model"),
           sep = " ", extra = "merge", fill = "right")
