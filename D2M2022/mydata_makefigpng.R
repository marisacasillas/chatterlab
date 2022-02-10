(ggplot(mydata, aes(x = mpg)) +
  geom_histogram() +
  theme_apa()) %>%
  ggsave(filename = "figures/mpg-histogram.png",
         device = "png", width = 10, units = "in")
