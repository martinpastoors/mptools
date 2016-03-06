# The crayola function originates from Einar Hjorleifsson
# Converted into a function, by Martin Pastoors
# note: data needs to be in long format: year, age, number
# requires libraries: dplyr, tidyr, RColorBrewer, ggplot2
# input: df (d) and title (t)

crayola <- function(d, t) {

  # to do: check whether the df is in the right format

  # to do: make the function flexible by allowing input of which variable names to use

  # create the colour scheme
  PAIRED <- rep(brewer.pal(12, "Paired"), 100)

  # make sure the data is numbers and add yearclass
  d <-
    d %>%
    mutate(
      age  = as.integer(as.character(age)),
      year = as.integer(as.character(year)),
      yc   = year - age
    )

  # number of yearclasses
  n <- length(unique(d$yc))

  # create and show the plot
  print(
    d %>%
      ggplot(aes(year, number, fill = factor(yc))) +
      theme_bw() +
      geom_hline(yintercept = 1, col = "grey") +
      geom_bar(stat = "identity") +
      expand_limits(x = c(min(d$year), max(d$year)), y = 0) +
      scale_fill_manual(values = PAIRED[1:n]) +
      facet_grid(age ~ ., scale = "free_y", switch = "y") +
      theme(legend.position = "none", axis.text.y = element_blank()) +
      labs(x = NULL, y = NULL, title = t) +
      scale_y_continuous(NULL, NULL)
  )
  return()

}  # end of the function
