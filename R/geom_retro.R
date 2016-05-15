# =====================================================
# geom_retro (function)
# 
# Einar Hjorleifsson, 5/4/2016
# =====================================================

geom_retro <- function(x) {
  
  # dataset with last year
  x.last <- x %>% filter(AssYear == max(AssYear))
  
  # dataset from other years
  x <- x %>% filter(AssYear != max(AssYear))
  
  # plotting
  p <- 
    ggplot(x, aes(Year,value, group=AssYear)) +
    theme_bw() +
    geom_line() +
    geom_line(data = x.last, col = "red", lwd = 1) +
    facet_grid(variable ~ ., scales = "free", switch = "y") +
    expand_limits(y = 0) +
    theme(legend.title = element_blank()) +
    labs(x = NULL, y = NULL)
  
  return(p)
}
