# Make a histogram with shaded areas

library(tidyverse)
library(ggpattern)

# see https://r-graphics.org/recipe-miscgraph-function-shade
dnorm_limit <- function(x) {
    y <- dnorm(x)
    y[x > -2 & x < 2] <- NA
    return(y)
}

# ggplot() with dummy data
p <- ggplot(data.frame(x = c(-3, 3)), aes(x = x))

p +
  stat_function(fun = dnorm_limit, geom = "area", fill = "grey", alpha = 0.8) +
  stat_function(fun = dnorm, lwd = 1.2) +
  theme_classic(base_size = 40) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
    )

ggsave("working/hist.png", dpi = 600)
