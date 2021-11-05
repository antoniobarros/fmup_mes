#
#

library(tidyverse)
library(ggstatsplot)
library(viridis)
#1.DINOSAURS ----

db <- read.csv("data/DatasaurusDozen.tsv", sep = "\t") %>% as_tibble()

# plot
db %>% 
    ggplot() + 
    aes(x = x, y = y) +
    geom_point() +
    facet_wrap(~dataset)

ggsave("images/figures.png")

#1.1 statistics (mean)
db %>% 
    group_by(dataset) %>% 
    summarise(mean_x = mean(x),
              mean_y = mean(y))

#1.2 std
db %>% 
    group_by(dataset) %>% 
    summarise(sd_x = sd(x),
              sd_y = sd(y))


#1.3. correlation
db %>% 
    group_by(dataset) %>% 
    summarise(cor = cor(x,y))


##

df <- rnorm(n = 10000, mean = 7, sd = 2.0) %>% as_tibble()


q025 <- quantile(df$value, 0.025)

ggplot(df) +
    aes(x = value) +
    geom_density() +
    geom_vline(xintercept = q025)
    


###