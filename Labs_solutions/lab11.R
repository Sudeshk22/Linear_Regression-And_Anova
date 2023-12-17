## Question 1
library(ggplot2)
fac <- factor(rep(c(0.25, 0.5,0.75,1),each = 4))
fac
height <- c(1.4, 2.0, 2.3,2.5,7.8,9.2,6.8,6.0,7.6,7.0,7.3,5.5,1.6,3.4,3.0,3.9)
exp <- data.frame(treatment = fac , response = height)
library(ggplot2)
ggplot(exp) + aes(x= treatment, y = response, color = treatment)+ geom_jitter()
mod <- aov(response ~ treatment, data = exp)
summary(mod)



### Question 2
library(palmerpenguins)
library(tidyverse)
data<-penguins[, c("species", "flipper_length_mm")]
summary(data)

ggplot(data) +
  aes(x = species, y = flipper_length_mm, color = species) +
  geom_jitter() +
  theme(legend.position = "none")
res_aov <- aov(flipper_length_mm ~ species,
               data = data)

summary(res_aov)

