#### modeling ####

# two parts to a model
# 1. define a family of models that express a precise but generic pattern that you want to 
# capture
#    eg a straight line y = a_0 + a_1 * b
# 2. generate a fitted model by finding the model from the family that is closest to ur data
#    e.g. find the parameters that give u the closest fit to ur data - for straight line
#    those that fit a_0 and a_1

library(tidyverse)
library(modelr)

sim1
ggplot(sim1, aes(x, y)) +
  geom_point()
# pretty close to a straight line

y <- a_1 + a_2*x # y = b + mx

# finding values for a_1, a_2 = look thru random values
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

# adding to plot
ggplot(sim1, aes(x, y)) +
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
# just need to specify intercept + slope
  geom_point()
# lot of the randomly gen ones v bad, but some good
# how do we pick out the best ones
# one way - quantify distance bw each parameter and dataset - for each data pt, 
# cal distance between each of the datapts and the fitted model

# write a func
model1 <- function(param, data) {
  param[1] + data$x * param[2]
}

model1(c(7,1.5), sim1) # for each x value in sim 1, calculating 

sim2 <- sim1 %>% 
  mutate(pred = model1(c(7,1.5), sim1))

sim2 # adds predicted y value from the model - predicted 

ggplot(sim2, aes(x, y)) +
  geom_line(aes(x = x, y = pred)) +
  geom_point()
# drawing the line with the predicted y values

# now want to measure how good of a fit this line is to the data
# use root mean squared deviation 
# the 250 lines - its two params are defining it, all the lines from same gen formula
measure_distance <- function(param, data) {
  diff <- data$y - model1(param, data) # actual y - pred y
  sqrt(mean(diff^2)) # sqrt of the mean of all the squared diffs
} # default in functions is to return the last sentence

measure_distance(c(7,1.5), sim1)

models # 250 combinations

# test models combinations against sim1
sim1_dist <- function(a1, a2) {
  # helper function sp. for sim1 - so don't have to put that in the map call
  measure_distance(c(a1,a2), sim1)
}

models <- models %>% 
  mutate(dist = map2_dbl(a1, a2, sim1_dist)) 
# 2 input vectors so use map2

models

# now we have the distance, so we can rank all the models - shortest dist best
models <- models %>% 
  mutate(rank = rank(dist)) %>% 
  arrange(rank)
models


ggplot(sim1, aes(x, y)) +
  geom_point() +
  geom_abline(aes(intercept = a1, slope = a2, color = -dist), data = filter(models, rank<=10)) 
# models w shortest dist get brightest colors

# finding the best out of top 10
ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank <=10), size = 4, color = "red") +
  geom_point(aes(color = -dist))
# best circled in red

# zoom in
grid <- expand.grid(
  a1 = seq(-5,20, length = 25),
  a2 = seq(1,3, length = 25)
) %>% 
  mutate(dist = map2_dbl(a1,a2, sim1_dist))
head(grid)

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <=10), size = 4, color = "red") +
  geom_point(aes(color = -dist))

ggplot(sim1, aes(x, y)) +
  geom_abline(aes(intercept = a1, slope = a2), data = filter(grid, rank(dist) <=10)) +
  # just need to specify intercept + slope
  geom_point()
# much better fit now

# repeating to get even better fit - predef funcs
# minimizing these distances - function called optim
# single best model
best <- optim(c(0,0), measure_distance, data = sim1)
# function requires a starting point - could be anything...same if u start with 10,10
# or if u have local min - might have to try diff values
best$par

ggplot(sim1, aes(x,y)) +
  geom_point() +
  geom_abline(intercept = best$par[1], slope = best$par[2])

# this is a general optimizing func (for any model)
# but r has a specific linear model function also

# linear model so implied that we're looking for a1, a2
sim1_mod <- lm(y ~ x, data = sim1)
sim1_mod
# get coeffs
coef(sim1_mod)
# exactly the same as optim


sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)

sim1alm <- lm(y ~ x, data = sim1a)

#(Intercept)            x  
# 6.051        1.511
# 5.159        1.588
# 5.892        1.528

ggplot(sim1a, aes(x,y)) +
  geom_point() +
  geom_abline(intercept = sim1alm$coefficients[1], slope = sim1alm$coefficients[2])
# can get outliers


# predictions -------------------------------------------------------------

# use model to make predictions about values not included in data set

# grid of evenly spaced x values
grid <- sim1 %>% 
  data_grid(x)

grid <- grid %>% 
  add_predictions(sim1_mod)

grid # we've added predictions 

ggplot(sim1, aes(x = x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, color = "Red")
# using line bc now we have the actual predictions (not abline)  

##### residuals #####
# diff bw observed y and predicted y

sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

# standard way of looking at residuals - frequency polygram to look at spread
ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)

ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

