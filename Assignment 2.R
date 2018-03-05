## load tidyverse
library(tidyverse)

## set the amount of numbers it will randomly generate
set.seed = 10000

## create function that creates a dataset based on given parameters for n, b0, b1 and sd.err
rand.mod <- function(n = 200, b0 = 2, b1 = 3, sd.err = 1) {
  predictor <- runif(n) - 0.5
  response <- b0 + b1 * predictor + rnorm(n, mean = 0, sd = sd.err)
  return(data.frame(response = response, predictor = predictor))}

## asks you to run the model 1000 times, adds the new data it generated into the new data set called est
## n.times = number of times you run each model(?) ..
## line 16 -> creating a brand new dataset with nothing in it because we're going to add the other models into that dataset
## ncol = 3 because we're running 3 models. 
## mod1 linear models from random data function (created earlier) .. called it tval.B1 because we're manipulating B1 only (arbitrary numbers)
## [i, 1] adds the created data to col 1, [i, 2] adds that created data to col 2, etc. 
## ["predictor", "t value"] line just gives you the summary for the t value of the predictor (so you're not going through a whole table)
## line 31 just puts it all into a dataframe
n.times <- 1000
tval.B1 <- matrix(nrow = n.times, ncol = 3, NA)
for (i in 1:n.times) {
  mod1 <- lm(response ~ predictor, data = rand.mod(n = 100, b0 = 1, b1 = 0.5, sd.err = 2))
  tval.B1[i, 1] <- summary(mod1)$coefficients["predictor", "t value"]
  mod2 <- lm(response ~ predictor, data = rand.mod(n = 100, b0 = 1, b1 = 2, sd.err = 2))
  tval.B1[i, 2] <- summary(mod2)$coefficients["predictor", "t value"]
  mod3 <- lm(response ~ predictor, data = rand.mod(n = 100, b0 = 1, b1 = 3, sd.err = 2))
  tval.B1[i, 3] <- summary(mod3)$coefficients["predictor", "t value"]
  }
tval.B1 <- data.frame(tval.B1)

## split tval.B1 into a data frame in long format (instead of having 3 cols (X1, X2 and X2), you instead have "trial" column that includes X1, X2, X3. The other column is the corresponding tvalues)
## changes data from two columns to one 
tval.B1 <- gather(tval.B1, value = "tval", key="trial")

## double checking that "trial" column are factors and not intergers 
tval.B1$trial <- factor(tval.B1$trial)

## getting mean lines for the plot (see plot)
tval.sum.B1 <- group_by(tval.B1, trial) %>% summarize(mn.tval = mean(tval))

## plotting 
p <- ggplot(aes(tval, group = trial, col = trial), data = tval.B1)
p + geom_density() + geom_vline(data = tval.sum.B1, aes(xintercept = mn.tval, col = trial))


# exactly the same, except instead of manipulating B1, we're manipulating error. CHANGE ALL est.err to tval.err
n.times <- 1000
tval.err <- matrix(nrow = n.times, ncol = 3, NA)
for (i in 1:n.times) {
  mod1 <- lm(response ~ predictor, data = rand.mod(n = 100, b0 = 1, b1 = 1, sd.err = 0.5))
  tval.err[i, 1] <- summary(mod1)$coefficients["predictor", "t value"]
  mod2 <- lm(response ~ predictor, data = rand.mod(n = 100, b0 = 1, b1 = 1, sd.err = 1))
  tval.err[i, 2] <- summary(mod2)$coefficients["predictor", "t value"]
  mod3 <- lm(response ~ predictor, data = rand.mod(n = 100, b0 = 1, b1 = 1, sd.err = 2))
  tval.err[i, 3] <- summary(mod3)$coefficients["predictor", "t value"]
}
tval.err <- data.frame(tval.err)

## split tval.err into a data frame in long format
#changes data from two columns to one 
tval.err <- gather(tval.err, value = "tvalue", key="trial")
tval.err$trial <- factor(tval.err$trial)

tval.sum.err <- group_by(tval.err, trial) %>% summarize(mn.tval = mean(tvalue))


p <- ggplot(aes(tvalue, group = trial, col = trial), data = tval.err)
p + geom_density() + geom_vline(data = tval.sum.err, aes(xintercept = mn.tval, col = trial))

## manipulate sample size -- switch est.ss to tval.ss
n.times <- 1000
tval.ss <- matrix(nrow = n.times, ncol = 3, NA)
for (i in 1:n.times) {
  mod1 <- lm(response ~ predictor, data = rand.mod(n = 100, b0 = 1, b1 = 1, sd.err = 1))
  tval.ss[i, 1] <- summary(mod1)$coefficients["predictor", "t value"]
  mod2 <- lm(response ~ predictor, data = rand.mod(n = 1000, b0 = 1, b1 = 1, sd.err = 1))
  tval.ss[i, 2] <- summary(mod2)$coefficients["predictor", "t value"]
  mod3 <- lm(response ~ predictor, data = rand.mod(n = 10000, b0 = 1, b1 = 1, sd.err = 1))
  tval.ss[i, 3] <- summary(mod3)$coefficients["predictor", "t value"]
}
tval.ss <- data.frame(tval.ss)

## split est.df into a data frame in long format
#changes data from two columns to one 
tval.ss <- gather(tval.ss, value = "tvalue", key="trial")
tval.ss$trial <- factor(tval.ss$trial)

tval.sum.ss <- group_by(tval.ss, trial) %>% summarize(mn.tval = mean(tvalue))

p <- ggplot(aes(tvalue, group = trial, col = trial), data = tval.ss)
p + geom_density() + geom_vline(data = tval.sum.ss, aes(xintercept = mn.tval, col = trial))


##sample size effect on p-value  ... Identical code but changed n, b0 and b1 to what Phil told us in question 4 .. and we're now looking at the pvalue (pr(>|t|) instead of tvalue) 
n.times <- 1000
pval.ss <- matrix(nrow = n.times, ncol = 2, NA)
for (i in 1:n.times) {
  mod <- lm(response ~ predictor, data = rand.mod(n = 100, b0 = 0, b1 = 0, sd.err = 3))
  pval.ss[i, 1] <- summary(mod)$coefficients["predictor", "Pr(>|t|)"]
  mod1 <- lm(response ~ predictor, data = rand.mod(n = 10, b0 = 0, b1 = 0, sd.err = 3))
  pval.ss[i, 2] <- summary(mod1)$coefficients["predictor", "Pr(>|t|)"]
  }
pval.ss <- data.frame(pval.ss)

## split est.df into a data frame in long format
#changes data from two columns to one 
pval.ss <- gather(pval.ss, value = "pvalue", key="trial")
pval.ss$trial <- factor(pval.ss$trial)

pval.sum.ss <- group_by(pval.ss, trial) %>% summarize(mn.pval = mean(pvalue))

p <- ggplot(aes(pvalue, group = trial, col = trial), data = pval.ss)
p + geom_density() + geom_vline(data = pval.sum.ss, aes(xintercept = mn.pval, col = trial))
