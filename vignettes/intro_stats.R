## ----include = FALSE-----------------------------------------------------
library(natality2014)
library(statisticalModeling)
library(mosaic)

## ------------------------------------------------------------------------
tally( ~ combgest == 36, data = Natality_2014_1k)
tally( ~ combgest == 36, data = Natality_2014_1k, format = "proportion")

## ------------------------------------------------------------------------
tally( ~ combgest %in% c(35, 36, 37), data = Natality_2014_1k, format = "proportion")
tally( ~ combgest %in% c(34, 35, 36, 37, 38), data = Natality_2014_1k, format = "proportion")

## ------------------------------------------------------------------------
tally( ~ combgest, data = Natality_2014_1k)

## ------------------------------------------------------------------------
tally( ~ combgest %in% c(37, 38, 39, 40, 41), data = Natality_2014_1k, format = "proportion")

## ------------------------------------------------------------------------
qdata( ~ combgest, data = Natality_2014_1k, p = c(.10, .90))

## ------------------------------------------------------------------------
qdata( ~ combgest, data = Natality_2014_1k, p = c(.05, .95))
qdata( ~ combgest, data = Natality_2014_1k, p = c(.025, .975))    

## ------------------------------------------------------------------------
gf_density( ~ combgest + fill:"blue" + alpha:0.5, data = Natality_2014_1k) +
  geom_vline(aes(xintercept = 35, color = "red")) +
  geom_vline(aes(xintercept = 41, color = "red")) +
  ggtitle("Defining normal") +
  xlab("Gestation (weeks)") +
  guides(color = FALSE)

