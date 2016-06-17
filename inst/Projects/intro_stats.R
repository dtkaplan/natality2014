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

## ---- message=FALSE, warning=FALSE---------------------------------------
gf_density( ~ combgest + fill:"blue" + alpha:0.5, data = Natality_2014_1k) +
  geom_vline(aes(xintercept = 35, color = "red")) +
  geom_vline(aes(xintercept = 41, color = "red")) +
  ggtitle("Defining normal") +
  xlab("Gestation (weeks)") +
  guides(color = FALSE)

## ------------------------------------------------------------------------
Natality_2014_100k %>%
  filter(mager == 28, pwgt_r > 120, pwgt_r < 130, sex == "F") %>%
  qdata( ~ combgest, data = ., p = c(.05, .95))

## ------------------------------------------------------------------------
gest_mod_1 <- lm(combgest ~ mager + pwgt_r + sex, data = Natality_2014_1k)
me <- list(mager = 28, sex = "F", pwgt_r = 125)
evaluate_model(gest_mod_1, at = me)

## ------------------------------------------------------------------------
Preds <- evaluate_model(gest_mod_1, data = Natality_2014_1k)
qdata( ~ I(combgest - model_output), data = Preds, p = c(.05, .95))

## ------------------------------------------------------------------------
effect_size(gest_mod_1, ~ mager, at = me)
effect_size(gest_mod_1, ~ pwgt_r, at = me)
effect_size(gest_mod_1, ~ sex, at = me)

## ------------------------------------------------------------------------
BE <- ensemble(gest_mod_1, nreps = 100)
effect_size(BE, ~ mager, at = me ) %>%
  ggplot(aes(x = slope)) + geom_density() + ggtitle("Age and gestation")
effect_size(BE, ~ pwgt_r, at = me ) %>%
  ggplot(aes(x = slope)) + geom_density() + ggtitle("Weight and gestation")
effect_size(BE, ~ sex, at = me ) %>%
  ggplot(aes(x = change)) + geom_density() + ggtitle("Baby's sex and gestation")

## ------------------------------------------------------------------------
tally( ~ uop_induc, data = Natality_2014_1k)

## ------------------------------------------------------------------------
gf_histogram( ~ combgest + fill:uop_induc + alpha:0.7 + position:"dodge", 
              data = Natality_2014_1k) + 
  scale_fill_discrete(guide_legend(title = "Labor induced")) + 
  xlab("Gestation") 

## ------------------------------------------------------------------------
gf_histogram( ~ combgest + fill:uop_induc + alpha:0.7 + position:"stack", 
              data = Natality_2014_1k) +
  scale_fill_discrete(guide_legend(title = "Labor induced")) + 
  xlab("Gestation") 


## ----message=FALSE, warning = FALSE--------------------------------------
gf_histogram( ~ combgest + fill:uop_induc + alpha:0.7 + position:"fill", 
              data = Natality_2014_1k) + 
  scale_fill_discrete(guide_legend(title = "Labor induced")) + 
  xlab("Gestation") 

## ------------------------------------------------------------------------
apgar_to_score <- function(apgar) {
  (apgar >= 4) + (apgar >= 6) + (apgar >= 8)
}

## ------------------------------------------------------------------------
nicu_to_score <- function(nicu) {
  nicu == "n"
}
weight_to_score <- function(weight) {
  (weight > 2700) + (weight > 2300) + (weight > 2000) + (weight > 1500)
}

## ------------------------------------------------------------------------
For_analysis <-
  Natality_2014_100k %>%
  mutate(outcome_score = 2 + apgar_to_score(apgar5) + 
           weight_to_score(dbwt) + nicu_to_score(ab_nicu))
gf_counts( ~ outcome_score, data = For_analysis)
tally( ~ outcome_score <= 7, data = For_analysis)

## ------------------------------------------------------------------------
ggplot(For_analysis, aes(x = precare, fill = outcome_score > 9)) +
  geom_histogram(position = "fill")

## ------------------------------------------------------------------------
tally(outcome_score >= 9 ~ precare, data = For_analysis, format = "proportion")


## ------------------------------------------------------------------------
mod_1 <- lm(outcome_score ~ previs * precare, dat = For_analysis)
summary(mod_1)

## ------------------------------------------------------------------------
library(rpart)
library(rpart.plot)
mod_2 <- rpart(outcome_score ~ previs + precare + priorlive + mager, 
               data = For_analysis, cp = 0.001)
prp(mod_2, type = 3)
fmodel(mod_2, ~ previs + precare)

## ------------------------------------------------------------------------
For_analysis <- Natality_2014_100k %>% mutate(male = 0 + (sex == "M"))
mod_0 <- glm(male ~ 1, data = For_analysis, family = "binomial")
mod_1 <- glm(male ~ mager, data = For_analysis, family = "binomial")
mod_2 <- glm(male ~ mager + pwgt_r + urf_diab + fagecomb, 
             data = For_analysis, family = "binomial")
mod_3 <- glm(male ~ mager > 35, data = For_analysis)

## ------------------------------------------------------------------------
ggplot(Natality_2014_100k, aes(x = combgest, fill = ilive)) + geom_bar()
ggplot(Natality_2014_100k, aes(x = combgest, fill = ilive)) + geom_bar(position = "fill")

## ------------------------------------------------------------------------
ggplot(Natality_2014_100k, aes(x = ilive, fill = as.factor(combgest))) + geom_bar(position = "fill")

## ------------------------------------------------------------------------
mod_1 <- glm(ilive == "y" ~ combgest * mager, 
           data = Natality_2014_100k,
           family = "binomial")
fmodel(mod_1, ~ combgest + mager)
library(splines)
mod_2 <- glm(ilive == "y" ~ ns(combgest, 2) * mager,
             data = Natality_2014_100k,
             family = "binomial")
fmodel(mod_2, ~ combgest + mager) 

## ------------------------------------------------------------------------
tally(ilive ~ combgest, data = Natality_2014_100k, format = "proportion", useNA = "no")

