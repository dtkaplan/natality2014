# reading from the CDC data and selecting just a few columns
# Source: http://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Births
# http://www.nber.org/natality/2014/natl2014.dta.zip

require(dplyr)

# wic, ld_ndl IS NOT WORKING CORRECTLY

Keep_vars_data <- '
name,recode
mager,na99
fagecomb,na99
ubfacil,recode_ubfacil
urf_diab,one_yes
urf_chyper,one_yes
urf_phyper,one_yes
urf_eclam,one_yes
uop_induc,one_yes
uld_breech,one_yes
ilive,na_u
ab_aven1,na_u
ab_aven6,na_u
ab_nicu,na_u
ab_surf,na_u
ab_anti,na_u
ab_seiz,na_u
dbwt,na9999
combgest,na99
sex,I
dplural,I
apgar5,recode_apgar
apgar10,recode_apgar
mtran,na_u
pay,recode_pay
mm_mtr,na_u
mm_plac,na_u
mm_rupt,na_u
mm_uhyst,na_u
mm_aicu,na_u
me_pres,recode_me_pres
me_rout,recode_me_rout
ld_indl,na_u
pwgt_r,na999
dwgt_r,na999
m_ht_in,na99
cig_0,na99
cig_1,na99
cig_2,na99
cig_3,na99
wic,na_u
precare,recode_precare
previs,na99
priorlive,na99
priordead,na99
priorterm,na99
'

one_yes <- function(x) { x <- na9(x); ifelse(x == 1, 'y', 'n') }
na_u <- function(x) ifelse(x == "U" | x == "", NA, tolower(x))
na8 <- function(x) ifelse(x == 8, NA, x)
na88 <- function(x) ifelse(x == 88, NA, x)
na9 <- function(x) ifelse(x == 9, NA, x)
na99 <- function(x) ifelse(x == 99, NA, x)
na999 <- function(x) ifelse(x == 999, NA, x)
na9999 <- function(x) ifelse(x == 9999, NA, x)
recode_me_pres <- function(x) {
  meanings <- c("cephalic", "breech", "other", NA, NA, NA, NA, NA, NA)
  meanings[as.integer(x)]
  # 9 -> NA fetal presentation RECODE
  # 1 = cephalic, 2 = breech, 3 = other

}
recode_precare <- function(x) {
  ifelse(x == 0, 15, na99(x))
}
recode_me_rout <- function(x) {
  meanings <- c("spontaneous", "forceps", "vacuum", "cesarean", NA, NA, NA, NA, NA)
  # 1 = spontaneous, 2 = forceps, 3 = vacuum, 4 = cesarean
  meanings[as.integer(x)]
}
recode_pay <- function(x) {
  meanings <- c("medicaid", "private", "self-pay", "IHS", "military", "other_gov", NA, "other", NA)
  meanings[x]
  # 9 -> NA, 1 = medicaid, 2 = private, 3 = self-pay
  # 4 = Indian Health Service, 5 = CHAMPUS/TRICARE
  # 6 = Other government, 8 = other
}
recode_apgar <- function(x) na88(na99(x))
recode_ubfacil <- function(x) {
  meanings <- c("hosp", "center", "clinic", "residence", "other", NA, NA, NA, NA)
  meanings[x]
}


Births_raw <- data.table::fread("~/Downloads/natl2014.csv", nrows = -1)
Keep <- read.csv(textConnection(Keep_vars_data), stringsAsFactors = FALSE)

Subset_of_keepers <- subset(Births_raw, select = Keep$name)

Natality_2014 <- NULL
for (k in 1:ncol(Subset_of_keepers)) {
  input <- Subset_of_keepers[[k]]
  Natality_2014[[Keep$name[k]]] <- do.call(Keep$recode[k], list(x = input))
}

Natality_2014 <- data.frame(Natality_2014, stringsAsFactors = FALSE)

save(Natality_2014, file = "inst/Natality_2014.rda")
set.seed(101)
Natality_2014       %>% sample_n(size = 1000000) -> Natality_2014_1000k
Natality_2014_1000k %>% sample_n(size = 100000)  -> Natality_2014_100k
Natality_2014_100k  %>% sample_n(size = 10000)   -> Natality_2014_10k
Natality_2014_10k   %>% sample_n(size = 1000)    -> Natality_2014_1k
save(Natality_2014_1000k, file = "inst/Natality_2014_1000k.rda")
save(Natality_2014_100k, file = "data/Natality_2014_100k.rda")
save(Natality_2014_10k, file = "data/Natality_2014_10k.rda")
save(Natality_2014_1k, file = "data/Natality_2014_1k.rda")



* Describe how women reduce smoking during pregnancy.
* Is there a relationship between mother's height and weight and birth outcomes?
* Was there weight gain? Relationship to outcome?
* Who were the people whose weight and other simple measures were not recorded? Differences in prenatal care?
* Is there a link between who pays (`pay`) and prenatal care?
* Gestation period for multiple births. Prenatal care for multiple births.
* DATA MEANING QUESTION: Are the cases mothers or babies --- try to find some multiple
births with the same mother age, height and weight.
* Are `urf_diab` and `urf_chyper` risk factors?
* Is there a "normal" number of prenatal care visits (`previs`)
* Does prenatal care depend on the number of previous births/pregnancies?
* Some babies had died by the time their birth was registered. Does this correspond to risk factors?
* DATA MEANING QUESTION: Are the missing entries in `ilive` possibly dead? How does `NA` in `ilive` correspond to APGAR
* Describe the relationship between birth weight (`dbwt`) and gestation (`combgest`). Make an approprate
graphic and give the answer to this question: How does the typical birth weight change per week of
gestation at 30 weeks? At 40 weeks? (should have a smoother) Other questions: What patterns does an `lm()` model capture
and what patterns does it fail to capture?
* Cause and effect. Build this model: `mod1 <- rpart(apgar5 ~ ., data = Natality_2014_100k)` Interpret the relationships
it represents and give an explanation of what causes what. Do it again with `apgar5 ~ . - apgar10`
* Significance versus substantiality: Compare p-values for the whole data to the p-values for 100k samples. Indeed,
compare the p-values for the different sample sizes. Example: Compare `apgar5 ~ urf_diab` and `apgar5 ~ urf_phyper`
* Fail to reject ... Construct the model `apgar5 ~ pay` for the different sample sizes. At the largest
sample sizes, what's the conclusion? IHS and self-pay are different from all the rest. Is this mean
difference substantial? How about the percentage below 8 and below 5.


## Statistics

* Ratio of male to female births. Estimate from different sample sizes. How many would you need to sample to see that it's uneven? Distinguish between
a 95% confidence interval on the "ratio" to an interval of the prob. of being female. What methods
would you use.
* Ratio of male to female births by age.
* Ratio of male to female births by parity.
* Is induction of childbirth related to gestation time. `combgest` and `ld_indl` or `me_rout`
* Meaning of this table on assisted ventilation: `with(Births_raw, table(ab_aven1, ab_aven6))`
* Model baby weight. e.g. `mod <- lm(dbwt ~ combgest * pwgt_r * dwgt_r, data = Natality_2014_100k)`
