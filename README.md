---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Survey Chapter 2
```{r}
library(survey)
data(api)
head(apisrs)
srs_design = svydesign(id= ~1, data = apisrs)
# The total weighted by the survey design and the weights for each school.  So each person is multipled by the same weight N/n we want big N over little n, because we want to scale up and since little n will be less than big N it will go into big N many more times telling us how many times we should mutliple the values by.
svytotal(~enroll, srs_design)
# The survey mean weighted by the probability of being selected in the sample which is just the sample n over the population N.
svymean(~enroll, srs_design)

# When you do not have the total sample size is not known you need to make up the sample weight and put it in to some variable variable that the data gets mutiplied by

nofpc = svydesign(id = ~1, weights = ~pw, data = apisrs)
svytotal(~enroll, nofpc)
svymean(~enroll, nofpc)
# Can be done with categorical values as well.  Gives the proportions or population counts for each category
svytotal(~stype, srs_design)
svymean(~stype, srs_design)

# Stratfied means less variation, because you are ensuring a certain set of certain populations.  It is just several simple random samples from different strata. Also need fpc here, because that has the known actual weights which allows you weight the data up to those actual values.  So your total will be the exact total for each of the groups since it is known.

strat_design = svydesign(id=~1, strata = ~stype, fpc =~ fpc, data = apistrat)
svytotal(~enroll, strat_design)
svymean(~enroll, strat_design)
svymean(~enroll, srs_design)
svytotal(~stype, strat_design)


emerg_high = subset(strat_design, emer > 20)
svymean(~api00, emerg_high)
```
How to allocate how much of the population should be for each sample in reducing variance is the goal.  Sometimes oversampling an underepresented group is the priroty so this may not be helpful.: https://rpubs.com/trjohns/allocate

Another good example: https://www.rdocumentation.org/packages/PracTools/versions/0.6/topics/strAlloc
```{r}
allocate = function (Ni, si, ci = rep(1, length(Ni)), c0 = 0, ct = NA, vt = NA) 
{
    f <- Ni * si/sqrt(ci)/sum(Ni * si/sqrt(ci))
    N <- sum(Ni)
    if (!is.na(ct) & !is.na(vt)) {
        stop("both survey cost and variance cannot be fixed")
    }
    else if (is.na(ct) & is.na(vt)) {
        return(list(fractions = f, ni = NA, variance = NA, cost = NA))
    }
    else {
        t1 <- sum(Ni * si/sqrt(ci))
        t2 <- sum(Ni * si * sqrt(ci))
        if (!is.na(vt)) {
            n <- t1 * t2/(vt * N^2 + sum(Ni * si^2))
            ni <- n * f
            if (any(ni > Ni)) 
                warning("optimum sample size exceeds available units")
            return(list(fractions = f, ni = n * f, n = n, variance = ifelse(all(ni <= 
                Ni), sum(Ni^2 * (Ni - ni)/Ni * (si^2/ni))/N^2, 
                NA), cost = ifelse(all(ni <= Ni), c0 + sum(ni * 
                ci), NA)))
        }
        if (!is.na(ct)) {
            n <- (ct - c0) * t1/t2
            ni <- n * f
            if (any(ni > Ni)) 
                warning("optimum sample size exceeds available units")
            return(list(fractions = f, ni = n * f, n = n, variance = ifelse(all(ni <= 
                Ni), sum(Ni^2 * (Ni - ni)/Ni * (si^2/ni))/N^2, 
                NA), cost = ifelse(all(ni <= Ni), c0 + sum(ni * 
                ci), NA)))
        }
    }
}
# Ni are the sample sizes, si are the standard deviations, ci are the costs per person to collect the data
allocate(Ni = c(155,62,93), si = c(5,15,10), ci = c(9,9,16))



```
Post strata.  ID is from some cluster and strata design with two FPCs.  Then we get the actual population values for the type that we are interested in. And set the stype that we want to post strata by
```{r}
data(api)
clusDe = svydesign(id=~dnum+snum, fpc = ~ fpc1+fpc2, data = apiclus2)
popTypes = data.frame(stype = c("E", "H", "M"), Freq = c(4421, 755, 1018))
psDesign = postStratify(clusDe, strata = ~ stype, population = popTypes)
svymean(~api00, psDesign)
```
Post strat with raking with more than two variables.
```{r}
popCTBAND = data.frame(CTBAND = 1:9, Freq = c(round(rnorm(9,5000, 1000),0)))
popTenure = data.frame(TENURE = 1:4, Freq = c(round(rnorm(4,5000, 1000),0)))
# Need to create some type of design with the appropraite weights
# Try doing with two variables it is the same as with one, 
frsRaked = rake(frs.des, sample = list(~CTBAND, TENURE))
```
Post strata with regression.  Each time you are calculating the slope you are times it by the person's weight, which is the post strata which big N over little n gives the ratio of how many more times people like that person should be in the sample.  This is an example with two post stratas. 
```{r}
set.seed(12345)
preYear = c(0:100)
preYear = sample(preYear, 100, replace = TRUE)

income = c(0:100000)
income = sample(income, 100, replace = TRUE)

gender = c("Male", "Female")
gender = sample(gender, 100, replace = TRUE)
gender = as.numeric(factor(gender))

ethnicity = c("White", "African_American", "Mixed_Ethnicity", "Other_Ethnicity")
ethnicity = sample(ethnicity, 100, replace = TRUE)
ethnicity = as.numeric(factor(ethnicity))

postYear = preYear + 10

data = cbind(preYear, income, gender, ethnicity, postYear)
dat = as.data.frame(data)
dat

datSvyUnweighted = svydesign(ids=~1, data = dat)

genderDist = data.frame(gender = c("1", "2"), Freq = nrow(dat)*c(.45, .55))

ethDist = data.frame(ethnicity = c("1", "2", "3", "4"), Freq = nrow(dat)*c(.25, .25, .25, .25))

datSvyRake = rake(design = datSvyUnweighted, sample.margins = list(~gender, ~ethnicity), population.margins = list(genderDist, ethDist))

datSvyRakeTrim = trimWeights(datSvyRake, lower =.3, upper = 3, strict = TRUE)
svymean(~income, datSvyRakeTrim)

datReg = svyglm(income ~ gender + ethnicity +preYear, datSvyRakeTrim)
summary(datReg)

```

