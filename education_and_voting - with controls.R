### Merging in new controls for UKIP vote share ###
### MP 27/05/15 ###

setwd("~/GitHub/election2015")

library(openxlsx)
library(reshape)
library(pastecs)
library(ggplot2)
library(dplyr)

# load the dataset
df <- read.xlsx(xlsxFile = "election_education_v3.xlsx")
str(df$party)

# UKIP dataset ------------------------------------------

ukip <- df[df$party == "UKIP", ] # keep only UKIP candidates
names(ukip)

# read in then merge qualifications, employment and ethnicity vars -----------------------------
quals <- read.xlsx(xlsxFile = "qualifications_constituency.xlsx", sheet = 2, startRow = 1, skipEmptyRows = TRUE)
names(quals)

emp <- read.xlsx(xlsxFile = "unemployment_constituency.xlsx", sheet = 3, startRow = 1, skipEmptyRows = TRUE)
names(emp)

eth <- read.xlsx(xlsxFile = "ethnicity_constituency.xlsx", sheet = 1, startRow = 1, skipEmptyRows = TRUE)
names(eth)
library(reshape)
eth <- reshape::rename(eth, c(allethnicities = "all"))
names(eth)

age <- read.xlsx(xlsxFile = "age_constituency.xlsx", sheet = 2, startRow = 1, skipEmptyRows = TRUE)
names(age)


combo <- merge(ukip, quals, by = "constituency_renamed")
combo <- merge(combo, emp, by = "constituency_renamed")
combo <- merge(combo, eth, by = "constituency_renamed")


combo$percwb <- (combo$whitebritish / combo$all) * 100 # calculate the percentage of white british people in each constituency

combo$percem <- 100 - combo$percwb

names(combo)

# merge in the new deprivation values
dep <- read.xlsx(xlsxFile = "deprivation_constituency.xlsx", sheet = 3, startRow = 1, skipEmptyRows = TRUE)
names(dep)
dep2 <- dep[,c("PCON11NM", "Deprivation")]
dep3 <- dep2[complete.cases(dep2),]
dep3 <- dep3[order(dep3$PCON11NM),] # there are multiple dep measures in each constituency, so need to take the mean and keep just one row for each constituency

# get help: http://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
library(dplyr)
dep4 <- dep3 %>% group_by(PCON11NM) %>% mutate(depmean = mean(Deprivation))
dep5 <- dep4[, c(1, 3)] # remove the deprivation column, which is what is varying
names(dep5)
dep6 <- dplyr::distinct(dep5) # get rid of duplications, boom, 533 rows

combo2 <- merge(combo, dep6, by.y = "PCON11NM", by.x = "constituency_renamed", all.x = T, all.y = F)
names(combo2)
combo2 <- merge(combo2, age, by = "constituency_renamed")

keepvars <- c(1,3:7,14:16,20,58:60,64:71,73:77,79:84)
tidy <- combo2[, keepvars]
names(tidy)

# run the models ----------------------------------------------------------

library(psych)
attach(tidy)
describe(acem_avg)
describe(share)
describe(unemployed_pc_all)
describe(noquals)
describe(unemployed_pc_16_24)
describe(percwb)
describe(percem)


m1 <- lm(share ~ acem_avg, data = tidy)
summary(m1)

m2 <- lm(share ~ acem_avg + noquals, data = tidy)
summary(m2)

m3 <- lm(share ~ acem_avg + noquals + unemployed_pc_all,  data = tidy)
summary(m3)

m4 <- lm(share ~ acem_avg + noquals + unemployed_pc_all + percem,  data = tidy )
summary(m4)

m5 <- lm(share ~ acem_avg + noquals + unemployed_pc_16_24 + percem,  data = tidy )
summary(m5)

m6 <- update(m4, . ~ . + depmean)
summary(m6)

# compare the models
anova(m4, m5) # not hiearchical so cannot compare
AIC(m4) - AIC(m5) # difference in AIC between the two
anova(m4, m6) # m6 is better
BIC(m4) - BIC(m6) # difference in AIC between the two, M6 is smaller


# normalise the variables in the model -------------------------------------------------
names(tidy)
tidyn <- tidy %>% mutate_each_(funs(scale),vars=c("share", "acem_avg","noquals", "unemployed_pc_all", "percem", "percwb", "depmean", "Mean.age")) 
colMeans(tidyn$depmean, tidyn$unemployed_pc_all)



# robust regression on chosen model ---------------------------------------
# use robust regression
library(MASS)
summary(r1 <- rlm(share ~ acem_avg + noquals + unemployed_pc_all + percem + depmean,  data = tidyn))
summary(r2 <- rlm(share ~ acem_avg + noquals + unemployed_pc_all + percem + depmean,  data = tidy))
summary(m7 <- lm(share ~ acem_avg + noquals + unemployed_pc_all + percem + depmean,  data = tidyn))

# install.packages("robust")
library(robust)
# help(lmRob)
summary(r3 <- lmRob(share ~ acem_avg + noquals + unemployed_pc_all + percem + depmean,  data = tidyn))

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(m7, type = c("lmg","last","first","pratt"),
            rela=TRUE)
plot(calc.relimp(m7, sort=TRUE))

# Bootstrap Measures of Relative Importance (1000 samples) 
# boot <- boot.relimp(m7, b = 1000, type = c("lmg", 
                                            "last", "first", "pratt"), rank = TRUE, 
                    diff = TRUE, rela = TRUE)
# booteval.relimp(boot) # print result
# plot(booteval.relimp(boot,sort=TRUE)) # plot result

# help(calc.relimp)


# check the diagnostics of the model --------------------------------------

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(m7)

# Assessing Outliers
library(car)

outlierTest(m7) # Bonferonni p-value for most extreme obs
outlierTest(r1)
names(tidyn)

tidyn[c(113, 369, 466), 1] 
tidyn[c(113, 369, 419, 218), 1] # outliers 113 = Clacton, 369 = Rochester and Strood, 419 = South Thanet, 218 = Heywood and Middleton

qqPlot(m7, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(m7) # leverage plots

# Normality of Residuals
# qq plot for studentized resid
qqPlot(m4, main="QQ Plot")

# distribution of studentized residuals
require(MASS)
sresid <- studres(m7) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) # residuals are normalised

# extract coefficients
coef(m7)

# look at standardised coefficients
# install.packages("lm.beta")
library(lm.beta)
m7.beta <- lm.beta(m7)
print(m7.beta)
summary(m7.beta)
summary(m7)


# final plan for the model ------------------------------------------------
# this model estimates a non-robust regression on the predictor variables, which have all been standardised. This means the resulting coefficients are also standardised.

summary(m7.1 <- lm(share ~ acem_avg + noquals + unemployed_pc_all + percem + depmean,  data = tidy))
m7.1.beta <- lm.beta(m7.1)
print(m7.1.beta)
summary(m7.1.beta)

m7.2 <- lm(share ~ acem_avg + noquals + unemployed_pc_all + percem + depmean,  data = tidyn)
m7.2.beta <- lm.beta(m7.2)
print(m7.2.beta)
summary(m7.2.beta) #now that I've standardized the outcome variable too, I get the same result when estimating the model on the standardized df as I do when I standardize the coeffients after estimating the model

summary(r4 <- lmRob(share ~ acem_avg + noquals + unemployed_pc_all + percem + depmean + Mean.age,  data = tidyn))


# fix under-scaled variables ----------------------------------------------

# two variables haven't been multiplied by 100 i.e. %noquals is 0.54 rather than 54.0

histogram(tidy$noquals)
histogram(tidy$unemployed_pc_all)

# go back and multiply these in the original tidy df
tidy$noquals100 <- tidy$noquals * 100 # these vars were 100 times too small
tidy$unemp100 <- tidy$unemployed_pc_all * 100 # these vars were 100 times too small



summary(tidyn$noquals)
summary(tidyn$noquals100)
sd(tidyn$noquals100)
summary(tidyn$acem_avg)
sd(tidyn$acem_avg)

# standardise the variables again
rm(tidyn)
library(dplyr)
names(tidy)
tidyn <- tidy %>% mutate_each_(funs(scale),vars=c("share", "acem_avg","noquals100", "unemp100", "percem", "percwb", "depmean", "Mean.age")) 
colMeans(tidyn$depmean)
colMeans(tidyn$unemp100)
colMeans(tidyn$noquals100)
colMeans(tidyn$share)

library(robust)
summary(r5 <- lmRob(share ~ acem_avg + noquals100 + unemp100 + percem + depmean + Mean.age,  data = tidyn))

std.coeff <- r5$coefficients
se <- sqrt(diag(r5$cov))
se

vars <- c("intercept", "mean 5A*-CEM", "percent no qualifications", "percent unemployed (aged 16+)", "percent non-White British", "mean deprivation", "mean age")

points <- cbind(vars, std.coeff, se)
pointsdf <- as.data.frame(points)
attach(pointsdf)
pointsdf$ub <- std.coeff + (1.96*se)
pointsdf$lb <- std.coeff - (1.96*se)
names(pointsdf)
str(pointsdf)
pointsdf$coeff <- as.numeric(paste(pointsdf$std.coeff))
pointsdf <- pointsdf[order(pointsdf$coeff),]
pointsdf2 <- pointsdf[-c(4),]

mean(tidy$acem_avg)
summary(tidy$acem_avg)
sd(tidy$acem_avg)
sd(tidy$share) * 0.09
sd(tidy$depmean)
sd(tidy$share) * -0.26
sd(tidy$percem)
sd(tidy$share) * -0.48
sd(tidy$noquals100)
sd(tidy$share) * 0.47

# View(pointsdf2)

# plot


g1 <- ggplot(pointsdf2, aes(x = vars, y = coeff)) +
    geom_point(aes(size=4)) + 
    geom_hline(yintercept = 0.0, colour = gray(1/2), lty = 2) +
    geom_hline(yintercept = 0.2, colour = "grey90", lty = 1) +
    geom_hline(yintercept = 0.4, colour = "grey90", lty = 1) +
    geom_hline(yintercept = 0.6, colour = "grey90", lty = 1) +
    geom_hline(yintercept = -0.2, colour = "grey90", lty = 1) +
    geom_hline(yintercept = -0.4, colour = "grey90", lty = 1) +
    geom_hline(yintercept = -0.6, colour = "grey90", lty = 1) +
    geom_errorbar(aes(ymin=lb,ymax=ub),width=0.1) +
    theme_classic() +
    coord_flip() +
    theme(legend.position="none") +
    scale_x_discrete("Constituency characteristics") + 
    scale_y_continuous("Standardised effect on UKIP vote share", limits = c(-0.6, 0.6), breaks = c(-0.6, -0.4,-0.2, 0, 0.2, 0.4, 0.6))
g1


# create a data table just with mapping vars ------------------------------

names(tidy)
map <- tidy[, c(1, 6, 11, 15, 22, 30, 31, 32)]
# write.xlsx(map, file = "UKIP_map_data.xlsx", colNames = TRUE)
