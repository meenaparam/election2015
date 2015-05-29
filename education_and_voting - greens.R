### Exploring relationships between political participation and educational outcomes ###
### MP 27/05/15 ###
### Repeating the analysis for the Greens ###


setwd("~/GitHub/election2015")

library(openxlsx)
library(reshape)
library(pastecs)
library(ggplot2)
library(dplyr)

# load the dataset
ee <- read.xlsx(xlsxFile = "election_education_v3.xlsx")
names(ee)
str(ee$party)
ee$partyfac <- as.factor(ee$party)
str(ee$partyfac)
table(ee$partyfac) #502 green party candidates


# making a green party datasets ------------------------------------------

green <- ee[ee$party == "GRN", ] # keep only GRN candidates

# graph for greens  ------------------------------------------------------------------

#green
names(green)
g1_green_acem <- ggplot(green, aes(acem_avg, share)) + geom_point() + geom_smooth(method = "lm", colour = "green", alpha = 0.1, fill = "green") + labs (x = "Average 5A*C inc E&M 2011-2014", y = "Green vote share 2015") 
# + ggtitle ("Relationship between constituency average GCSE performance \n and Green vote share") +  theme(plot.title = element_text(size = 12))
g1_green_acem

cor(green$acem_avg, green$share)
cor.test(green$acem_avg, green$share)
library(psych)
describe(green$acem_avg)
describe(green$share)

m1 <- lm(green$share ~ green$acem_avg)
summary(m1)

# where was the green vote high?
high <- green[green$share > 25,]
high$constituency_renamed
