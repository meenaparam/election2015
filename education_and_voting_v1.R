### Exploring relationships between political participation and educational outcomes ###
### MP 15/05/15 ###

setwd("~/GitHub/election2015")

library(openxlsx)
library(reshape)
library(pastecs)
library(ggplot2)


# setting up election datasets --------------------------------------------


# make a turnout dataframe
turnout <- read.xlsx(xlsxFile = "turnout2015_clean.xlsx", sheet = 1, startRow = 1, skipEmptyRows = TRUE)

votes <- read.csv("~/GitHub/election2015/votes2015.csv")
view(votes)

table(votes$position) # look at distribution of positions
class(votes$position)
votes <- votes[votes$position != 1, ] # get rid of empty first rows
votes$position <- votes$position - 1 # re-rank candidates

str(votes) # look at data structure
str(turnout)
turnout$constituency <- turnout$constituency_original
# merge together turnout and votes data frames

election <- merge(votes, turnout, by = "constituency")
names(election)

levels(votes$party)


# making separate party datasets ------------------------------------------


ukip <- election[election$party == "UKIP", ] # keep only UKIP candidates
lab <- election[election$party == "LAB", ] # keep only LAB candidates
con <- election[election$party == "CON", ] # keep only CON candidates
ld <- election[election$party == "LD", ] # keep only LD candidates


# making gcse results datasets --------------------------------------------

# make GCSE results dataframes
gcse_years <- c(2011, 2012, 2013, 2014)

x = 5
for (i in gcse_years) {
     z <- paste0("gcse", i)
     y <- read.xlsx(xlsxFile = "turnout2015_clean.xlsx", sheet = x, startRow = 1, skipEmptyRows = TRUE)
     assign(z, y)
     #str(y)
     x = x-1
     rm(y)
}

# check structure of gcse dfs
sapply(gcse2011, class)
table(gcse2011$Achieving.the.English.Baccalaureate)
names(gcse2011)

# check structure of turnout df
str(turnout)
turnout <- rename(turnout, c(constituency_renamed = "constituency_name"))
names(turnout) # check renaming of vars
turnout <- turnout[, -c(1,2,3,4,5)] # get rid of extra columns

# rename gcse df vars
names(gcse2011)

gcse2011 <- rename (gcse2011, c(constituency.code = "code","5+.A*-C.grades" = "AC", "5+.A*-G.grades" = "AG", "5+.A*-C.grades.inc..English.and.mathematics.GCSEs" = "ACEM", "5+.A*-G.grades.inc..English.and.mathematics.GCSEs" = "AGEM", "A*-C.in.English.and.mathematics.GCSEs" = "EM", Entering.the.English.Baccalaureate = "EEB", Achieving.the.English.Baccalaureate = "AEB"))
colnames(gcse2011) <- paste(colnames(gcse2011), "2011", sep = "_")
names(gcse2011)

gcse2012 <- rename (gcse2012, c(constituency.code = "code","5+.A*-C.grades" = "AC", "5+.A*-G.grades" = "AG", "5+.A*-C.grades.inc..English.and.mathematics.GCSEs" = "ACEM", "5+.A*-G.grades.inc..English.and.mathematics.GCSEs" = "AGEM", "A*-C.in.English.and.mathematics.GCSEs" = "EM", Entering.the.English.Baccalaureate = "EEB", Achieving.the.English.Baccalaureate = "AEB"))
colnames(gcse2012) <- paste(colnames(gcse2012), "2012", sep = "_")
names(gcse2012)

gcse2013 <- rename (gcse2013, c(constituency.code = "code","5+.A*-C.grades" = "AC", "5+.A*-G.grades" = "AG", "5+.A*-C.grades.inc..English.and.mathematics.GCSEs" = "ACEM", "5+.A*-G.grades.inc..English.and.mathematics.GCSEs" = "AGEM", "A*-C.in.English.and.mathematics.GCSEs" = "EM", Entering.the.English.Baccalaureate = "EEB", Achieving.the.English.Baccalaureate = "AEB"))
colnames(gcse2013) <- paste(colnames(gcse2013), "2013", sep = "_")
names(gcse2013)

gcse2014 <- rename (gcse2014, c(constituency.code = "code","5+.A*-C.grades" = "AC", "5+.A*-G.grades" = "AG", "5+.A*-C.grades.inc..English.and.mathematics.GCSEs" = "ACEM", "5+.A*-G.grades.inc..English.and.mathematics.GCSEs" = "AGEM", "A*-C.in.English.and.mathematics.GCSEs" = "EM", Entering.the.English.Baccalaureate = "EEB", Achieving.the.English.Baccalaureate = "AEB"))
colnames(gcse2014) <- paste(colnames(gcse2014), "2014", sep = "_")
names(gcse2014)

# make a constituency variable with the same name
gcse2011$cons <- gcse2011$constituency_name_2011
gcse2012$cons <- gcse2012$constituency_name_2012
gcse2013$cons <- gcse2013$constituency_name_2013
gcse2014$cons <- gcse2014$constituency_name_2014


# merging gcse datasets ---------------------------------------------------


# merge the gcse datasets
gcsemerge <- merge(gcse2011, gcse2012, by = "cons")
gcsemerge <- merge(gcsemerge, gcse2013, by = "cons")
gcsemerge <- merge(gcsemerge, gcse2014, by = "cons")
str(gcsemerge)

# get rid of duplicate constituency name vars
gcsemerge$constituency_name_2011 <- NULL
gcsemerge$constituency_name_2012 <- NULL
gcsemerge$constituency_name_2013 <- NULL
gcsemerge$constituency_name_2014 <- NULL

colnames(gcsemerge) # see column names and positions in gcsemerge


# making gcse averages across years ---------------------------------------

gcsemerge$acem_avg <- rowMeans(gcsemerge[, c(5, 14, 23, 32)])
table(gcsemerge$acem_avg)
library(pastecs)
stat.desc(gcsemerge$acem_avg)

gcsemerge$eeb_avg <- rowMeans(gcsemerge[, c(9, 18, 27, 36)])
summary(gcsemerge$eeb_avg)

gcsemerge$aeb_avg <- rowMeans(gcsemerge[, c(10, 19, 28, 37)])
summary(gcsemerge$aeb_avg)


# merge gcse data with each party’s data ----------------------------------

# merge in the gcse data for each party
names(gcsemerge)
names(ukip)
ukip$cons <- ukip$constituency_renamed
ukip_gcse <- merge(ukip, gcsemerge, by = "cons")

# ukip has 610 obs; ukip_gcse has 527 obs - which ones are missing?
ukipdiff <- setdiff(ukip$cons, ukip_gcse$cons)
ukipdiff # fine, the 83 that are missing are in Scotland, Wales and NI

lab$cons <- lab$constituency_renamed
lab_gcse <- merge(lab, gcsemerge, by = "cons")

con$cons <- con$constituency_renamed
con_gcse <- merge(con, gcsemerge, by = "cons")

ld$cons <- ld$constituency_renamed
ld_gcse <- merge(ld, gcsemerge, by = "cons")

# graphs by party ------------------------------------------------------------------

#ukip
names(ukip_gcse)
g1_ukip_acem <- ggplot(ukip_gcse, aes(acem_avg, share)) + geom_point() + geom_smooth(method = "lm", colour = "purple", alpha = 0.1, fill = "purple") + labs (x = "average 5A*C inc E&M 2011-2014", y = "UKIP vote share 2015")
g1_ukip_acem
g2_ukip_eeb <- ggplot(ukip_gcse, aes(eeb_avg, share)) + geom_point() + geom_smooth(method = "lm", colour = "purple", alpha = 0.1, fill = "purple") + labs (x = "average % of pupils entered for EBacc 2011-2014", y = "UKIP vote share 2015")
g2_ukip_eeb
g3_ukip_aeb <- ggplot(ukip_gcse, aes(aeb_avg, share)) + geom_point() + geom_smooth(method = "lm", colour = "purple", alpha = 0.1, fill = "purple") + labs (x = "average % of pupils achieving EBacc 2011-2014", y = "UKIP vote share 2015")
g3_ukip_aeb

#labour
names(lab_gcse)
g1_lab_acem <- ggplot(lab_gcse, aes(acem_avg, share)) + geom_point() + geom_smooth(method = "lm", colour = "red", alpha = 0.1, fill = "red") + labs (x = "average 5A*C inc E&M 2011-2014", y = "Labour vote share 2015")
g1_lab_acem
g2_lab_eeb <- ggplot(lab_gcse, aes(eeb_avg, share)) + geom_point() + geom_smooth(method = "lm", colour = "red", alpha = 0.1, fill = "red") + labs (x = "average % of pupils entered for EBacc 2011-2014", y = "Labour vote share 2015")
g2_lab_eeb
g3_lab_aeb <- ggplot(lab_gcse, aes(aeb_avg, share)) + geom_point() + geom_smooth(method = "lm", colour = "red", alpha = 0.1, fill = "red") + labs (x = "average % of pupils achieving EBacc 2011-2014", y = "Labour vote share 2015")
g3_lab_aeb

#tory
names(con_gcse)
g1_con_acem <- ggplot(con_gcse, aes(acem_avg, share)) + geom_point() + geom_smooth(method = "lm", colour = "blue", alpha = 0.1, fill = "blue") + labs (x = "average 5A*C inc E&M 2011-2014", y = "Conservative vote share 2015")
g1_con_acem
g2_con_eeb <- ggplot(con_gcse, aes(eeb_avg, share)) + geom_point() + geom_smooth(method = "lm", colour = "blue", alpha = 0.1, fill = "blue") + labs (x = "average % of pupils entered for EBacc 2011-2014", y = "Conservative vote share 2015")
g2_con_eeb
g3_con_aeb <- ggplot(con_gcse, aes(aeb_avg, share)) + geom_point() + geom_smooth(method = "lm", colour = "blue", alpha = 0.1, fill = "blue") + labs (x = "average % of pupils achieving EBacc 2011-2014", y = "Conservative vote share 2015")
g3_con_aeb

#lib dems
names(ld_gcse)
g1_ld_acem <- ggplot(ld_gcse, aes(acem_avg, share)) + geom_point() + geom_smooth(method = "lm", colour = "yellow", alpha = 0.3, fill = "yellow") + labs (x = "average 5A*C inc E&M 2011-2014", y = "Liberal Democrat vote share 2015")
g1_ld_acem
g2_ld_eeb <- ggplot(ld_gcse, aes(eeb_avg, share)) + geom_point() + geom_smooth(method = "lm", colour = "yellow", alpha = 0.3, fill = "yellow") + labs (x = "average % of pupils entered for EBacc 2011-2014", y = "Liberal Democrat vote share 2015")
g2_ld_eeb
g3_ld_aeb <- ggplot(ld_gcse, aes(aeb_avg, share)) + geom_point() + geom_smooth(method = "lm", colour = "yellow", alpha = 0.3, fill = "yellow") + labs (x = "average % of pupils achieving EBacc 2011-2014", y = "Liberal Democrat vote share 2015")
g3_ld_aeb


# recoding constituency results -------------------------------------------

str(election$result)
election$resultfac <- as.factor(election$result)
levels(election$resultfac)

# setting up the recoded groups
conservative <- levels(election$resultfac)[1:4]
other <- levels(election$resultfac)[c(5:8, 14:20, 22:23)]
labour <- levels(election$resultfac)[9:12]
libdem <- levels(election$resultfac)[13]


election$winner <- NA
election$winner[election$resultfac %in% conservative] <- "CON"
election$winner[election$resultfac %in% other] <- "OTH"
election$winner[election$resultfac %in% labour] <- "LAB"
election$winner[election$resultfac %in% libdem] <- "LIB"
table(election$winner)

# turnout vs. majority
g1_turnout <- ggplot(election, aes(turnout, majority)) + geom_point(aes(colour = factor(winner))) + geom_smooth(method = "lm", colour = "grey", alpha = 0.3, fill = "grey") + labs (x = "constituency turnout %", y = "constituency majority %", colour = "Winner")
g1_turnout

election_lab <- election[which(election$winner=="LAB"), ]
g2_turnout_lab <- ggplot(election_lab, aes(turnout, majority)) + geom_point(colour = "red") + geom_smooth(method = "lm", colour = "grey", alpha = 0.3, fill = "grey") + labs (x = "constituency turnout %", y = "constituency majority %", colour = "Labour") + theme(legend.position = "none") + ggtitle("Labour turnout vs. majority")
g2_turnout_lab

election_con <- election[which(election$winner=="CON"), ]
g3_turnout_con <- ggplot(election_con, aes(turnout, majority)) + geom_point(colour = "blue") + geom_smooth(method = "lm", colour = "grey", alpha = 0.3, fill = "grey") + labs (x = "constituency turnout %", y = "constituency majority %", colour = "Conservatives") + theme(legend.position = "none") + ggtitle("Conservative turnout vs. majority")
g3_turnout_con


# merge election and gcsemerge datasets -----------------------------------

gcsemerge$constituency <- gcsemerge$cons #get a same name var to merge
election_education <- merge(election, gcsemerge, by = "constituency")
election_education <- election_education[order(election_education$constituency, election_education$position), ]
names(election_education)
keep_el_ed <- names(election_education)[c(1:12, 14:15, 17:20, 23:30, 32:39, 41:60)]
el_ed <- election_education[keep_el_ed]

# write out the election_education file
write.xlsx(el_ed, file = "election_education.xlsx", colNames = T)

# test change