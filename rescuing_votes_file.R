votes2 <- read.xlsx(xlsxFile = "voteslookups2015.xlsx", sheet = 1, startRow = 1, skipEmptyRows = TRUE)
names(votes2)

votes2 <- votes2[votes2$position != 1, ] # get rid of empty first rows
votes2$position <- votes2$position - 1 # re-rank candidates

write.xlsx(votes2, file = "votes_v2.xlsx", colNames = T)