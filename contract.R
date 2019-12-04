library(plyr)
library(stringr)
# Just for Practice
election2018 <- read.delim('~/STAT 350/Contract Project/final2018.txt', header=FALSE, sep = '|')
# Actual Dataset
electionDF <- read.delim('~/STAT 350/Contract Project/electionData.txt', header=FALSE, sep = '|')

# Use Average CPI and translate to 2016 dollars
averageCPI = c(240.008, 237.017, 236.736, 232.957, 229.594, 224.939,
              218.056, 214.537, 215.303, 207.342, 201.600, 195.300,
              188.900, 183.960, 179.880, 177.100, 172.200, 166.600,
              163.000, 160.500, 156.900, 152.400)
year = c(2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007, 
          2006, 2005, 2004, 2003, 2002, 2001, 2000, 1999, 1998, 1997,
          1996, 1995)
CPI = data.frame(averageCPI, year)

# Extract just the Year of each transaction
electionDF$Year <- str_match(electionDF$V5, '\\d{4}$')

# Add a column to keep track of the CPI that year
electionDF$Inflation <- lapply(electionDF, function(x) CPI$averageCPI[match(x, CPI$year)])$Year

# Adjust for inflation! Put it in 2016 dollars
electionDF$Amount <- as.numeric(with(electionDF, V6 * 240.008 / inflation))

# Rename our V1 and V4
names(electionDF)[1] <- "Recipient"
names(electionDF)[4] <- "Donor"

# Create the new dataset with all donations from each Donor -> Recipient
#   pair summed up
# Will try first without accounting for the year
# But because of the same candidate running multiple times and inflation rates,
# should probably do on a per-year basis
# summedDF <- ddply(electionDF, .(Recipient, Donor), .fun=function(electionDF) sum(electionDF$Amount))

# This is taking a while... possibly I need to speed this up by doing it per 2 year cycle and adding at the end
# Or maybe just each year, because I really don't want to deal with +100/-100 across multiple years due to inflation
# Final Decision is to add up over each year
summedDF2 <- ddply(electionDF, .(Recipient, Donor, Year), .fun=function(electionDF) sum(electionDF$Amount))


# Saved these to summedData.txt and summedData2.txt, respectively with save(summedDF, file="summedData.txt")

# load("summedData2.txt") # brings this data back

# Now on to the analysis!

library(ggplot2)

# FIRST UP: Is the Average donation to an Incumbent different between
# Republican Candidates and Democratic Candidates?
# Republican Incumbent: George W. Bush, 2004 (2001-2004)
# Democratic Incumbent: Barack H. Obama, 2012 (2009-2012)
# (could include Clinton for 1995-1996 as well)


incumbentDF <- subset(summedDF2, 
  (
  (Year == '2009' | Year == '2010' | Year == '2011' | Year == '2012')
  & (Recipient == 'C00431445')) |
  (Year == '2001' | Year == '2002' | Year == '2003' | Year == '2004')
  & (Recipient == 'C00404343' | Recipient == 'C00388579' |
      Recipient == 'C00386987'))

names(incumbentDF)[4] <- "Amount"
# Chooising to leave out Bush's 2000-era committees due to negatives

incumbentDF$Party <- with(incumbentDF, ifelse(Recipient == 'C00431445', 'Democrat', 'Republican'))

incumbentDF$Party <- factor(incumbentDF$Party)

# HISTOGRAM FOR EACH GROUP
xbar <- tapply(incumbentDF$Amount, incumbentDF$Party, mean)
s    <- tapply(incumbentDF$Amount, incumbentDF$Party, sd)

incumbentDF$normal.density <- ifelse(incumbentDF$Party == "Democrat",
                                      dnorm(incumbentDF$Amount, xbar["Democrat"], s["Democrat"]),
                                      dnorm(incumbentDF$Amount, xbar["Republican"], s["Republican"]))

#binlen <- as.numeric(max(tapply(incumbentDF$Amount, incumbentDF$Party, length)))
ggplot(incumbentDF, aes(x = Amount)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,#sqrt(binlen)+2,
                 fill = "grey", col = "black") +
  facet_grid(Party ~ .) +
  geom_density(col = "red", lwd = 1) +
  geom_line(aes(y = normal.density), col = "blue", lwd = 1) +
  ggtitle("Histograms of Donation Amount by Party")

# Things are SKEWED!
# Time to do some transformation
# BUT FIRST we need to remove the negatives and the zeroes
incumTransDF <- subset(incumbentDF, Amount > 100)  # With > 0 we get some -29 in logs so let's use a higher minimum
incumTransDF$LogAmount <- log(incumTransDF$Amount, 10)

# HISTOGRAM FOR EACH GROUP
xbar <- tapply(incumTransDF$LogAmount, incumTransDF$Party, mean)
s    <- tapply(incumTransDF$LogAmount, incumTransDF$Party, sd)

incumTransDF$normal.density <- ifelse(incumTransDF$Party == "Democrat",
        dnorm(incumTransDF$LogAmount, xbar["Democrat"], s["Democrat"]),
        dnorm(incumTransDF$LogAmount, xbar["Republican"], s["Republican"]))

#binlen <- as.numeric(max(tapply(incumTransDF$LogAmount, incumTransDF$Party, length)))
ggplot(incumTransDF, aes(x = LogAmount)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,#sqrt(binlen)+2,
                 fill = "grey", col = "black") +
  facet_grid(Party ~ .) +
  geom_density(col = "red", lwd = 1) +
  geom_line(aes(y = normal.density), col = "blue", lwd = 1) +
  ggtitle("Histograms of Log of Donation Amount by Party")

# BOXPLOT
ggplot(incumTransDF, aes(x = Party, y = LogAmount)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  stat_summary(fun.y = mean, col = "black", geom = "point", size = 3) +
  ggtitle("Boxplots of Log of Donation Amount by Party")

# NORMAL PROBABILITY PLOT
incumTransDF$intercept <- ifelse(incumTransDF$Party == "Democrat",
                                  xbar["Democrat"], xbar["Republican"])
incumTransDF$slope <- ifelse(incumTransDF$Party == "Democrat",
                             xbar["Democrat"], xbar["Republican"])

ggplot(incumTransDF, aes(sample = LogAmount)) +
  stat_qq() +
  facet_grid(Party ~ .) +
  geom_abline(data = incumTransDF, aes(intercept = intercept,
                                       slope = slope)) +
  ggtitle("QQ Plots of Log of Donation Amount by Party")



# This is horrendously NOT normal.... but I will still attempt to do some analysis on it

t.test(incumTransDF$LogAmount ~ incumTransDF$Party, mu = 0, conf.level = 0.99,
       paired = FALSE, alternative = "two.sided", var.equal = FALSE)

t.test(incumTransDF$LogAmount ~ incumTransDF$Party, mu = 0, conf.level = 0.99,
       paired = FALSE, alternative = "less", var.equal = FALSE)

# NOW IT'S TIME TO ANOVA

republicanDF <- subset(summedDF2, 
  ((Year == '2013' | Year == '2014' | Year == '2015' | Year == '2016')
   & (Recipient == 'C00580100')) |
  ((Year == '2009' | Year == '2010' | Year == '2011' | Year == '2012')
   & (Recipient == 'C00431171')) |
  ((Year == '2005' | Year == '2006' | Year == '2007' | Year == '2008')
   & (Recipient == 'C00453928' | Recipient == 'C00446104' |
      Recipient == 'C00430470')) |
  ((Year == '1997' | Year == '1998' | Year == '1999' | Year == '2000')
   & (Recipient == 'C00360503' | Recipient == 'C00346932' |
      Recipient == 'C00343509')))

republicanDF$Candidate <- with(republicanDF, 
                      ifelse(Recipient == 'C00580100', 'Trump', 
                      ifelse(Recipient == 'C00431171', 'Romney', 
                      ifelse(Recipient == 'C00453928' | 
                             Recipient == 'C00446104' |
                             Recipient == 'C00430470', 'McCain', 'Bush'))))


republicanDF$Candidate <- factor(republicanDF$Candidate)

names(republicanDF)[4] <- "Amount"

# Histogram
xbar <- tapply(republicanDF$Amount, republicanDF$Candidate, mean)
s <- tapply(republicanDF$Amount, republicanDF$Candidate, sd)

republicanDF$normal.density <- apply(republicanDF, 1, function(x){
  dnorm(as.numeric(x["Amount"]), 
        xbar[x["Candidate"]], s[x["Candidate"]])})

ggplot(republicanDF, aes(x = Amount)) +
  geom_histogram(aes(y = ..density..), bins = 35, 
                 fill = "grey", col = "black") +
  facet_grid(Candidate ~ .) +
  geom_density(col = "red", lwd = 0.5) +
  geom_line(aes(y = normal.density), col = "blue", lwd = 0.5) +
  ggtitle("Histograms of Donation Amount by Canddiate")

# Things are SKEWED!
# Time to do some transformation
# BUT FIRST we need to remove the negatives and the zeroes
repubTransDF <- subset(republicanDF, Amount > 100)  # With > 0 we get some -15 in logs so let's use a higher minimum
repubTransDF$LogAmount <- log(repubTransDF$Amount, 10)

# EFFECTS PLOT
ggplot(data = repubTransDF, aes(x = Candidate, LogAmount)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1)) +
  ggtitle("Effects Plot of Log Donation Amount by Candidate")

# HISTOGRAM
xbar <- tapply(repubTransDF$LogAmount, repubTransDF$Candidate, mean)
s <- tapply(repubTransDF$LogAmount, repubTransDF$Candidate, sd)

repubTransDF$normal.density <- apply(repubTransDF, 1, function(x){
  dnorm(as.numeric(x["LogAmount"]), 
        xbar[x["Candidate"]], s[x["Candidate"]])})

#binlen
ggplot(repubTransDF, aes(x = LogAmount)) +
  geom_histogram(aes(y = ..density..), bins = 35, 
                 fill = "grey", col = "black") +
  xlim(c(2, 5)) + # try removing outliers
  facet_grid(Candidate ~ .) +
  geom_density(col = "red", lwd = 0.5) +
  geom_line(aes(y = normal.density), col = "blue", lwd = 0.5) +
  ggtitle("Histograms of Log Donation Amount by Canddiate")

# BOXPLOTS
ggplot(repubTransDF, aes(x = Candidate, y = LogAmount)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  stat_summary(fun.y = mean, col = "black", geom = "point", size = 3) +
  ggtitle("Boxplots of Log Donation Amount by Candidate")

# QQ PLOTS
repubTransDF$intercept <- apply(repubTransDF, 1, function(x){xbar[x["Candidate"]]})
repubTransDF$slope <- apply(repubTransDF, 1, function(x){s[x["Candidate"]]})

ggplot(repubTransDF, aes(sample = LogAmount)) +
  stat_qq() +
  facet_grid(Candidate ~ .) +
  geom_abline(data = repubTransDF, aes(intercept = intercept, slope = slope)) +
  ggtitle("QQ Plots of Log Donation Amount by Candidate")

# ANOVA
fit <- aov(LogAmount ~ Candidate, data = repubTransDF)
summary(fit)

# Tukey
TukeyHSD(fit, conf.level = 0.95)
qtukey(0.95, 3, 529672 - 3)
