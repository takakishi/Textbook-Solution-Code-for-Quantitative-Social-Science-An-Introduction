## Chapter 3 Measurement
## Exercise Solution

setwd("/users/takaaki/documents/qss/MEASUREMENT")

## -----------------------------------------------------
## Taka(the author of this script) uses Japanese-Version QSS.
## -----------------------------------------------------
## Section 1
## Q1

reshaped <- read.csv("gayreshaped.csv")
ccap <- read.csv("ccap2012.csv")

head(reshaped)
head(ccap)

## "complete.obs" perform a calculation for rows which have perfect matching. 
## Correlation is strong b/w the two. 
cor(reshaped$therm1, reshaped$therm2, use = "complete.obs")


## Q2

## Create the subset
study2 <- subset(reshaped, subset = (study == 2) & (treatment == "No Contact"))
study2a <- study2[, 3:6]
class(study2a)

## "pairwise.complete.obs" perform a calculation for a data.frame which
## has several variables & missing values. 
cor(study2a, use = "pairwise.complete.obs") # correlation coefficient matrix

## The result is slightly different form the argument "pairwise.complete.obs". 
## See. http://keita43a.hatenablog.com/entry/2018/04/10/034230. 
cor(study2a, use = "complete.obs")

## All correlation coefficients are nealy 1 (means storong correlation).

## Extra exercise, ploting the matrix using packages psych & corrplot
## install.packages("psych")
library(psych)
psych::pairs.panels(study2a)
psych::pairs.panels(study2a, 
                    hist.col = "white", rug = F, ellipses = F, lm = T)

## install.packages("corrplot")
library(corrplot)
corrplot::corrplot(cor(study2a, use = "complete.obs"))

cor.plot(cor(study2a, use = "complete.obs"), numbers=T)


## Q3

## You might encounter "Error in plot.new() : figure margins too large"
## use dev.new() & dev.off() before and after the command plot

plot(study2a$therm1, study2a$therm2, pch = 20,
     col = "blue", xlab = "Therm 1", ylab = "Therm 2") 

plot(study2a$therm1, study2a$therm3, pch = 20,
     col = "red", xlab = "Therm 1", ylab = "Therm 3") 

plot(study2a$therm1, study2a$therm4, pch = 20,
     col = "green", xlab = "Therm 1", ylab = "Therm 4") 

## Plot & save 3 scatter plots in 1 screan (a bit weird though).
png("aaa.png")
split.screen(figs = c(1, 2))
split.screen(figs = c(2, 1), screen = 2)

screen(1)
plot(study2a$therm1, study2a$therm2, pch = 20,
     col = "blue", xlab = "Therm 1", ylab = "Therm 2") 
screen(3)
plot(study2a$therm1, study2a$therm3, pch = 20,
     col = "red", xlab = "Therm 1", ylab = "Therm 3") 
screen(4)
plot(study2a$therm1, study2a$therm4, pch = 20,
     col = "green", xlab = "Therm 1", ylab = "Therm 4") 
dev.off()

## Get back to 1 screan format. 
## close.screen(all = T)

## Another way to plot. 
## Plot 3 groups in 1 picture. 

plot(0, 0, type = "n", 
     xlim = c(0, max(study2a$therm1)), 
     ylim = c(0, max(study2a$therm1)), 
     xlab = "therm1", ylab = "therm2-4")

points(study2a$therm1, study2a$therm2, pch = 20, col = "red") 
points(study2a$therm1, study2a$therm3, pch = 20, col = "blue") 
points(study2a$therm1, study2a$therm4, pch = 20, col = "green") 

plot(0, 0, type = "n", 
     xlim = c(0, max(study2a$therm1)), 
     ylim = c(0, max(study2a$therm1)), 
     xlab = "therm1", ylab = "therm2-4")

points(study2a$therm1, study2a$therm2, pch = 20, col = "#30303020") 
points(study2a$therm1, study2a$therm3, pch = 20, col = "#ff990020") 
points(study2a$therm1, study2a$therm4, pch = 20, col = "#ffb6c120") 


## Q4

## Plot CCAP's gaytherm
hist(ccap$gaytherm, col = "#0000ff20", las = 1, freq = FALSE, 
     xlab = "therm", main = "Histogram of GayTherm", breaks = 20)

## Plot some lines
abline(v = mean(ccap$gaytherm, na.rm = TRUE), 
       lty = "dashed", col = "red")
text(x = 67, y = 0.030, "mean(58.7)", col = "red")
lines(density(ccap$gaytherm, na.rm = TRUE), col = "orange", lwd = 2)
mean(ccap$gaytherm, na.rm = TRUE)

## Plot study1's gaytherm

head(reshaped)
study1 <- subset(reshaped, reshaped$study == 1)

hist(study1$therm1, col = "#0000ff20", las = 1, freq = FALSE, 
     xlab = "therm", main = "Histogram of Study1's Therm1", breaks = 20)

hist(study2$therm1, col = "#0000ff20", las = 1, freq = FALSE, 
     xlab = "therm", main = "Histogram of Study2's Therm1", breaks = 20)


## Q5

qqplot(study2a$therm1, study2a$therm2, pch = 20, col = "#0000ff") 
abline(0, 1) # 45-degree line
qqplot(study2a$therm1, study2a$therm3, pch = 20, col = "#0000ff") 
abline(0, 1)
qqplot(study2a$therm1, study2a$therm4, pch = 20, col = "#0000ff") 
abline(0, 1)

## It is difficult to put a interpretation. 
## Q-Q plot is almost on the 45-degree line. 
## This means two variables' distributions are almost the same. 


## -----------------------------------------------------
## Section 2
## Q1

vign <- read.csv("vignettes.csv")
head(vign); dim(vign)

china <- subset(vign, vign$china == 1)
mexico <- subset(vign, vign$china != 1)

Cself <- prop.table(table(china$self))
Mself <- prop.table(table(mexico$self))

## Plot barplot of self-evalutaion in China & Mexico.
barplot(Cself, xlab = "Score of Self Evaluation in China", ylab = "ratio")
barplot(Mself, xlab = "Score of Self Evaluation in Mexico", ylab = "ratio")

## Calculate the means. 
Cself; Mself
mean(Cself); mean(Mself) 
## mean political effectiveness is the same, 
## but the shapes of distributions are totally different. 
## The results seem not to be consistent with their political conditions. 


## Q2

hist(china$age, ylim = c(0, 80))
abline(v = median(china$age), lty = "dashed", col = "red")
text(x = 50, y = 60, "median = 45")

hist(mexico$age, ylim = c(0, 80))
abline(v = median(mexico$age), lty = "dashed", col = "red")
text(x = 40, y = 80, "median = 35")

## Extra work: plotting hists in one picture (using "add = TRUE").

hist(china$age, ylim = c(0, 80), xlab = "age", main = "hist of age",
     col = "#0000ff40", border = "#0000ff")
abline(v = median(china$age), lty = "dashed", col = "red")
text(x = 50, y = 60, "median = 45")

hist(mexico$age, ylim = c(0, 80), col = "#ff00ff40", border = "#ff00ff", 
     add = TRUE)
abline(v = median(mexico$age), lty = "dashed", col = "red")
text(x = 40, y = 80, "median = 35")
cols <- c("#0000ff40", "#ff00ff40")
legend("topright", c("China", "Mexico"), col = cols, pch = 19)

## End the extra work. 

## Plot the Q-Q plots.
## China's Q-Q plot
qqplot(china$age, mexico$age)
abline(0, 1)

## First, the Q-Q plot is not on the 45-degree line, 
## which means thier age's distributions are not similar.
## Q-Q line is below the 45-degree line, and thus 
## Mexico's distribution is not that scattered like China counterpart. 


## Q3

## Percentage of respondents who give low self-evaluation
## less than eveluation about moses

## Create dummy variables in China & Mexico. 
Cmo.se <- sum(ifelse(china$moses > china$self, 1, 0))
Cmo.se.l <- length(ifelse(china$moses > china$self, 1, 0)) # no need to use ifelse

Mmo.se <- sum(ifelse(mexico$moses > mexico$self, 1, 0))
Mmo.se.l <- length(ifelse(mexico$moses > mexico$self, 1, 0))

## Answers
Cmo.se / Cmo.se.l # China
Mmo.se / Mmo.se.l # Mexico


## Q4

## Create subsets which have relationship alison > jane > moses. 
china3 <- subset(china, subset = (alison >= jane) & (jane >= moses))
mexico3 <- subset(mexico, subset = (alison >= jane) & (jane >= moses))
vign3 <- subset(vign, subset = (alison >= jane) & (jane >= moses))

nrow(mexico)
nrow(china3)

## Create categorical variables in China & Mexico, and overall. 
## overall
vign3$self.pos <- NA

vign3$self.pos[vign3$moses > vign3$self] <- 1

vign3$self.pos[(vign3$moses == vign3$self) | 
                  ((vign3$moses <= vign3$self) & 
                     (vign3$self < vign3$jane))] <- 2

vign3$self.pos[(vign3$jane == vign3$self) | 
                  ((vign3$jane <= vign3$self) & 
                     (vign3$self < vign3$alison))] <- 3

vign3$self.pos[(vign3$alison <= vign3$self)] <- 4

## China 
china3$self.pos <- NA

china3$self.pos[china3$moses > china3$self] <- 1

china3$self.pos[(china3$moses == china3$self) | 
                  ((china3$moses <= china3$self) & 
                     (china3$self < china3$jane))] <- 2

china3$self.pos[(china3$jane == china3$self) | 
                  ((china3$jane <= china3$self) & 
                     (china3$self < china3$alison))] <- 3

china3$self.pos[(china3$alison <= china3$self)] <- 4

china3$self.pos # confirm

## Mexico

mexico3$self.pop <- NA

mexico3$self.pop[mexico3$moses > mexico3$self] <- 1

mexico3$self.pop[(mexico3$moses == mexico3$self) | 
                   ((mexico3$self >= mexico3$moses) & 
                      (mexico3$jane > mexico3$self))] <- 2

mexico3$self.pop[(mexico3$jane == mexico3$self) | 
                   ((mexico3$self >= mexico3$jane) & 
                      (mexico3$alison > mexico3$self))] <- 3

mexico3$self.pop[(mexico3$self >= mexico3$alison)] <- 4

## bar plot
Vself.pop <- prop.table(table(vign3$self.pos))
Cself.pop <- prop.table(table(china3$self.pos))
Mself.pop <- prop.table(table(mexico3$self.pop))

barplot(Vself.pop)
barplot(Cself.pop)
barplot(Mself.pop)

## mean

mean(Vself.pop); mean(Cself.pop); mean(Mself.pop)


## Q5

## under & over 40
Vu40 <- subset(vign3, vign3$age < 40)
Vo40 <- subset(vign3, vign3$age >= 40)

Cu40 <- subset(china3, china3$age < 40)
Co40 <- subset(china3, china3$age >= 40)

Mu40 <- subset(mexico3, mexico3$age < 40)
Mo40 <- subset(mexico3, mexico3$age >= 40)

## proportion tables
Vu40.pop <- prop.table(table(Vu40$self.pos))
Vo40.pop <- prop.table(table(Vo40$self.pos))

Cu40.pop <- prop.table(table(Cu40$self.pos))
Co40.pop <- prop.table(table(Co40$self.pos))

Mu40.pop <- prop.table(table(Mu40$self.pos))
Mo40.pop <- prop.table(table(Mo40$self.pos))

## barplot(Vu40.pop); barplot(Vo40.pop)
## barplot(Cu40.pop); barplot(Co40.pop)
## barplot(Mu40.pop); barplot(Mo40.pop)

## mean
mean(Vu40.pop); mean(Vo40.pop)
mean(Cu40.pop); mean(Co40.pop)
mean(Mu40.pop); mean(Mo40.pop)


## -----------------------------------------------------
## Section 3
## Q1

un <- read.csv("unvoting.csv")
unique(un$Year)

## in the year 1980 & 2000
un80 <- un[c(un$Year == "1980"), ]
un00 <- un[c(un$Year == "2000"), ]
dim(un80); dim(un00)

## plot histgrams
hist(un80$idealpoint, col = "#0000ff40", border = "#0000ff")
abline(v = median(un80$idealpoint), lty = "dashed", col = "red")
hist(un00$idealpoint, col = "#ff00ff40", border = "#ff00ff")
abline(v = median(un00$idealpoint), lty = "dashed", col = "blue")

## Extra work: plotting hists in one picture (using "add = TRUE").
hist(un80$idealpoint, col = "#0000ff40", border = "#0000ff",
     xlab = "", main = "Estimated Ideal Point in 1980", 
     xlim = c(-3, 3), ylim = c(0, 60))
abline(v = median(un80$idealpoint), lty = "dashed", col = "red")
text(0.5, 50, "median(-0.09)", col = "red")

hist(un00$idealpoint, col = "#ff00ff40", border = "#ff00ff", 
     add = TRUE)
abline(v = median(un00$idealpoint), lty = "dashed", col = "blue")
text(-0.5, 60, "median(-0.35)", col = "blue")

cols <- c("#0000ff40", "#ff00ff40")
legend("topright", c("1980", "2000"), col = cols, pch = 19)

## calculate quantiles
quantile(un80$idealpoint)
quantile(un00$idealpoint)


## Q2

## time-series plot
trans.us <- tapply(un$PctAgreeUS, un$Year, mean)
trans.russia <- tapply(un$PctAgreeRUSSIA, un$Year, mean)

length(trans.mean)
plot(names(trans.us), trans.us, type = "l", col = "deepskyblue", 
     ylim = c(0, 0.9), xlab = "Year", ylab = "", 
     main = "Transition: Mean Percentage of Agree with US & Russia")
text(2010, 0.1, "US", col = "deepskyblue")
lines(names(trans.russia), trans.russia, type = "l", col = "pink")
text(2010, 0.6, "Russia", col = "pink")

## Find the countries where support the US & Russia the most. 
usMost <- tapply(un$PctAgreeUS, un$CountryName, mean)
russiaMost <- tapply(un$PctAgreeRUSSIA, un$CountryName, mean)

## [2] finds the socond highest element
## We can also get result w/o [2]
sort(usMost, decreasing = TRUE)[2]
sort(russiaMost, decreasing = TRUE)[2]

## FYI: Command which.max gets the place of maximum number.
## However now we need to find the socond best because US and Russia
## have obviously concordance rate 1. 
which.max(russiaMost) # length 143
russiaMost[143] # finds 143th element


## Q3

## Create subsets of US & Russia.
usIde <- subset(un, un$CountryName == "United States of America")
russiaIde <- subset(un, un$CountryName == "Russia")

## Create subsets.
usIde <- usIde[, c("Year", "idealpoint")]
russiaIde <- russiaIde[, c("Year", "idealpoint")]
medianIde <- tapply(un$idealpoint, un$Year, median)

## Plot transition. 
plot(usIde, type = "l", ylim = c(-3, 3), col = "red")
lines(russiaIde, type = "l", col = "blue")
lines(names(medianIde), medianIde, lty = "dashed")
text(2010, 3, "US", col = "red")
text(2010, 0.6, "Russia", col = "blue")
text(2010, -1, "UN-Median")


## Q4

## Soviet Union subset
soviet <- subset(un, subset = (CountryName == "Estonia") | 
         (CountryName == "Latvia") | 
         (CountryName == "Lithuania") | 
         (CountryName == "Belarus") | 
         (CountryName == "Moldova") | 
         (CountryName == "Ukraine") | 
         (CountryName == "Armenia") | 
         (CountryName == "Azerbaijan") | 
         (CountryName == "Georgia") | 
         (CountryName == "Kazakhstan") |
         (CountryName == "Kyrgyzstan") |
         (CountryName == "Tajikistan") |
         (CountryName == "Turkmenistan") |
         (CountryName == "Uzbekistan") |
         (CountryName == "Russia") 
)

## 2012 data
un2012 <- un[c(un$Year == "2012"), ]
soviet2012 <- soviet[c(soviet$Year == "2012"), ]

a <- un2012$CountryName %in% soviet2012$CountryName

## Soviet Union dummy
un2012$sov <- NA
un2012$sov <- ifelse((un2012$CountryName %in% soviet2012$CountryName) == TRUE, 1, 0)

un2012a <- subset(un2012, un2012$sov == 0)
un2012a <- un2012a[, c("idealpoint", "PctAgreeUS")]
soviet2012a <- soviet2012[, c("idealpoint", "PctAgreeUS")]

## Plot 2 groups in 1 picture. 
plot(0, 0, type = "n", 
     xlim = c(min(un2012a$idealpoint), max(un2012a$idealpoint)), 
     ylim = c(min(un2012a$PctAgreeUS), max(un2012a$PctAgreeUS)), 
     xlab = "idealpoint", ylab = "percentage agree with US", 
     main = "Soviet & non-Soviet in 2012")

points(un2012a, pch = 22, col = "blue")
points(soviet2012a, pch = 20, col = "red")

## same way to plot the points. 
points(un2012a$idealpoint, un2012a$PctAgreeUS, pch = 22, col = "deepskyblue") 
points(soviet2012a$idealpoint, soviet2012a$PctAgreeUS, pch = 20, col = "red") 

## There exists storong correlation b/w idealpoint & PctAgreeUS. 
## Tendency seems to be the same b/w Soviet and non-Soviet countries. 


## Q5

## Create non-Soviet subset
un$sov <- NA
un$sov <- ifelse((un$CountryName %in% soviet$CountryName) == TRUE, 1, 0)

un.sov <- subset(un, un$sov == 0)

trans.sov.m <- tapply(soviet$idealpoint, soviet$Year, median)
trans.un.m <- tapply(un.sov$idealpoint, un.sov$Year, median)

## plot lines
plot(names(trans.sov.m), trans.sov.m, type = "l", 
     xlab = "", ylab = "median ideal point", col = "red",
     ylim = c(-3, 1))
lines(names(trans.un.m), trans.un.m, type = "l", col = "blue")
abline(v = 1989, lty = "dashed") # Fall of the Berlin Wall
text(2007, 0.5, "Soviet", col = "red")
text(2000, -0.2, "non-Soviet", col = "blue")
text(1985, 1, "Fall of the Berlin Wall")
