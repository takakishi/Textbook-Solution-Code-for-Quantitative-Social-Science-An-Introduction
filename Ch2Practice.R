## Chapter 2 Causality 
## Exercise Solution

setwd("/users/takaaki/documents/qss/CAUSALITY")

## -----------------------------------------------------
## Taka(the author of this script) uses Japanese-Version QSS.
## -----------------------------------------------------
## Section 1
## Q1

star <- read.csv("STAR.csv")
head(star)

## Create kinder variable.
unique(star$classtype)
star$kinder <- NA 
star$kinder[star$classtype == 1] <- "small" 
star$kinder[star$classtype == 2] <- "middle"
star$kinder[star$classtype == 3] <- "large" 
class(star$kinder)
## Class of tinder has to be factor in the following question.
star$kinder <- as.factor(star$kinder)
unique(star$kinder)

## Re-create race variable.
unique(star$race)
star$race[star$race == 1] <- "white"
star$race[star$race == 2] <- "black"
star$race[star$race == 4] <- "hispanic"
star$race[star$race == 3 | star$race == 5 | star$race == 6] <- "others"


## Q2

small <- subset(star, star$kinder == "small") 
## small <- subset(star, subset = (kinder == "small")) ## same as above
middle <- subset(star, star$kinder == "middle")
large <- subset(star, star$kinder == "large")

## Ignore the missing value by argument na.rm = TRUE
## reading score of 4th grade
sr <- mean(small$g4reading, na.rm = TRUE)
mr <- mean(middle$g4reading, na.rm = TRUE)
lr <- mean(large$g4reading, na.rm = TRUE)

## math score of 4th grade
sm <- mean(small$g4math, na.rm = TRUE)
mm <- mean(middle$g4math, na.rm = TRUE)
lm <- mean(large$g4math, na.rm = TRUE)

## Display & see means of each score.
## Replacing mean by sd, we can get standard deviation of each.
c(sr, mr, lr); c(sm, mm, lm)


## Q3 

srq <- quantile(small$g4reading, 
                probs = seq(0.33, 0.66, 0.33), na.rm = TRUE)
mrq <- quantile(middle$g4reading,
                probs = seq(0.33, 0.66, 0.33), na.rm = TRUE)

smq <- quantile(small$g4math, 
                probs = seq(0.33, 0.66, 0.33), na.rm = TRUE)
mmq <- quantile(middle$g4math,
                probs = seq(0.33, 0.66, 0.33), na.rm = TRUE)

## We can also get them by another way.
## sr33 <- quantile(small$g4reading, probs = 0.33, na.rm = TRUE)
## sr66 <- quantile(small$g4reading, probs = 0.66, na.rm = TRUE)
## 1/3 instead of 0.33 is more acculate.


## Q4

## Make a contingency table.
table(class_size = star$kinder, year = star$yearssmall)

## Function tapply() applies one function repeatedly
## to each level of the factor variable. 
## tapply(x2, x1, mean) means to calculate mean of x2 for x1.

tapply(star$g4reading, star$yearssmall, mean, na.rm = TRUE)
tapply(star$g4reading, star$yearssmall, median, na.rm = TRUE)

tapply(star$g4math, star$yearssmall, mean, na.rm = TRUE)
tapply(star$g4math, star$yearssmall, median, na.rm = TRUE)


## Q5

## White people have higher score in both class size.
tapply(middle$g4reading, middle$race, mean, na.rm = TRUE)
tapply(middle$g4math, middle$race, mean, na.rm = TRUE)

tapply(small$g4reading, small$race, mean, na.rm = TRUE)
tapply(small$g4math, small$race, mean, na.rm = TRUE)


## Q6

tapply(star$hsgrad, star$kinder, mean, na.rm = TRUE)
tapply(star$hsgrad, star$yearssmall, mean, na.rm = TRUE)
tapply(star$hsgrad, star$race, mean, na.rm = TRUE)


## -----------------------------------------------------
## Section 2
## Q1

## Canvasser: –K–âˆõ

gay <- read.csv("gay.csv")
head(gay)
dim(gay)

## Create subset for wave = 1 & study = 1.
wave1 <- subset(gay, gay$study == 1 & gay$wave == 1)
## wave1 <- subset(gay, subset = (study == 1) & (wave == 1)) 
## same as above

sum(gay$ssm) ## no missing value
wave1a <- tapply(wave1$ssm, wave1$treatment, mean)


## Q2

## Create subset for wave = 2 & study = 1.
wave2 <- subset(gay, gay$study == 1 & gay$wave == 2)

## Calculate mean for each of smm.
wave2a <- tapply(wave2$ssm, wave2$treatment, mean)

## Convert wave2a into data.frame for simplicity's sake.
wave1b <- as.data.frame(wave1a)
wave2b <- as.data.frame(wave2a)

## Sample Average Treatment Effect for the Treated for Gay & Straight
## Difference-in-Difference Estimators
DIDgay1 <- (wave2b[4,] - wave1b[4,]) - (wave2b[1,] - wave1b[1,])
DIDstraight1 <- (wave2b[5,] - wave1b[5,]) - (wave2b[1,] - wave1b[1,])

## Maybe this part (wave2b[1,] - wave1b[1,]) is not need?
## Please give me feedback on it :D.

## DID
DIDgay1 - DIDstraight1


## Q3

DIDgay2 <- (wave2b[2,] - wave1b[2,]) - (wave2b[1,] - wave1b[1,])
DIDstraight2 <- (wave2b[3,] - wave1b[3,]) - (wave2b[1,] - wave1b[1,])

## Interpretation: there is no big difference b/w the two
## that means there is a possibility that the bias does not exist. 

DIDgay2 - DIDstraight2


## Q4

## Create subsets for wave = 3:7 & study = 1
wave3 <- subset(gay, gay$study == 1 & gay$wave == 3)
wave4 <- subset(gay, gay$study == 1 & gay$wave == 4)
wave5 <- subset(gay, gay$study == 1 & gay$wave == 5)
wave6 <- subset(gay, gay$study == 1 & gay$wave == 6)
wave7 <- subset(gay, gay$study == 1 & gay$wave == 7)

## Calculate mean for each of smm.
wave3a <- tapply(wave3$ssm, wave3$treatment, mean)
wave4a <- tapply(wave4$ssm, wave4$treatment, mean)
wave5a <- tapply(wave5$ssm, wave5$treatment, mean)
wave6a <- tapply(wave6$ssm, wave6$treatment, mean)
wave7a <- tapply(wave7$ssm, wave7$treatment, mean)

## Convert waves into data.frame for simplicity's sake.
wave3b <- as.data.frame(wave3a)
wave4b <- as.data.frame(wave4a)
wave5b <- as.data.frame(wave5a)
wave6b <- as.data.frame(wave6a)
wave7b <- as.data.frame(wave7a)

## ATE
DIDgay3 <- (wave3b[4,] - wave1b[4,]) - (wave3b[1,] - wave1b[1,])
DIDstraight3 <- (wave3b[5,] - wave1b[5,]) - (wave3b[1,] - wave1b[1,])

DIDgay4 <- (wave4b[4,] - wave1b[4,]) - (wave4b[1,] - wave1b[1,])
DIDstraight4 <- (wave4b[5,] - wave1b[5,]) - (wave4b[1,] - wave1b[1,])

DIDgay5 <- (wave5b[4,] - wave1b[4,]) - (wave5b[1,] - wave1b[1,])
DIDstraight5 <- (wave5b[5,] - wave1b[5,]) - (wave5b[1,] - wave1b[1,])

DIDgay6 <- (wave6b[4,] - wave1b[4,]) - (wave6b[1,] - wave1b[1,])
DIDstraight6 <- (wave6b[5,] - wave1b[5,]) - (wave6b[1,] - wave1b[1,])

DIDgay7 <- (wave7b[4,] - wave1b[4,]) - (wave7b[1,] - wave1b[1,])
DIDstraight7 <- (wave7b[5,] - wave1b[5,]) - (wave7b[1,] - wave1b[1,])

DIDgay3; DIDga4; DIDgay5; DIDga6; DIDgay7
DIDstraight3; DIDstraight4; DIDstraight5; DIDstraight6; DIDstraight7

## DID

DIDgay1 - DIDstraight1
DIDgay3 - DIDstraight3
DIDgay4 - DIDstraight4
DIDgay5 - DIDstraight5
DIDgay6 - DIDstraight6
DIDgay7 - DIDstraight7

## The difference b/w gay & straight canvasser is still positive 
## even in wave 7, and effects seems to be lasted. 


## Q5

## Create subset & calculate means
study2 <- subset(gay, subset = (study == 2) & (wave == 1))
study2a <- tapply(study2$ssm, study2$treatment, mean)
study2b <- as.data.frame(study2a)

## Randomization seems to be done prorerly
## (because of almost no difference b/w two means). 


## Q6

## Create subset & calculate means
study2.2 <- subset(gay, subset = (study == 2) & (wave == 2))
study2.2a <- tapply(study2.2$ssm, study2.2$treatment, mean)
study2.2b <- as.data.frame(study2.2a)

DIDstudy2 <- (study2.2b[4,] - study2b[4, ]) - (study2.2b[1, ] - study2b[1, ])

## Compare the results of study 1 & study 2.
DIDstudy2; DIDgay1

## The difference seems not so big. We can conclude that 
## the result of study 2 of wave 2 is consistent with study 1. 


## Q7


## Create subsets of different waves
study3 <- subset(gay, subset = (study == 2) & (wave == 3))
study4 <- subset(gay, subset = (study == 2) & (wave == 4))
study7 <- subset(gay, subset = (study == 2) & (wave == 7))

## Create means of different waves
study3a <- tapply(study3$ssm, study3$treatment, mean)
study4a <- tapply(study4$ssm, study4$treatment, mean)
study7a <- tapply(study7$ssm, study7$treatment, mean)

## Convert 3 studies into data.frame for simplicity's sake.
study3b <- as.data.frame(study3a)
study4b <- as.data.frame(study4a)
study7b <- as.data.frame(study7a)

diff3 <- (study3b[4, ] - study2b[4, ]) - (study3b[1, ] - study2b[1, ])
diff4 <- (study4b[4, ] - study2b[4, ]) - (study4b[1, ] - study2b[1, ])
diff7 <- (study7b[4, ] - study2b[4, ]) - (study7b[1, ] - study2b[1, ])

diff3; diff4; diff7

## Study 2 also has the positive effects of asking by gay canvasser on
## marriage script. Overall, if gay canvasser asks about marriage, 
## rate of suppor for same-sex marriage become higher. 


## -----------------------------------------------------
## Section 3
## Q1

leader <- read.csv("leaders.csv")

dim(leader)
summary(leader$result)

length(unique(leader$country)) # 88 countries
y <- nrow(leader) # 250 countries in total
z <- length(unique(leader$year)) # n of plots w/o overlaps
y / z # year mean of plots in those 88 countries


## Q2

unique(leader$result)

## Create variable success in the original data frame.
## We use the logical disjunction (|) here 
## because the way to die does not matter. 

leader$success <- NA 
leader$success[leader$result == "dies between a day and a week" 
               | leader$result == "dies between a week and a month" 
               | leader$result == "dies within a day after the attack"
               | leader$result == "dies, timing unknown"
               ] <- 1

leader$success[leader$result == "hospitalization but no permanent disability" 
               | leader$result == "not wounded"
               | leader$result == "plot stopped"
               | leader$result == "survives but wounded severely"
               | leader$result == "survives, whether wounded unknown"
               | leader$result == "wounded lightly"
               ] <- 0

## leader <- leader[, -"sucess"] # Delete wrong variable name

mean(leader$success) # 21.6%

## Success of assassination attempt seems not to be done randomly. 
## Thus, the assumption is not valid. 


## Q3

tapply(leader$politybefore, leader$success, mean)
## The difference b/w the two is about 1.04, seems not to be trivial. 

tapply(leader$age, leader$success, mean)
## Aimed leader among sccuees is 3 years higher than among failure. 


## Q4

## Create a variable warbefore
leader$warbefore <- NA

leader$warbefore[leader$civilwarbefore == 1 
                 | leader$interwarbefore == 1] <- 1

## Notice that we have to use the logical cunjunction (&). 
leader$warbefore[leader$civilwarbefore != 1 
                 & leader$interwarbefore != 1] <- 0

summary(leader$warbefore)

war3b <- subset(leader, leader$warbefore == 1)

tapply(war3b$politybefore, war3b$success, mean)
tapply(war3b$age, war3b$success, mean)

## Among the countries where experience war have higher polity score. 
## Among them, age of ploted leaders are higher among success, diff. is trivial though. 


## Q5

## Create a variable warbefore
leader$warafter <- NA

leader$warafter[leader$civilwarafter == 1
                | leader$interwarafter == 1] <- 1

leader$warafter[leader$civilwarafter != 1
                & leader$interwarafter != 1] <- 0

war3a <- subset(leader, subset = (warafter == 1))

tapply(leader$warafter, leader$success, mean)
tapply(leader$polityafter, leader$success, mean)

## After success the rate of breaking out war is about 20%, 
## while its rate after failure is 30%. 

