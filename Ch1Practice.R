## Chapter 1 Introduction
## Exercise Solution

setwd("/users/takaaki/documents/qss/INTRO")

## -----------------------------------------------------
## Taka(the author of this script) uses Japanese-Version QSS.
## -----------------------------------------------------
## Section 1
## Q1

## Read the turnout data file. 
turn <- read.csv("turnout.csv")

dim(turn)
head(turn)
summary(turn)
turn$year # 1980 
length(turn) # length of a vector


## Q2 

## Add overseas electorate to VAP.
## electorate: 有権�?
turn$electorate <- turn$VAP + turn$overseas

## Calculate voter turnout of VAP. 
vap <- (turn$total / turn$electorate) * 100

## Calculate voter turnout of VEP. 
vep <- (turn$total / turn$VEP) * 100

vap; vep
mean(vap); mean(vep) # The diff. b/w the two seems to be trivial. 


## Q3

d1 <- turn$ANES - vap
d2 <- turn$ANES - vep

mean(d1); mean(d2) 
range(d1); range(d2) # range of the difference


## Q4

turn$year
## 1980, '84, '88, '92, '96, '00, '04, '08: presidential election
## 1982, '86, '90, '94, '98, '02,         : midterm election

pe <- turn[seq(1, 14, 2), ] # create presidential election
pe[8, ] <- turn[14, ]       # add year 2008
me <- turn[seq(2, 12, 2), ] # create midterm election

vepP <- (pe$total / pe$VEP) * 100 # voter turnout of VEP predident
vepM <- (me$total / me$VEP) * 100 # voter turnout of VEP midterm

pe$ANES # voter turnout of ANES president
me$ANES # voter turnout of ANES midterm

## Big difference b/w presidential & midterm elec, in ANES

## Create difference. 
diff1 <- mean(pe$ANES) - mean(vepP) # VEP vs. ANES in presidential election
diff2 <- mean(me$ANES) - mean(vepM) # VEP vs. ANES in midterm election

diff1 - diff2

## Estimation of (ANES - VEP) is 2.46% higher in presidential election. 
## Voter turnout is higher in presidential election. 


## Q5

old <- turn[c(1:7), ]  # Extract first hald of data (up to 1992).
## old <- turn[seq(1, 7), ] # Do same thing as shown above. 
new <- turn[c(8:14), ] # Extract second hald of data (from '94 to the end). 

VEPo <- (old$total / old$VEP) * 100
VEPn <- (new$total / new$VEP) * 100
ANESo <- old$ANES
ANESn <- new$ANES

mean(ANESo) - mean(VEPo)
mean(ANESn) - mean(VEPn)

## In terms of the mean of two periods, the ANES bias increased. 


## Q6

## Subtract felons & noncitisen from VAP.
VAP1 <- turn$VAP - turn$felons - turn$noncit

## Extract the year 2008
turn08 <- turn[turn$year==2008, ] 
## turn08 <- turn[14, ] ## same as above 

## Subtract osvoters from total in 2008.
turn$total[14] <- turn08$total - turn08$osvoters

## adjusted voter turnout VAP
adjVAP <- turn$total / VAP1

mean(adjVAP)
mean(vap)
mean(vep)
mean(turn$ANES)


## -----------------------------------------------------
## Section 2
## Q1

kenya <- read.csv("Kenya.csv")
sweden <- read.csv("Sweden.csv")
world <- read.csv("World.csv")

dim(kenya); dim(sweden); dim(world)

## Create sum of person-year.
kenya$py <- kenya$py.men + kenya$py.women
sweden$py <- sweden$py.men + sweden$py.women
world$py <- world$py.men + world$py.women

## We will create CBRs in 3 ways.
## Create Kenya's CBR. 
kc1 <- sum(kenya[1:15, "births"]) / sum(kenya[1:15, "py"])
kc2 <- sum(kenya[16:30, "births"]) / sum(kenya[16:30, "py"])
kCBR <- c(kc1, kc2)

## Create Sweden's CBR. 
sc1 <- sum(sweden[c(1:15), "births"]) / sum(sweden[c(1:15), "py"])
sc2 <- sum(sweden[c(16:30), "births"]) / sum(sweden[c(16:30), "py"])
sCBR <- c(sc1, sc2)

## Create World's CBR.
## First, we create & add CBR in the data.frame.
world$crb <- world$births / world$py

## Divide data into two periods.  
wp1 <- world[world$period=="1950-1955", ] # first half
wp2 <- world[world$period=="2005-2010", ] # second half

## Calculate CBRs
wc1 <- sum(wp1$births) / sum(wp1$py)
wc2 <- sum(wp2$births) / sum(wp2$py)

## Save CBR as a new vector which has length=2
wCBR <- c(wc1, wc2)

## Print 3 countries' CBRs
kCBR; sCBR; wCBR


## Q2

## We will create ASFR in the different ways again.
## childbearing age: CBA
## Kenya
## 1950-55
Kcba1 <- kenya[4:10, ]
K.ASFR1 <- Kcba1$births / Kcba1$py.women

## 2005-10
Kcba2 <- kenya[19:25, ]
K.ASFR2 <- Kcba2$births / Kcba2$py.women

## total
## K.ASFR <- c(K.ASFR1, K.ASFR2) # easiest way
Kcba3 <- rbind(kenya[4:10, ], kenya[19:25, ])
K.ASFR <- Kcba3$births / Kcba3$py.women
 
## Sweden
## 1950-55
Scbab1 <- sweden[4:10, "births"]
Scbaw1 <- sweden[4:10, "py.women"]
S.ASFR1 <- Scbab1 / Scbaw1

## 2005-10
Scbaw2 <- sweden[19:25, "py.women"]
Scbab2 <- sweden[19:25, "births"]
S.ASFR2 <- Scbab2 / Scbaw2

## total
Scbab <- c(Scbab1, Scbab2)
Scbaw <- c(Scbaw1, Scbaw2)
S.ASFR <- Scbab / Scbaw

## World
## 1950-55
W.ASFR <- world$births / world$py.women
W.ASFR1 <- W.ASFR[4:10]

## 2005-10
W.ASFR2 <- W.ASFR[c(19:25)]

## total
W.ASFR <- W.ASFR[c(4,5,6,7,8,9,10,19,20,21,22,23,24,25)]

## kenya[kenya$age == "15-19", ]

K.ASFR1; S.ASFR1; W.ASFR1
mean(K.ASFR1); mean(S.ASFR1); mean(W.ASFR1)

K.ASFR2; S.ASFR2; W.ASFR2
mean(K.ASFR2); mean(S.ASFR2); mean(W.ASFR2)

K.ASFR; S.ASFR; W.ASFR
mean(K.ASFR); mean(S.ASFR); mean(W.ASFR)


## Q3

## Kenya's TFR
K.TFR <- c(sum(K.ASFR1 * 5), sum(K.ASFR2 * 5))
## the number of women
kw1 <- sum(kenya[kenya$period=="1950-1955", "py.women"]) # first half
kw2 <- sum(kenya[kenya$period=="2005-2010", "py.women"]) # second half

## Sweden's TFR
S.TFR <- c(sum(S.ASFR1 * 5), sum(S.ASFR2 * 5))
## the number of women
sw1 <- sum(sweden[sweden$period=="1950-1955", "py.women"]) # first half
sw2 <- sum(sweden[sweden$period=="2005-2010", "py.women"]) # second half

## World's TFR
W.TFR <- c(sum(W.ASFR1 * 5), sum(W.ASFR2 * 5))
## the number of women
ww1 <- sum(world[world$period=="1950-1955", "py.women"]) # first half
ww2 <- sum(world[world$period=="2005-2010", "py.women"]) # second half
## the number of births
wb1 <- sum(world[world$period=="1950-1955", "births"]) # first half
wb2 <- sum(world[world$period=="2005-2010", "births"]) # second half

## print TFR & #women
K.TFR; S.TFR; W.TFR
kw1; kw2; sw1; sw2; ww1; ww2
wb1; wb2


## Q4

## Kenya's CDR
K.CDR1 <-  sum(kenya[kenya$period=="1950-1955", "deaths"]) / 
  sum(kenya[kenya$period=="1950-1955", "py"]) 

K.CDR2 <-  sum(kenya[kenya$period=="2005-2010", "deaths"]) / 
  sum(kenya[kenya$period=="2005-2010", "py"]) 

K.CDR <- c(K.CDR1, K.CDR2)

## Sweden's CDR
S.CDR1 <- sum(sweden[1:15, "deaths"]) / sum(sweden[1:15, "py"])
S.CDR2 <- sum(sweden[16:30, "deaths"]) / sum(sweden[16:30, "py"])

S.CDR <- c(S.CDR1, S.CDR2)

## World's CDR
W.CDR1 <- sum(world[c(1:15), "deaths"]) / sum(world[c(1:15), "py"])
W.CDR2 <- sum(world[c(16:30), "deaths"]) / sum(world[c(16:30), "py"])

W.CDR <- c(W.CDR1, W.CDR2)

## Print CDRs. 
K.CDR; S.CDR; W.CDR


## Q5

K.ASDR <- kenya[16:30, "deaths"] / kenya[16:30, "py"]
S.ASDR <- sweden[16:30, "deaths"] / sweden[16:30, "py"]

options(scipen = 1) # avoid exponential notation
K.ASDR; S.ASDR
S.ASDR - K.ASDR

## In the every age class Kenya's ASDR is higher. 


## Q5

## rate of Sweden's each age class, P
sp <- sweden$py[16:30] / sum(sweden$py[16:30])

## Kenya's counterfactual CDR
cfK.CDR <- sum(K.ASDR * sp) # result

## Compare the two
K.CDR[2]; cfK.CDR # result: 0.01038914 0.02321646
K.CDR[2] - cfK.CDR

## This means if Kenya has Sweden's population distribution, 
## CDR will be 1 percent point higher than factual CDR. 
## Thus, the comparison of factual CDRs b/w Kenya & Sweden
## does not necessarily give a meaningful result. 
