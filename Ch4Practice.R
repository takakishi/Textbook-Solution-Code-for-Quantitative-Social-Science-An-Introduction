## Chapter 4 Prediction
## Exercise Solution

setwd("/users/takaaki/documents/qss/PREDICTION")

## -----------------------------------------------------
## Taka(the author of this script) uses Japanese-Version QSS.
## -----------------------------------------------------
## Section 1
## Q1

intrade08 <- read.csv("intrade08.csv") 
intrade12 <- read.csv("intrade12.csv")
pres08 <- read.csv("pres08.csv") # Election results
pres12 <- read.csv("pres12.csv")

head(intrade08); head(intrade12)
head(pres08); head(pres12) 
dim(intrade08); dim(intrade12)

## 2008

## calculate Obama's mergin
pres08$margin <- pres08$Obama - pres08$McCain # result

## calculate D-Paety's mergin
intrade08$margin <- intrade08$PriceD - intrade08$PriceR

## convert to a Date object
class(intrade08$day)
intrade08$day <- as.Date(intrade08$day)
class(intrade08$day) # object is changed to "date"

unique(intrade08$day)
(unique(intrade08$state))

poll.pred08 <- rep(NA, 51)
poll.pred08 # this is just an empty vector
## extract unique state names which the loop will iterate through 
st.names <- unique(intrade08$state)

## add state names as labels for easy interpretation later on
names(poll.pred08) <- as.character(st.names) # element name is added 

## loop across 50 states plus DC
for (i in 1:51){
  ## subset the 2008 data
  before <- subset(intrade08, intrade08$day == "2008-11-03")
  before <- subset(before, subset = (state == st.names[i]))
  poll.pred08[i] <- before$margin
}

## predicted winners
poll.pred08

## which state prediction called wrong? 
sign(poll.pred08) != sign(pres08$margin)
pres08$state[sign(poll.pred08) != sign(pres08$margin)]
## what was the actual margin for these states? 
pres08$margin[sign(poll.pred08) != sign(pres08$margin)]

## prediction using poll is more accurate than using market price of gambling

## 2012
## convert to a Date object
intrade12$day <- as.Date(intrade12$day)

## calculate Obama's mergin
pres12$margin <- pres12$Obama - pres12$Romney # result

## calculate D-Paety's mergin
intrade12$margin <- intrade12$PriceD - intrade12$PriceR

poll.pred12 <- rep(NA, 50)

## extract unique state names which the loop will iterate through 
st.names <- unique(intrade12$state)

length(unique(intrade12$state))

## add state names as labels for easy interpretation later on
names(poll.pred12) <- as.character(st.names) # element name is added 

length(poll.pred12)

## loop across 50 states 
for (i in 1:50){
  ## subset the 2012 data
  before <- subset(intrade12, intrade12$day == "2012-11-05")
  before <- subset(before, subset = (state == st.names[i]))
  poll.pred12[i] <- before$margin
}

## predicted winners
poll.pred12

## replace NA in predicted winners by 1
poll.pred12 <- replace(poll.pred12, which(is.na(poll.pred12)), 1)

## because their lengths are different -
## unique(intrade12$state) does not contain DC - 
## we need to omit DC from pres12$margin in order to arrange the lengths
length(pres12$margin); length(poll.pred12); length(unique(intrade12$state))
names(poll.pred08) %in% names(poll.pred12) # find place where state is different
poll.pred08[8]; poll.pred12[8]
poll.pred08[9]; poll.pred12[9] # 9th is DC in intrade12$state

## which state prediction called wrong? 
sign(poll.pred12) != sign(pres12$margin[-9])
pres12$state[sign(poll.pred12) != sign(pres12$margin[-9])]
## what was the actual margin for these states? 
pres12$margin[sign(poll.pred12) != sign(pres12$margin[-9])]

## prediction using poll is still more accurate 
## than using market price of gambling. however, compared to Q1, 
## gambling in 2012 gives better prediction (due to lack of competition) 


## Q2

