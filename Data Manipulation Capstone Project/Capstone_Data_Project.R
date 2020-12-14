# MoneyBall Project
# Use baseball statistics from Sean Lahaman's website to help the Oakland A's find the under-valued replacement players

# Read the data
batting <- read.csv('Batting.csv')
# head(batting)
# str(batting)
# head(batting$AB)
# head(batting$X2B)

# Feature Engineering
# We will add three more statistics based off existing metrics:
# 1. Batting Average = H / AB , where H(Hits) and AB(At Base)
# 2. On Base Percentage = (H+BB+HBP) / (AB+BB+HBP+SF), where H(Hits), BB(Bases on Balls, i.e. Walks), HBP(Hit By Pitch), AB(At Bat), SF(Sacrifice Fly)
# 3. Slugging Percentage = ((1B) + (2*2B) + (3*3B)+(4*HR)) / AB, where 1B(# Singles), 2B(# Doubles), 3B(# Triples), HR(# Home Runs)

batting$BA <- batting$H / batting$AB
batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF) 
# Creating X1B(Singles):
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR
batting$SLG <- (batting$X1B + 2*batting$X2B + 3*batting$X3B + 4*batting$HR) / batting$AB

# tail(batting$BA,5)
# tail(batting$OBP,5)
# tail(batting$SLG,5)
# str(batting)


# Merging Salary Data with Batting Data
sal <- read.csv('Salaries.csv')
# summary(sal)
# summary(batting)
# The salary data starts at 1985, while batting data goes back to 1871
# Remove the batting data that occured before 1985:
batting <- subset(batting,yearID>=1985)
# summary(batting)

# Merge the sal and batting data frames by playerID and yearID
combo <- merge(batting,sal,by=c('playerID','yearID'))
# summary(combo)


# Analyzing the Lost Players:
# The 3 players the Oakland A's lost were:
# 1. first baseman 2000 AL MVP Jaon Giambi (giambja01) to the New York Yankees
# 2. outfielder Johnny Damon (damonjo01) to the Boston Red Sox
# 3. infielder Rainer Gustavo "Ray" Olmedo(saenzol01)

# Create a data frame from the combo consisting of these 3 lost players
lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01'))
# lost_players

# Since these 3 players were lost after 2001 in the off season, we will only look into the data from 2001
lost_players <- subset(lost_players,yearID==2001)
# lost_players

# Reduce the lost_players data frame to the columns of concerns
# playerID,H,X2N,X3B,HR,OBP,SLG,BA,AB
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
# lost_players


# Finding Replacement Players:
# Three constraints in finding replacement players for the three lost palyers:
# 1. Total combined salary of the 3 players <= $15 million
# 2. Their combined number of AB(At Bats) >= that of the lost players
# 3. Their mean OBP >= that of the lost players

sum(lost_players$AB) # 1469 in total, 490 on average
mean(lost_players$OBP) # 0.36 on average

# Get candidates players from 2001
library(dplyr)
cand.players <- filter(combo,yearID==2001)
cand.players <- filter(cand.players,!(playerID %in% c('giambja01','damonjo01','saenzol01')))

# Plot the salary vs OBP to see where to cut off salary:
library(ggplot2)
ggplot(cand.players,aes(x=OBP,y=salary)) + geom_point()

# From the plot it seems that we can cut off salary at ~10M
# And we can cut off the AB at 500 (average of lost players were 490)
cand.players <- filter(cand.players,salary<10000000,OBP>0,AB>=500) 

# Select the columns of interest, sort by OBP and see the top 20 candidates:
top.cand <- head(arrange(cand.players,desc(OBP)),20)[,c('playerID','OBP','AB','salary')]
print(top.cand)

# The top 3 fit our constraints perfectly, so we will go ahead with them:
print(top.cand[1:3,])

