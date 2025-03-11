#open necessary packages
library(psych)
library(pracma)

#open csv file
banknote <- read.csv("banknote.csv")

#view statistical summary
summary(banknote)

#number of columns
ncol(banknote)
#number of rows
nrow(banknote)

#print column names
colnames(banknote)

#change column names
colnames(banknote) <- c("variance", "skewness", "kurtosis", "entropy", "class")
#verify column name changes
colnames(banknote)

#check for missing values
which(is.na(banknote))
sum(is.na(banknote))

#view structure of data
str(banknote)

#examine correlations
cor(banknote)
#plot the correlations
pairs.panels(banknote, gap = 0, pch = 21)

#rotation

set.seed(5623) #for randomization
pcp <- prcomp(banknote, center = T, scale = T)
summary(pcp)
plot(pcp)
attributes(pcp)
pcp$rotation

#plot rotation
pairs.panels(pcp$x,
             gap=0,
             bg = c("red", "blue"),
             pch = 21)

#scree plot to determine the number of cluster and where to prune
screeplot(pcp, type="line", main="Scree Plot")

#prune PCA
banknote_rotated <- psych::principal(banknote, rotate = "varimax", nfactors = 2,
                                     scores = TRUE)
#view data after pruning
banknote_rotated

#checking if the two components extracted was a good number based on amount of
#variance explained in each variable as represented by the 
#communalities (h2) after extraction

names(banknote_rotated) #viewing all possible contents

#Using Kaiser's criterion in two ways:
#1) when there are fewer than 30 variables and after extraction the communalities
#(represented by h2 in the pattern matrix) are greater than .70;
#h2 loadings show the above to be true
#2) when sample size is greater than 250 and the average communality is > .60
#we need to take the mean of the communality to to test this
mean(banknote_rotated$communality) #mean is 0.81, so this is also true

#Kaiser's criterion shows extraction of 2 components to be a good choice

#biplot the rotated data
biplot(banknote_rotated)
biplot(banknote_rotated,
       col = c('blue', 'red'),
       labels=NULL,
       cex=c(.75,1),
       xlim.s = c(-2.5, 2.5),
       ylim.s = c(-2.5, 2),
       xlim.f=c(-1,1),
       ylim.f=c(-1,1),
       main = 'Biplot from fa')
loadings(banknote_rotated)

biplot(pcp)
biplot(pcp,
       col = c('blue', 'red'),
       xlim = c(-.06, .08),
       main = 'PCA Results',
       xlab = 'First Component',
       ylab = 'Second Component',
       expand = .9)