# Amber Garner
# 10/10/2016

# Setup 
install.packages("arules")
install.packages("arulesViz")
library("arules", lib.loc="~/R/win-library/3.3")
library("arulesViz", lib.loc="~/R/win-library/3.3")
library("discretization", lib.loc="~/R/win-library/3.3")

# Load dataset
credit <- read.csv("CreditApproval.csv", header = T, sep = ",")

# Get descriptive stats on dataset
head(credit)
str(credit)
summary(credit)

# Remove NA values
credit<-na.omit(credit)

# Summary after NA removed
summary(credit)

# Identify outliers in A2
boxplot.stats(credit$A2, coef = 5)

#Find outliers & remove outliers in A3
boxplot.stats(credit$A3, coef = 5)

#Find outliers & remove outliers in A8
boxplot.stats(credit$A8, coef = 5)
credit <- credit[credit$A8<= 14.415,]

#Find outliers & remove outliers in A11
boxplot.stats(credit$A11, coef = 5)
credit <- credit[credit$A11<= 17,]

#Find outliers & remove outliers in A14
boxplot.stats(credit$A14, coef = 5)
credit <- credit[credit$A14<= 1160,]

#Find outliers & remove outliers in A15
boxplot.stats(credit$A15, coef = 5)
credit <- credit[credit$A15<= 2283,]

# Convert A2 to factor and verify change
credit$A2 <- discretize(credit$A2, "cluster", categories = 4)
summary(credit$A2)

# Convert A3 to factor and verify change
credit$A3 <- discretize(credit$A3, "cluster", categories = 5)
summary(credit$A3)

# Convert A8 to factor and verify change
credit$A8 <- discretize(credit$A8, "cluster", categories = 3)
summary(credit$A8)

# Convert A11 to factor and verify change
credit$A11 <- discretize(credit$A11, "cluster", categories = 4)
summary(credit$A11)

# Convert A14 to factor and verify change
credit$A14 <- discretize(credit$A14, "cluster", categories = 6)
summary(credit$A14)

# Convert A15 to factor and verify change
credit$A15 <- discretize(credit$A15, "cluster", categories = 5)
summary(credit$A15)

# Association rule mining with default 
rules <- apriori(credit)
inspect(rules[1:10])

# Apriori with support = 0.4, confidence=0.7, and minimum length=2
rules <- apriori(credit, parameter = list(supp=0.4, conf=0.7, minlen=2))
inspect(rules[1:10])

# Apriori with support = 0.5, confidence =0.9 and minimum length = 2
rules <- apriori(credit, parameter = list(supp=0.5, conf=0.9, minlen=2))
inspect(rules[1:10])

# Apriori with support = 0.3, confidence =0.8 and minimum length = 2
# Only include rules where right hand side is class=- or class = +
rules<-apriori(credit, parameter= list(supp=0.3, conf=0.8, minlen=2), 
               appearance=list(rhs=c("class=+", "class=-"), default="lhs"))
ispect(rules[1:10])

# Find redudant rules
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# Remove redudant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

# Build scatterplot of pruned rules
plot(rules.pruned)
