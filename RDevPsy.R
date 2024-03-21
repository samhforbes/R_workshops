library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)

2+2

var <- c(1,2,3,4,5,6)
length(var)
plot(var)

var <- as.character(var)
typeof(var)
var

var[4]
y <- var[2:4]
y

name <- "Sam"
age <- 35
today <- Sys.Date()
birthday <- as.Date("2024-12-07")

## Some Rules
# * Data needs to be *tidy*. This means:
#   - Each row is an *observation*
#   - Each variable is a *column*
#   - Data should be in a *single dataset*

x <- 5
x
x < 2
x == 5

y <- 1:10
y/x
y*x
y^x
#comments



level <- c('Undergrad', 'Graduate', 'Faculty')
males <- c(7, 2, 1)
females <- c(1, 5, 1)
lab <- data.frame(level, males, females)



lab2 <- pivot_longer(lab, 
                     cols = c(males, females), 
                     names_to = 'gender', 
                     values_to = 'number')

#go back
lab_short <- pivot_wider(lab2, 
                         names_from = gender, 
                         values_from = number)

# reading in data
choc <- read_csv('choccies.csv')
summary(choc)

#select
ctwo <- select(choc, boost, bounty)
#multiple
ctwo <- select(choc, Group:bounty)

# if we only want groups who rate boost bars
cfil <- filter(choc, boost >= .5)

# arrange
carr <- arrange(choc, desc(boost))

# new columns with a weighted score
cmut <- mutate(choc, 
               weighting = (boost + lion)/2)
cmut <- arrange(cmut, desc(boost))

# renaming

# putting it together
Group <- choc$Group
n <- c(7, 12, 6)

choc_dem <- data.frame(Group, n)

choc_with_dem <- full_join(choc, choc_dem)

# back to pivoting
choc_long <- pivot_longer(choc, cols = c(2:8), names_to = 'chocolate', values_to = 'score')
  
# rename
choc_renamed <- rename(choc_long, 
                       Choccy = chocolate,
                       Prop = score)

# piping - try it



# other syntax
cfil <- choc[choc$boost >= 0.5]
names(choc_long)[c(2,3)] <- c('Choccy', 'Prop')

# looking at data

boxplot(Prop ~ Choccy, data = choc_renamed)

library(ggplot2)
cplot <- ggplot(data = choc_renamed, aes(x = Choccy, y = Prop))

cplot + geom_boxplot()

# don't need to save it
ggplot(data = choc_renamed,
       aes(x = Group, y = Prop)) +
  geom_boxplot()

# point
ggplot(data = choc_renamed,
       aes(x = Group, y = Prop)) + 
  geom_boxplot() +
  stat_summary(aes(colour = Choccy, shape = Choccy),
                     geom = 'point', fun = 'mean')

ggplot(data = choc_renamed,
       aes(x = Group, y = Prop)) + 
  geom_boxplot() +
  stat_summary(aes(colour = Choccy, shape = Choccy),
                     geom = 'point', fun.y = 'mean',
                     size = 4, position = position_jitter())


#themes etc

ggplot(data = choc_renamed,
       aes(x = Group, y = Prop, colour = Group, fill = Group)) +
  geom_violin(alpha = 0.2) + 
  stat_summary(geom = 'pointrange', fun.data = 'mean_se') + 
  theme_classic() +
  ylab('Proportion') +
  xlab('Research Group')
