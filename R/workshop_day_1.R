library(tidyverse)

# library(readr) #for reading data
# library(tidyr)
# library(dplyr) # for manipulating data
# library(forcats)
# library(stringr) 
# library(ggplot2)

# create a variable ------------------------
var <- c(1, 2, 3, 4, 5, 6) # from 1 to 6
var <- c(1:6)

typeof(var)

var2 <- as.character(var)
typeof(var2)


name <- "Sam"
age <- 35
today <- Sys.Date()
birthday <- as.Date("2024-12-07")

x <- 5
x

x < 2
x == 5
x == 12

y <- c(1:10)
b <- y/x
y*x
y^x
y + x

# data frames
level <- c('Undergrad', 'Graduate', 'Faculty')
males <- c(7, 2, 1)
females <- c(1, 5, 1)

lab <- data.frame(level, males, females)


lab2 <- pivot_longer(data = lab, 
                     cols = c(males, females), 
                     names_to = 'gender', 
                     values_to = 'number')

#go back
lab_short <- pivot_wider(lab2, 
                         names_from = gender, 
                         values_from = number)

# read new data ---------------------------------------------

choc <- read_csv('data/choccies.csv')
summary(choc)

#select
# ctwo <- select(choc, -boost, -bounty)
#multiple
ctwo <- select(choc, Group:bounty)

# c_select <- select(choc, starts_with())

cfil <- filter(choc, 
               boost >= .5,
               !is.na(bounty))

#arrange
carr <- arrange(choc, desc(boost))

# mutate
cmut <- mutate(choc, 
               weighting = (boost + lion)/2,
               subcode = substr(Group, 1, 3))

cmut <- arrange(cmut, desc(boost))

# combining
Group <- choc$Group
n <- c(7, 12, 6)
choc_dem <- data.frame(Group, n)

# choc_with_dem <- full_join(choc, choc_dem, by = 'Group')
choc_with_dem <- inner_join(choc, choc_dem)

choc_long <- pivot_longer(choc, 
                          cols = c(2:8), 
                          names_to = 'chocolate', 
                          values_to = 'score')

choc_renamed <- rename(choc_long, 
                       Choccy = chocolate,
                       Prop = score)

choc_final <- choc %>% 
  pivot_longer(cols = c(2:8), 
               names_to = 'chocolate', 
               values_to = 'score') %>% 
  rename(Choccy = chocolate,
         Prop = score)

#other syntax
# cfil <- choc[choc$boost >= 0.5]
# 
# names(choc_long)[c(2,3)] <- c('Choccy', 'Prop')
# choc$weighting <- khsdfgzdfjkhgark.

# plotting ----------------------------
boxplot(Prop ~ Choccy, data = choc_final)

hist(choc_final$Prop)

cplot <- ggplot(data = choc_final, aes(x = Choccy, y = Prop))  
cplot

cplot + geom_boxplot()

ggplot(data = choc_renamed,
       aes(x = Group, y = Prop)) +
  geom_boxplot()

ggplot(data = choc_renamed,
       aes(x = Group, y = Prop, colour = Group, fill = Group)) + 
  geom_violin(alpha = 0.1) +
  geom_boxplot(alpha = 0.2) + #transparency
  stat_summary(geom = 'pointrange', fun.data = 'mean_se', size = 1) +
  theme_classic() +
  theme(legend.position = 'none')

x <- rnorm(100, 0 , 1)
y <- rnorm(100, mean = 2, sd = 1.5)

data <- data.frame(x, y)

plot <- data %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.7) +
  stat_smooth(method = 'lm', se = T, colour = 'red', fill = 'red') +
  theme_classic() +
  labs(x = 'a normal distribution',
       y = 'another distribution')

library(plotly)

ggplotly(plot) 

# testing ------------------------------

cor.test(data$x, data$y)
test1 <- lm(y ~ x, data = data)
summary(test1)

# sleep ------------------------------------
sleep <- sleep

t.test(extra ~ group, paired = T, data = sleep)

# choccies
test3 <- lm(Prop ~ Group, data = choc_final)
summary(test3)

library(car)
Anova(test3, type = 3)

chisq.test()
wilcox.test()

