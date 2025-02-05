library(tidyverse)
library(opendatatoronto)
library(ggplot2)
library(patchwork)

df <- search_packages("Neighbourhood Crime Rates")
df <- df %>% list_package_resources()
df <- df %>% get_resource()

df <- drop_na(df)
df <- select(df, Neighbourhood, Population, Assault_2016, AutoTheft_2016, BreakandEnter_2016, Homicide_2016, Robbery_2016, TheftOver_2016)

p1 <- ggplot(df, aes(Population, Assault_2016)) + geom_point(size = 0.75, col = "#390099") + ggtitle("Scatterplot showing Population \n and Number of Assaults") + theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5), axis.title = element_text(size = 6.75)) +labs(x = "Population in 2016", y = "Number of Assaults in 2016")
p2 <- ggplot(df, aes(Population, AutoTheft_2016)) + geom_point(size = 0.75,col = "#9E0059") + ggtitle("Scatterplot showing Population \n and Number of Auto Thefts") + theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5), axis.title = element_text(size = 6.75)) + xlab("Population in 2016") + ylab("Number of Auto Thefts in 2016")
p3 <- ggplot(df, aes(Population, BreakandEnter_2016)) + geom_point(size = 0.75, col = "#FF0054") + ggtitle("Scatterplot showing Population \n and Number of Break-and-Enters") + theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5), axis.title = element_text(size = 6.75)) + xlab("Population in 2016") + ylab("Number of Break-and-Enters in 2016") 
p4 <- ggplot(df, aes(Population, Homicide_2016)) + geom_point(size = 0.75, col = "#FF5400") + ggtitle("Scatterplot showing Population \n and Number of Homicides") + theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5), axis.title = element_text(size = 6.75)) + xlab("Population in 2016") + ylab("Number of Homicides in 2016")
p5 <- ggplot(df, aes(Population, Robbery_2016)) + geom_point(size = 0.75, col = "#FFBD00") + ggtitle("Scatterplot showing Population \n and Number of Robberies") + theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5), axis.title = element_text(size = 6.75)) + xlab("Population in 2016") + ylab("Number of Robberies in 2016") 
p6 <- ggplot(df, aes(Population, TheftOver_2016)) + geom_point(size = 0.75, col = "#06D6A0") + ggtitle("Scatterplot showing Population \n and Number of Thefts") + theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5), axis.title = element_text(size = 6.75)) + xlab("Population in 2016") + ylab("Number of Thefts in 2016")

(p1 | p2 | p3) /
  (p4 | p5 | p6)

coef(lm(Assault_2016 ~ Population, data = df))
coef(lm(AutoTheft_2016 ~ Population, data = df))
coef(lm(BreakandEnter_2016 ~ Population, data = df))
coef(lm(Robbery_2016 ~ Population, data = df))
coef(lm(TheftOver_2016 ~ Population, data = df))

r1 <- ggplot(df, aes(Population, Assault_2016)) + geom_point(size = 0.75) + stat_smooth(method = lm, col = "#390099", fill = "#390099") + ggtitle("Relationship between Population \n and Number of Assaults") + theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5), axis.title = element_text(size = 6.75)) +labs(x = "Population in 2016", y = "Number of Assaults in 2016")
r2 <- ggplot(df, aes(Population, AutoTheft_2016)) + geom_point(size = 0.75) + stat_smooth(method = lm, col = "#9E0059", fill = "#9E0059") + ggtitle("Relationship between Population \n and Number of Auto Thefts") + theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5), axis.title = element_text(size = 6.75)) + xlab("Population in 2016") + ylab("Number of Auto Thefts in 2016")
r3 <- ggplot(df, aes(Population, BreakandEnter_2016)) + geom_point(size = 0.75) + stat_smooth(method = lm, col = "#FF0054", fill = "#FF0054") + ggtitle("Relationship between Population \n and Number of Break-and-Enters") + theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5), axis.title = element_text(size = 6.75)) + xlab("Population in 2016") + ylab("Number of Break-and-Enters in 2016") 
r4 <- ggplot(df, aes(Population, Robbery_2016)) + geom_point(size = 0.75) + stat_smooth(method = lm, col = "#FFBD00", fill = "#FFBD00") + ggtitle("Relationship between Population \n and Number of Robberies") + theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5), axis.title = element_text(size = 6.75)) + xlab("Population in 2016") + ylab("Number of Robberies in 2016") 
r5 <- ggplot(df, aes(Population, TheftOver_2016)) + geom_point(size = 0.75) + stat_smooth(method = lm, col = "#06D6A0", fill = "#06D6A0") + ggtitle("Relationship between Population \n and Number of Thefts") + theme(plot.title = element_text(size = 7, face = "bold", hjust = 0.5), axis.title = element_text(size = 6.75)) + xlab("Population in 2016") + ylab("Number of Thefts in 2016")

(r1 | r2 | r3) /
  (r4 + r5 + plot_spacer())