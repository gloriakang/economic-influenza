rm(list = ls(all.names = TRUE))
library(ggplot2)
library(tidyr)

#points <- data.frame(label = c("Low", "Mid", "High"),
#                     lbound = c( 0, 0.67, 1.64),
#                     ubound = c(0.674, 1.64, 2.33))


df <- read.csv("tables/icer-vaxbase-40.csv")
df1 <- df[1:12,]

# bar plot
ggplot(df1, aes(x = Age, y = icer.case, fill = Risk)) + geom_bar(stat = "identity", position = "dodge")
ggplot(df1, aes(x = Age, y = icer.death, fill = Risk)) + geom_bar(stat = "identity", position = "dodge")
ggplot(df1, aes(x = Age, y = icer.daly, fill = Risk)) + geom_bar(stat = "identity", position = "dodge")

# line plot
ggplot(df, aes(x = Age, y = icer.case, color = Risk, group = Risk)) + geom_point() + geom_line(linetype = "dotted")
ggplot(df, aes(x = Age, y = icer.death, color = Risk, group = Risk)) + geom_point() + geom_line(linetype = "dotted")
ggplot(df, aes(x = Age, y = icer.daly, color = Risk, group = Risk)) + geom_point() + geom_line(linetype = "dotted")


# violin plot


# dalys averted per 100K by age
ggplot(df1, aes(x = Age, y = dalys.averted.rate, fill = Risk)) + geom_bar(stat = "identity", position = "dodge")


# scatter plot: dalys averted per 100K by cost difference
ggplot(df, aes(x=cost.diff, y=dalys.averted.rate, shape = Risk, color = Age)) + geom_point()



