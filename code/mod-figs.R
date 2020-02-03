
rm(list = ls())

library(ggplot2)

data <- read.csv("df/fig_df.csv")
head(data)
summary(data)


# removed "All" column
data$risk_f = factor(data$risk, levels = c('High', 'Non-high'))

# legend order
data$age <- factor(data$age, levels = c("0-4 yrs",
                                              "5-19 yrs",
                                              "20-64 yrs",
                                              "65+ yrs"))

f1 <- ggplot(subset(data, scenario %in% "vaxbase"),
             aes(y = plot.cost,
                 x = plot.dalys,
                 group = age, color = age)) +
  geom_point() + expand_limits(y = 0) +
  labs(y = "Cost saved per 100,000 population\n",
       x = "\nDALYs averted per 100,000 population",
       color = "Age group") + theme_bw() +
  # theme(axis.text=element_text(size=10),
  #       axis.title=element_text(size=12,face="bold"),
  #       plot.title = element_text(colour = "grey39"),
  #       #panel.grid.major = element_blank(),
  #       panel.grid.major = element_line(colour = "grey86",size=0.5),### these remove the minor lines
  #       panel.grid.minor = element_line(colour = "grey86",size=0.5))+
  facet_grid(v.eff~risk_f)

f1

ggsave('fig1.png', height = 6, width = 8, dpi = 600)


f2 <- ggplot(subset(data, scenario %in% "vax70" & reference %in% "vaxbase"),
             aes(y = plot.cost,
                 x = plot.dalys,
                 group = age, color = age)) + geom_point() +
  expand_limits(y = 0) +
  labs(y = "Cost saved per 100,000 population\n",
       x = "\nDALYs averted per 100,000 population", color = "Age group") + 
  theme_bw() +
  # theme(axis.text=element_text(size=10),
  #       axis.title=element_text(size=12,face="bold"),
  #       plot.title = element_text(colour = "grey39"),
  #       panel.grid.major = element_blank(),
  #       #panel.grid.major = element_line(colour = "grey86",size=0.5), ### these remove the minor lines
  #       panel.grid.minor = element_blank())+
  facet_grid(v.eff~risk_f)

f2

ggsave('fig2.png', height = 6, width = 8, dpi = 600)

