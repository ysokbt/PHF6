data <- c("0_20", "20_40", "40_60", "60_80", "80_")
data <- as.data.frame(data)
colnames(data) <- "group"
data$percent <- c(1/63*100, 4/577*100, 24/1716*100, 69/4090*100, 11/671*100)
data$color <- c(1, 1, 1, 1, 1)
data$color <- as.factor(data$color)

library(ggplot2)
library(ggsci)
g <- ggplot(data, aes(x=group, y=percent, fill=color))
g <- g + geom_bar(stat="identity")
g <- g + xlab("") + ylab("Frequency of PHF6MT (%)")
g <- g + scale_y_continuous(expand=c(0,0))
g <- g + scale_fill_nejm()
g <- g + theme_classic()
g <- g + theme(legend.position="none")
g

scale_fill_nejm()(1)
