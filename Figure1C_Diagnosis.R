library(ggplot2)
library(ggsci)
data <- as.data.frame(c("AML", "AML", "AML", "MDS", "MDS", "MDS", "MDS/MPN", "MDS/MPN", "MDS/MPN", "MPN", "MPN", "MPN"))
colnames(data) <- "Dx"
data$Sex <- c("All", "Male", "Female", "All", "Male", "Female", "All", "Male", "Female", "All", "Male", "Female")
data$Percent <- c(1.61, 2.34, 0.76, 2.15, 2.59, 1.54, 3.43, 3.90, 2.61, 1.22, 0.85, 1.57)

data <- transform(data, Dx=factor(Dx, levels=c("AML", "MDS", "MDS/MPN", "MPN")))
data <- transform(data, Sex=factor(Sex, levels=c("All", "Male", "Female")))

g <- ggplot(data, aes(x=Sex, y=Percent, fill=Dx))
g <- g + geom_bar(stat="identity")
g <- g + xlab("") + ylab("Frequency of PHF6 mutations (%)")
g <- g + scale_y_continuous(expand=c(0,0))
g <- g + theme_classic()
g <- g + scale_fill_nejm()
g <- g + facet_grid(. ~ Dx, scales="free")
g