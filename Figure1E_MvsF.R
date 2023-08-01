data <- read.table("chrXgene2.txt", header=T, sep="\t")
data <- transform(data, Gene=factor(Gene, levels=c("PHF6", "UBA1", "ZRSR2" , "STAG2", "BCORL1", "BCOR", "ATRX", "PIGA", "KDM6A")))
data <- transform(data, Sex=factor(Sex, levels=c("Male", "Female")))

library(ggplot2)
library(ggsci)
g <- ggplot(data,aes(x=Gene, y=Percent, fill=Sex))
g <- g + geom_bar(stat="identity", position="dodge")
g <- g + xlab("") + ylab("% of male carriers (%)")
g <- g + scale_y_continuous(expand=c(0,0))
g <- g + theme_classic()
g <- g + scale_fill_aaas()
g
