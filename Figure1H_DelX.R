library(Epi)
twoby2(matrix(c(29, 3685-29, 4, 58), nrow=2))
data <- data.frame(c("PHF6 mutation in female", "Chr X deletion in female", "PHF6 mutations in chr X-preserved female", 
                     "PHF6 mutations in chr X-deleted female"))
colnames(data) <- "Category"
data$Percent <- c(36/3840*100, 62/3747*100, 29/3685*100, 4/62*100)

data <- transform(data, Category=factor(Category, levels=rev(c("PHF6 mutation in female", "Chr X deletion in female", 
                                                           "PHF6 mutations in chr X-preserved female", 
                                                           "PHF6 mutations in chr X-deleted female"))))

library(ggplot2)
g <- ggplot(data,aes(x=Category, y=Percent))
g <- g + geom_bar(stat="identity")
g <- g + xlab("") + ylab("Frequency (%)")
g <- g + scale_y_continuous(expand=c(0,0))
g <- g + theme_classic()
g <- g + coord_flip()
g