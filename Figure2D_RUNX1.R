data <- as.data.frame(c("AMLm", "sAMLm", "sAML with RUNX1mtm",
                        "AMLf", "sAMLf", "sAML with RUNX1mtf", "sAML with DelX", "sAML with RUNX1mt and delX"))
data$Percent <- c(2.341857, 3.693182, 9.219858,
                  0.756859, 1.507538, 4.83871, 20, 100)

data$Sex <- c("M", "M", "M", "F", "F", "F", "F", "F")

colnames(data) <- c("Category", "Percent", "Sex")
data <- transform(data, Category=factor(Category, levels=rev(c("AMLm", "sAMLm", "sAML with RUNX1mtm",
                                                               "AMLf", "sAMLf", "sAML with RUNX1mtf", "sAML with DelX", "sAML with RUNX1mt and delX"))))

g <- ggplot(data, aes(x=Category, y=Percent, fill=Sex))
g <- g + geom_bar(stat="identity")
g <- g + xlab("") + ylab("Frequency of PHF6 mutations (%)")
g <- g + scale_y_continuous(expand=c(0,0))
g <- g + theme_classic()
g <- g + scale_fill_manual(values=c("#BC3C29FF", "#0072B5FF"))
#g <- g + geom_signif(comparisons=list(c("Male", "Female")), annotation=c("***"))
g <- g + coord_flip()
#g <- g + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.4))
g

