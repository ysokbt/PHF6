library(ggcorrplot)
library(ggplot2)

#Both
cor.both <- read.table("cor_both.txt", header=T, row.names=1, sep="\t")
p.both <- read.table("p_both.txt", header=T, row.names=1, sep="\t")
g <- ggcorrplot(cor.both, method="circle", p.mat=p.both, sig.level=0.05, insig="blank")
g <- g + scale_fill_gradient2(limit=c(0.1, 10), trans="log" ,high="darkred", mid="white", low="darkblue", midpoint = 0.1,
                              breaks=c(0.1, 1, 10))
g


#Male
cor.male <- read.table("cor_male.txt", header=T, row.names=1, sep="\t")
p.male <- read.table("p_male.txt", header=T, row.names=1, sep="\t")
g <- ggcorrplot(cor.male, method="circle", p.mat=p.male, sig.level=0.01, insig="blank")
g <- g + scale_fill_gradient2(limit=c(0.1, 10.0000001), trans="log" ,high="darkred", mid="white", low="darkblue", midpoint = 0.1,
                              breaks=c(0.1, 1, 10))
g


#Female
cor.female <- read.table("cor_female.txt", header=T, row.names=1, sep="\t")
p.female <- read.table("p_female.txt", header=T, row.names=1, sep="\t")
g <- ggcorrplot(cor.female, method="circle", p.mat=p.female, sig.level=0.01, insig="blank")
g <- g + scale_fill_gradient2(limit=c(0.1, 10.0000001), trans="log" ,high="darkred", mid="white", low="darkblue", midpoint = 0.1,
                              breaks=c(0.1, 1, 10))
g