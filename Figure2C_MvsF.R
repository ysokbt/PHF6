library(ggcorrplot)
library(ggplot2)

#Male vs Female
male <- read.table("gene_male.txt", header=T, sep="\t")
male$p.Fisher <- NULL
male$OR.lower <- NULL
male$OR.upper <- NULL
male$Pearson.r <- NULL
male$p.Pearson <- NULL
male$FDR.Pearson <- NULL
comp <- as.data.frame(male$Gene1)
colnames(comp) <- "Gene1"
comp$Gene2 <- male$Gene2
comp$FDR.male <- male$FDR.Fisher
comp$OR.male <- male$OR
female <- read.table("gene_female.txt", header=T, sep="\t")
comp$FDR.female <- female$FDR.Fisher
comp$OR.female <- female$OR
#write.table(comp, "comp_MvsF.txt", sep="\t", quote=F)

comp <- read.table("comp_MvsF.txt", header=T, sep="\t")

#Large
g <- ggplot(comp, aes(x=-log10(FDR.male), y=-log10(FDR.female)))
g <- g + geom_point()
g <- g + scale_y_continuous(limits=c(0,60), expand=c(0,0))
g <- g + scale_x_continuous(limits=c(0,60), expand=c(0,0))
g <- g + theme_classic()
g

library(ggsci)
#Small
comp$Color <-c(rep(0,406))
comp$Color[(comp$FDR.male<0.01)&(comp$FDR.female>=0.01)] <- 1
comp$Color[(comp$FDR.male>=0.01)&(comp$FDR.female<0.01)] <- 2
comp$Color[(comp$Color==1)&((comp$Gene1=="PHF6")|(comp$Gene1=="STAG2")|
             (comp$Gene2=="ZRSR2")|(comp$Gene2=="BCOR")|(comp$Gene2=="STAG2"))] <- 3

comp$Color <- as.character(comp$Color)
g <- ggplot(comp, aes(x=(-log10(FDR.male)+0.01), y=(-log10(FDR.female)+0.01)))
g <- g + geom_point(aes(color=Color), size=1.5)
g <- g + scale_x_log10(breaks=c(0.01, 0.1, 1, 10, 100), limits=c(0.01,500))
g <- g + scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100), limits=c(0.01,500))
#g <- g + scale_y_continuous(breaks=c(0,2,5,10,20), limits=c(0,20), expand=c(0,0))
#g <- g + scale_x_continuous(breaks=c(0,2,5,10,20), limits=c(0,20), expand=c(0,0))
g <- g + geom_hline(yintercept=2, linetype="dotted") + geom_vline(xintercept=2, linetype="dotted")
g <- g + scale_color_manual(values=c("#00000064", "#00008ba0", "#8b0000a0", "#EFC000a0"))
g <- g + theme_classic()
g <- g + theme(legend.position="none")
g