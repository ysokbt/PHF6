library(data.table)
library(ggplot2)
PHF6 <- fread("PHF6_DEG_annotation.txt", header=T, sep="\t")
PHF6$Color <- c(rep("Other", 10000))
PHF6$Color[(PHF6$Log2FC>1)&(PHF6$adj.P.Val<0.25)] <- "Higher"
PHF6$Color[(PHF6$Log2FC<(-1))&(PHF6$adj.P.Val<0.25)] <- "Lower"
PHF6$Color[PHF6$Anno=="Lymph"] <- "Lymph"
PHF6$Color[PHF6$Anno=="Myelo"] <- "Myelo"
PHF6 <- transform(PHF6, Color=factor(Color, levels=c("Lymph", "Myelo", "Higher", "Lower", "Other")))
PHF6 <- PHF6[1:1000,]

g <- ggplot(PHF6, aes(x=Log2FC, y=-log10(adj.P.Val), color=Color))
g <- g + geom_point(size=1)
g <- g + scale_x_continuous(limits=c(-3,6))
g <- g + scale_y_continuous(limits=c(0,3), expand=c(0,0))
g <- g + ylab("-log10FDR")
g <- g + scale_color_manual(values=c("#ee7800", "#00a1e9","darkred", "darkblue", "#00000032"))
g <- g + geom_hline(yintercept=0.6, linetype="dotted")
g <- g + geom_vline(xintercept=1, linetype="dotted")
g <- g + geom_vline(xintercept=-1, linetype="dotted")
g <- g + theme_classic()
g

RUNX1 <- fread("RUNX1_DEG_annotation.txt", header=T, sep="\t")
RUNX1$Color[is.na(RUNX1$Color)] <- "Other"

g <- ggplot(RUNX1, aes(x=Log2FC, y=-log10(adj.P.Val), color=Color))
g <- g + geom_point(size=1)
#g <- g + scale_x_continuous(limits=c(-3,6))
#g <- g + scale_y_continuous(limits=c(0,3), expand=c(0,0))
g <- g + ylab("-log10FDR")
g <- g + scale_color_manual(values=c("#ee7800", "#00a1e9","#00000032"))
g <- g + geom_hline(yintercept=01, linetype="dotted")
g <- g + geom_vline(xintercept=1, linetype="dotted")
g <- g + geom_vline(xintercept=-1, linetype="dotted")
g <- g + theme_classic()
g

Both <- fread("Both_DEG_annotation.txt", header=T, sep="\t")
Both$Color[is.na(RUNX1$Color)] <- "Other"

g <- ggplot(Both, aes(x=Log2FC, y=-log10(adj.P.Val), color=Color))
g <- g + geom_point(size=1)
#g <- g + scale_x_continuous(limits=c(-3,6))
#g <- g + scale_y_continuous(limits=c(0,3), expand=c(0,0))
g <- g + ylab("-log10FDR")
g <- g + scale_color_manual(values=c("#ee7800", "#00a1e9","#00000032"))
g <- g + geom_hline(yintercept=01, linetype="dotted")
g <- g + geom_vline(xintercept=1, linetype="dotted")
g <- g + geom_vline(xintercept=-1, linetype="dotted")
g <- g + theme_classic()
g