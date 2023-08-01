library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
#DNA methylation
methy <- read.table("chrX_promoter.txt", header=T ,sep="\t")
methy <- methy %>% tidyr::gather(key=Gene, value=beta, -ID, -Sex)
methy <- transform(methy, Sex=factor(Sex, levels=c("M", "F")))
methy <- transform(methy, Gene=factor(Gene, levels=c("UBA1", "ZRSR2", "PHF6", "STAG2", "BCORL1", "BCOR", "ATRX", "PIGA", "KDM6A")))

g <- ggplot(methy, aes(x=Sex, y=beta, color=Sex))
g <- g + geom_boxplot(outlier.size=2, outlier.alpha=0.7)
#g <- g + geom_beeswarm(aes(color=Sex), size=0.2, cex=1)
g <- g + scale_y_continuous(expand=c(0,0))
g <- g + scale_color_manual(values=c("#00008b", "#8b0000"))
g <- g + theme_classic()
g <- g + facet_grid(. ~ Gene, scales="free")
g

UBA1 <- filter(methy, Gene=="UBA1")
UBA1.male <- filter(UBA1, Sex=="M")
UBA1.female <- filter(UBA1, Sex=="F")
ks.test(UBA1.male$beta, UBA1.female$beta)
t.test(UBA1.male$beta, UBA1.female$beta)
mean(UBA1.male$beta)
mean(UBA1.female$beta)
#difference 0.11

ZRSR2 <- filter(methy, Gene=="ZRSR2")
ZRSR2.male <- filter(ZRSR2, Sex=="M")
ZRSR2.female <- filter(ZRSR2, Sex=="F")
ks.test(ZRSR2.male$beta, ZRSR2.female$beta)
t.test(ZRSR2.male$beta, ZRSR2.female$beta)
mean(ZRSR2.male$beta)
mean(ZRSR2.female$beta)
#difference 0.11

PHF6 <- filter(methy, Gene=="PHF6")
PHF6.male <- filter(PHF6, Sex=="M")
PHF6.female <- filter(PHF6, Sex=="F")
ks.test(PHF6.male$beta, PHF6.female$beta)
t.test(PHF6.male$beta, PHF6.female$beta)
mean(PHF6.male$beta)
mean(PHF6.female$beta)
#difference 0.30

STAG2 <- filter(methy, Gene=="STAG2")
STAG2.male <- na.omit(filter(STAG2, Sex=="M"))
STAG2.female <- na.omit(filter(STAG2, Sex=="F"))
ks.test(STAG2.male$beta, STAG2.female$beta)
t.test(STAG2.male$beta, STAG2.female$beta)
mean(STAG2.male$beta)
mean(STAG2.female$beta)
#difference 0.47

BCORL1 <- filter(methy, Gene=="BCORL1")
BCORL1.male <- na.omit(filter(BCORL1, Sex=="M"))
BCORL1.female <- na.omit(filter(BCORL1, Sex=="F"))
ks.test(BCORL1.male$beta, BCORL1.female$beta)
t.test(BCORL1.male$beta, BCORL1.female$beta)
mean(BCORL1.male$beta)
mean(BCORL1.female$beta)
#difference 0.47

BCOR <- filter(methy, Gene=="BCOR")
BCOR.male <- na.omit(filter(BCOR, Sex=="M"))
BCOR.female <- na.omit(filter(BCOR, Sex=="F"))
ks.test(BCOR.male$beta, BCOR.female$beta)
t.test(BCOR.male$beta, BCOR.female$beta)
mean(BCOR.male$beta)
mean(BCOR.female$beta)
#difference 0.30

ATRX <- filter(methy, Gene=="ATRX")
ATRX.male <- na.omit(filter(ATRX, Sex=="M"))
ATRX.female <- na.omit(filter(ATRX, Sex=="F"))
ks.test(ATRX.male$beta, ATRX.female$beta)
t.test(ATRX.male$beta, ATRX.female$beta)
mean(ATRX.male$beta)
mean(ATRX.female$beta)
#difference 0.30

PIGA <- filter(methy, Gene=="PIGA")
PIGA.male <- na.omit(filter(PIGA, Sex=="M"))
PIGA.female <- na.omit(filter(PIGA, Sex=="F"))
ks.test(PIGA.male$beta, PIGA.female$beta)
t.test(PIGA.male$beta, PIGA.female$beta)
mean(PIGA.male$beta)
mean(PIGA.female$beta)
#difference 0.35

KDM6A <- filter(methy, Gene=="KDM6A")
KDM6A.male <- filter(KDM6A, Sex=="M")
KDM6A.female <- filter(KDM6A, Sex=="F")
ks.test(KDM6A.male$beta, KDM6A.female$beta)
t.test(KDM6A.male$beta, KDM6A.female$beta)
mean(KDM6A.male$beta)
mean(KDM6A.female$beta)
#difference 0.01

#Expression
expr <- read.table("chrX_expr.txt", header=T, sep="\t")
expr <- expr %>% tidyr::gather(key=Gene, value=beta, -ID, -Sex)
expr <- transform(expr, Sex=factor(Sex, levels=c("M", "F")))
expr <- transform(expr, Gene=factor(Gene, levels=c("UBA1", "ZRSR2", "PHF6", "STAG2", "BCORL1", "BCOR", "ATRX", "PIGA", "KDM6A")))

g <- ggplot(expr, aes(x=Sex, y=beta, color=Sex))
g <- g + geom_boxplot(outlier.size=2, outlier.alpha=0.7)
g <- g + scale_color_manual(values=c("#00008b", "#8b0000"))
g <- g + theme_classic()
g <- g + facet_grid(. ~ Gene, scales="free")
g

UBA1 <- filter(expr, Gene=="UBA1")
UBA1.male <- filter(UBA1, Sex=="M")
UBA1.female <- filter(UBA1, Sex=="F")
ks.test(UBA1.male$beta, UBA1.female$beta)
t.test(UBA1.male$beta, UBA1.female$beta)

ZRSR2 <- filter(expr, Gene=="ZRSR2")
ZRSR2.male <- filter(ZRSR2, Sex=="M")
ZRSR2.female <- filter(ZRSR2, Sex=="F")
ks.test(ZRSR2.male$beta, ZRSR2.female$beta)
t.test(ZRSR2.male$beta, ZRSR2.female$beta)

PHF6 <- filter(expr, Gene=="PHF6")
PHF6.male <- filter(PHF6, Sex=="M")
PHF6.female <- filter(PHF6, Sex=="F")
ks.test(PHF6.male$beta, PHF6.female$beta)

STAG2 <- filter(expr, Gene=="STAG2")
STAG2.male <- filter(STAG2, Sex=="M")
STAG2.female <- filter(STAG2, Sex=="F")
ks.test(STAG2.male$beta, STAG2.female$beta)
t.test(STAG2.male$beta, STAG2.female$beta)

BCORL1 <- filter(expr, Gene=="BCORL1")
BCORL1.male <- filter(BCORL1, Sex=="M")
BCORL1.female <- filter(BCORL1, Sex=="F")
ks.test(BCORL1.male$beta, BCORL1.female$beta)
t.test(BCORL1.male$beta, BCORL1.female$beta)

BCOR <- filter(expr, Gene=="BCOR")
BCOR.male <- filter(BCOR, Sex=="M")
BCOR.female <- filter(BCOR, Sex=="F")
ks.test(BCOR.male$beta, BCOR.female$beta)
t.test(BCOR.male$beta, BCOR.female$beta)

ATRX <- filter(expr, Gene=="ATRX")
ATRX.male <- filter(ATRX, Sex=="M")
ATRX.female <- filter(ATRX, Sex=="F")
ks.test(ATRX.male$beta, ATRX.female$beta)
t.test(ATRX.male$beta, ATRX.female$beta)

PIGA <- filter(expr, Gene=="PIGA")
PIGA.male <- filter(PIGA, Sex=="M")
PIGA.female <- filter(PIGA, Sex=="F")
ks.test(PIGA.male$beta, PIGA.female$beta)
t.test(PIGA.male$beta, PIGA.female$beta)

KDM6A <- filter(expr, Gene=="KDM6A")
KDM6A.male <- filter(KDM6A, Sex=="M")
KDM6A.female <- filter(KDM6A, Sex=="F")
ks.test(KDM6A.male$beta, KDM6A.female$beta)
t.test(KDM6A.male$beta, KDM6A.female$beta)

comp.table <- as.data.frame(c("UBA1", "ZRSR2", "PHF6", "STAG2", "BCORL1", "BCOR", "ATRX", "PIGA", "KDM6A"))
colnames(comp.table) <-"Gene"
comp.table$p.value <- c(rep(0,9))
comp.table$FDR <- c(rep(0,9))
temp <- t.test(UBA1.male$beta, UBA1.female$beta)
comp.table$p.value[1] <- temp$p.value
temp <- t.test(ZRSR2.male$beta, ZRSR2.female$beta)
comp.table$p.value[2] <- temp$p.value
temp <- t.test(PHF6.male$beta, PHF6.female$beta)
comp.table$p.value[3] <- temp$p.value
temp <- t.test(STAG2.male$beta, STAG2.female$beta)
comp.table$p.value[4] <- temp$p.value
temp <- t.test(BCORL1.male$beta, BCORL1.female$beta)
comp.table$p.value[5] <- temp$p.value
temp <- t.test(BCOR.male$beta, BCOR.female$beta)
comp.table$p.value[6] <- temp$p.value
temp <- t.test(ATRX.male$beta, ATRX.female$beta)
comp.table$p.value[7] <- temp$p.value
temp <- t.test(PIGA.male$beta, PIGA.female$beta)
comp.table$p.value[8] <- temp$p.value
temp <- t.test(KDM6A.male$beta, KDM6A.female$beta)
comp.table$p.value[9] <- temp$p.value
comp.table$FDR <- p.adjust(comp.table$p.value, method="BH")


