library(ggplot2)
library(ggsci)

#MPN
All <- as.data.frame(c("pAML", "sAML", "SLD", "MLD", "RS", "EB", "Del5q",
                       "CMML", "aCML", "MDS/MPN-RS-T", "MDS/MPN_Other", "CML", "PV", "PMF", "ET", "MPN_Other"))
All$Percent <- c(1.37, 2.90, 4.22, 1.36, 1.32, 3.83, 0, 3.82, 4.35, 1.52, 3.57, 0, 3.51, 0, 1.79, 0)
colnames(All) <- c("Dx", "Percent")
All <- transform(All, Dx=factor(Dx, levels=c("pAML", "sAML", "SLD", "MLD", "RS", "EB", "Del5q",
                                             "CMML", "aCML", "MDS/MPN-RS-T", "MDS/MPN_Other", "CML", "PV", "PMF", "ET", "MPN_Other")))
g <- ggplot(All, aes(x=Dx, y=Percent, fill=Dx))
g <- g + geom_bar(stat="identity")
g <- g + xlab("") + ylab("Frequency of PHF6 mutations (%)")
g <- g + scale_y_continuous(expand=c(0,0))
g <- g + theme_classic()
g <- g + scale_fill_manual(values=c("#BC3C29", "#BC3C29", "#0072B5", "#0072B5", "#0072B5", "#0072B5", "#0072B5",
                                    "#E18727", "#E18727", "#E18727", "#E18727", "#20854E", "#20854E", "#20854E", "#20854E", "#20854E"))
g <- g + theme(legend.position="none")
g <- g + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.4))
g

pie(c(3,2), labels=c("M", "F"), col=c("#00008b", "#8b0000"), clockwise=T)



#AML
AML <- as.data.frame(c("pAML", "sAML"))
AML$Percent <- c(72/5322*100, 33/1393*100)
colnames(AML) <- c("Dx", "Percent")
AML <- transform(AML, Dx=factor(Dx, levels=c("pAML", "sAML")))
g <- ggplot(AML, aes(x=Dx, y=Percent, fill=Dx))
g <- g + geom_bar(stat="identity")
g <- g + xlab("") + ylab("Frequency of mutations (%)")
g <- g + scale_y_continuous(breaks=c(0, 1, 2), limits=c(0,2.5), expand=c(0,0))
g <- g + theme_classic()
g <- g + scale_fill_nejm()
g <- g + geom_signif(comparisons=list(c("pAML", "sAML")), annotation=c("*"))
g


#MDS
MDS <- as.data.frame(c("SLD", "MLD", "RS", "EB", "Del5q", "Other or NA"))
MDS$Percent <- c(2/30*100, 1/114*100, 2/184*100, 9/263*100, 0, 5/233*100)
colnames(MDS) <- c("Dx", "Percent")
MDS <- transform(MDS, Dx=factor(Dx, levels=c("SLD", "MLD", "RS", "EB", "Del5q", "Other or NA")))
g <- ggplot(MDS, aes(x=Dx, y=Percent, fill=Dx))
g <- g + geom_bar(stat="identity")
g <- g + xlab("") + ylab("Frequency of mutations (%)")
g <- g + scale_y_continuous(expand=c(0,0))
g <- g + theme_classic()
g <- g + scale_fill_nejm()
g


#MDS/MPN
MDSMPN <- as.data.frame(c("CMML", "aCML", "MDS/MPN-RS-T", "Other or NA"))
MDSMPN$Percent <- c(6/152*100, 1/22*100, 1/46*100, 4/165*100)
colnames(MDSMPN) <- c("Dx", "Percent")
MDSMPN <- transform(MDSMPN, Dx=factor(Dx, levels=c("CMML", "aCML", "MDS/MPN-RS-T", "Other or NA")))
g <- ggplot(MDSMPN, aes(x=Dx, y=Percent, fill=Dx))
g <- g + geom_bar(stat="identity")
g <- g + xlab("") + ylab("Frequency of mutations (%)")
g <- g + scale_y_continuous(expand=c(0,0))
g <- g + theme_classic()
g <- g + scale_fill_nejm()
g


#MPN
MPN <- as.data.frame(c("CML", "PV", "PMF", "ET", "Other or NA"))
MPN$Percent <- c(0, 2/44*100, 0, 1/28*100, 1/139*100)
colnames(MPN) <- c("Dx", "Percent")
MPN <- transform(MPN, Dx=factor(Dx, levels=c("CML", "PV", "PMF", "ET", "Other or NA")))
g <- ggplot(MPN, aes(x=Dx, y=Percent, fill=Dx))
g <- g + geom_bar(stat="identity")
g <- g + xlab("") + ylab("Frequency of mutations (%)")
g <- g + scale_y_continuous(expand=c(0,0))
g <- g + theme_classic()
g <- g + scale_fill_nejm()
g