library(survival)
library(survminer)
library(ggsci)
library(dplyr)
library(tidyverse)
library(ggplot2)
all.ptx <- read.table("PHF6.txt", header=T, sep="\t")
all.ptx$OS <- all.ptx$OS/30
all.ptx$PHF6[all.ptx$PHF6>1] <- 1
all.ptx$ASXL1[all.ptx$ASXL1>1] <- 1
all.ptx$BCOR[all.ptx$BCOR>1] <- 1
all.ptx$BRAF[all.ptx$BRAF>1] <- 1
all.ptx$CBL[all.ptx$CBL>1] <- 1
all.ptx$CEBPA[all.ptx$CEBPA>1] <- 1
all.ptx$DNMT3A[all.ptx$DNMT3A>1] <- 1
all.ptx$ETV6[all.ptx$ETV6>1] <- 1
all.ptx$EZH2[all.ptx$EZH2>1] <- 1
all.ptx$FLT3_ITD[all.ptx$FLT3_ITD>1] <- 1
all.ptx$FLT3_TKD[all.ptx$FLT3_TKD>1] <- 1
all.ptx$GATA2[all.ptx$GATA2>1] <- 1
all.ptx$IDH1[all.ptx$IDH1>1] <- 1
all.ptx$IDH2[all.ptx$IDH2>1] <- 1
all.ptx$JAK2[all.ptx$JAK2>1] <- 1
all.ptx$KIT[all.ptx$KIT>1] <- 1
all.ptx$KRAS[all.ptx$KRAS>1] <- 1
all.ptx$MPL[all.ptx$MPL>1] <- 1
all.ptx$NF1[all.ptx$NF1>1] <- 1
all.ptx$NRAS[all.ptx$NRAS>1] <- 1
all.ptx$PTPN11[all.ptx$PTPN11>1] <- 1
all.ptx$RUNX1[all.ptx$RUNX1>1] <- 1
all.ptx$SF3B1[all.ptx$SF3B1>1] <- 1
all.ptx$SRSF2[all.ptx$SRSF2>1] <- 1
all.ptx$STAG2[all.ptx$STAG2>1] <- 1
all.ptx$TET2[all.ptx$TET2>1] <- 1
all.ptx$TP53[all.ptx$TP53>1] <- 1
all.ptx$U2AF1[all.ptx$U2AF1>1] <- 1
all.ptx$ZRSR2[all.ptx$ZRSR2>1] <- 1
require("survival")

#PHF6
AML <- filter(all.ptx, Dx=="AML")
#AML <- filter(AML, (Cohort=="Open")|(Cohort=="CCF"))
AML <- filter(AML, Sex=="M")
AML <- filter(AML, Sex=="F")
fit_AML <- survfit(Surv(OS,Status)~PHF6, data=AML)
g_AML <- ggsurvplot(
  fit_AML,
  data=AML,
  risk.table=T,
  size=1,
  break.time.by = 6,
  xlab="Months",
  ylab="Survival probability",
  palette=pal_jco("default")(4),
  conf.int=F,
  pval=T,
  legend.labs=c("WT", "Mut"),
  ggtheme=theme_classic(),
  axes.offset=F,
  xlim=c(0,60),
  linetype = c("solid", "dashed")
)
g_AML
cox.all_p <- coxph(Surv(OS, Status)~factor(PHF6), data=AML)
summary(cox.all_p)

#Survival
surv.table <- as.data.frame(fit_AML$time)
surv.table$survival <- fit_AML$surv

#Risk
AML <- filter(all.ptx, Dx=="AML")
#AML <- filter(AML, Cohort=="Open")
AML <- filter(AML, Risk=="Favorable")
AML <- filter(AML, Risk=="Intermediate")
AML <- filter(AML, Risk=="Adverse")
AML <- filter(AML, Sex=="M")
AML <- filter(AML, Sex=="F")
fit_AML <- survfit(Surv(OS,Status)~PHF6, data=AML)
g_AML <- ggsurvplot(
  fit_AML,
  data=AML,
  risk.table=T,
  size=1,
  break.time.by = 6,
  xlab="Months",
  ylab="Survival probability",
  palette=pal_jco("default")(4),
  conf.int=F,
  pval=T,
  legend.labs=c("WT", "Mut"),
  ggtheme=theme_classic(),
  axes.offset=F,
  xlim=c(0,60),
  linetype = c("solid", "dashed")
)
g_AML

#Multivariate
AML$Sex[AML$Sex=="M"] <- 0
AML$Sex[AML$Sex=="F"] <- 1
AML$Sex <- as.factor(AML$Sex)
AML$Risk[AML$Risk=="Favorable"] <- 0
AML$Risk[AML$Risk=="Intermediate"] <- 1
AML$Risk[AML$Risk=="Adverse"] <- 2
AML$Risk <- as.factor(AML$Risk)
cox.hazard <- coxph(Surv(OS, Status)~factor(PHF6)+factor(RUNX1)+factor(U2AF1)+factor(ASXL1)+factor(Sex)+factor(Age_60), data=AML)
cox.hazard <- coxph(Surv(OS, Status)~factor(Age_60), data=AML)
summary(cox.hazard)

#PHF6xRUNX1
all.ptx$PHF6_RUNX1[(all.ptx$PHF6==0)&(all.ptx$RUNX1==0)] <- 0
all.ptx$PHF6_RUNX1[(all.ptx$PHF6>0)&(all.ptx$RUNX1==0)] <- 1
all.ptx$PHF6_RUNX1[(all.ptx$PHF6==0)&(all.ptx$RUNX1>0)] <- 2
all.ptx$PHF6_RUNX1[(all.ptx$PHF6>0)&(all.ptx$RUNX1>0)] <- 3
AML <- filter(all.ptx, Dx=="AML")
#AML <- filter(AML, Cohort=="Open")
#AML <- filter(AML, Sex=="M")
AML <- filter(AML, Sex=="F")
#AML$PHF6_RUNX1[AML$PHF6_RUNX1==0] <- NA
#AML$PHF6_RUNX1[AML$PHF6_RUNX1==1] <- NA
#AML$PHF6_RUNX1[AML$PHF6_RUNX1==2] <- 0
#AML$PHF6_RUNX1[AML$PHF6_RUNX1==3] <- 1
#AML <- filter(AML, Sex=="F")
#AML <- filter(AML, Risk=="Favorable")
##AML <- filter(AML, Risk=="Intermediate")
#AML <- filter(AML, Risk=="Adverse")
fit_AML <- survfit(Surv(OS,Status)~PHF6_RUNX1, data=AML)
g_AML <- ggsurvplot(
  fit_AML,
  data=AML,
  risk.table=T,
  size=1,
  break.time.by = 6,
  xlab="Months",
  ylab="Survival probability",
  palette=pal_jco("default")(4),
  conf.int=F,
  pval=T,
  legend.labs=c("WTxWT", "MUTxWT", "WTxMut", "MutxMut"),
  ggtheme=theme_classic(),
  axes.offset=F,
  xlim=c(0,36),
  linetype = c("solid", "dashed", "dashed", "dashed")
)
g_AML

cox.hazard <- coxph(Surv(OS, Status)~factor(PHF6_RUNX1),data=AML)
summary(cox.hazard)
cox.hazard
cox.zph(cox.hazard)
result <- glm(AML$Status~AML$PHF6*AML$RUNX1, family=binomial)
summary(result)