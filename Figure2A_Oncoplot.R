library(ComplexHeatmap)
data <- read.table("oncoplot.txt", header=T, row.names=1, sep="\t")

col <- c("M"="#00a1e9", "F"="#e4007f",
         "AML"="#BC3C29", "MDS"="#0072B5", "MDSMPN"="#E18727", "MPN"="#20854E",
         "St"="#7f1184", "Sp"="#ee7800", "Fr"="#a0d8ef", "No"="#f19ca7", 
         "In"="#0068b7", "Sy"="#fff462", "Mu"="#009944",
         "Yes"="black", "NANA"="#7d7d7d")
alter_fun = list(
  background = alter_graphic("rect", fill = "#f7f6f5"),
  M = alter_graphic("rect", fill = col["M"]),
  F = alter_graphic("rect", fill = col["F"]),
  AML = alter_graphic("rect", fill = col["AML"]),
  MDS = alter_graphic("rect", fill = col["MDS"]),
  MDSMPN = alter_graphic("rect", fill = col["MDSMPN"]),
  MPN = alter_graphic("rect", fill = col["MPN"]),
  St = alter_graphic("rect", fill = col["St"]),
  Sp = alter_graphic("rect", fill = col["Sp"]),
  Fr = alter_graphic("rect", fill = col["Fr"]),
  No = alter_graphic("rect", fill = col["No"]),
  In = alter_graphic("rect", fill = col["In"]),
  Sy = alter_graphic("rect", fill = col["Sy"]),
  Mu = alter_graphic("rect", fill = col["Mu"]),
  Yes = alter_graphic("rect", fill = col["Yes"]),
  NANA = alter_graphic("rect", fill = col["NANA"])
)


oncoPrint(data, alter_fun = alter_fun, col = col, show_pct=F, row_order=rownames(data),
          column_order=colnames(data), alter_fun_is_vectorized=F)


#Gene
gene <- data[-1,]
gene <- gene[1:28,]
oncoPrint(gene, alter_fun = alter_fun, col = col, show_pct=F, row_order=rownames(gene),
          column_order=colnames(gene), alter_fun_is_vectorized=F)

#karyotype
karyotype <- rbind(data[2,], data[30:37,])

oncoPrint(karyotype, alter_fun = alter_fun, col = col, show_pct=F, row_order=rownames(karyotype),
          column_order=colnames(karyotype), alter_fun_is_vectorized=F)

