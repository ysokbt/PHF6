library(g3viz)
PHF6 <- read.table("20220509.txt", header=T, sep="\t")

plot.options <- g3Lollipop.options(chart.width=550,
                                   chart.margin=list(left=40, right=20, top=15, bottom=25),
                                   chart.background="transparent",
                                   chart.type="pie", legend.title="Type",
                                   lollipop.track.height=150,
                                   lollipop.track.background="white",
                                   lollipop.pop.min.size=4,
                                   lollipop.pop.max.size=20,
                                   lollipop.pop.info.color="white",
                                   lollipop.circle.width=0, 
                                   lollipop.color.scheme="google16",
                                   anno.height=50,
                                   domain.text.font="normal 20px Arial",
                                   domain.text.color="white",
                                   brush=F)

#library(tidyverse)
#PHF6 <- filter(PHF6, Dx=="AML")

g3Lollipop(PHF6,
           plot.options = plot.options,
           btn.style = "blue",
           gene.symbol = "PHF6",
           aa.pos.col = "AA_Position",
           protein.change.col = "Protein_Change",
           factor.col = 'Type')

g3Lollipop(PHF6,
           plot.options = plot.options,
           btn.style = "blue",
           gene.symbol = "PHF6",
           aa.pos.col = "AA_Position",
           protein.change.col = "Protein_Change",
           factor.col = 'Dx')

g3Lollipop(PHF6,
           plot.options = plot.options,
           btn.style = "blue",
           gene.symbol = "PHF6",
           aa.pos.col = "AA_Position",
           protein.change.col = "Protein_Change",
           factor.col = 'Sex')