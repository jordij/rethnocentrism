library(ggplot2)
library(reshape2)
library(extrafont)

#############
## WARNING ##
#############
## I like using Gill Sans Nova typeface for plots and maps.
## If you need to load your fonts in Windows, uncomment the line below.
## If you don't have Gill Sans Nova locally just replace all occurences of `pfamily="Gill Sans Nove"`
## or with your typeface of choice.

# loadfonts(device = "win")

pfamily <- "Gill Sans Nova"
ptextsize <- 20

# Immigrant chance cooperate with same from 0 to 1 in 0.1 intervals
# 10 executions of 2000 steps per interval
dset <- read.table(file="./data/ICCWS.csv", sep=",", header=TRUE)
dmelt <- melt(dset, id.vars="immigrant.chance.cooperate.with.same")
dtall <- subset(dmelt, (variable == "cc.percent" | variable == "cd.percent" | variable == "dc.percent" | variable == "dd.percent"))
# rename levels and values
colnames(dtall) <- c("iccws", "Strategy", "Value")
levels(dtall$Strategy)[levels(dtall$Strategy) == "cc.percent"] <- "CC"
levels(dtall$Strategy)[levels(dtall$Strategy) == "cd.percent"] <- "CD"
levels(dtall$Strategy)[levels(dtall$Strategy) == "dc.percent"] <- "DC"
levels(dtall$Strategy)[levels(dtall$Strategy) == "dd.percent"] <- "DD"

pplot <- ggplot(dtall, aes(x=iccws, Value, col=Strategy)) + 
    geom_point() + 
    geom_smooth(method="loess") +
    ggtitle("")  +
    xlab("Immigrant chance cooperate with same") + 
    ylab("Strategy percentage") +
    scale_x_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1)) +
    scale_colour_manual(values = c("CC" = "#007cff", "CD" = "#ff0000", "DC" = "#ceb600", "DD" = "#048c4e")) +
    scale_fill_manual(values = c("CC" = "#007cff", "CD" = "#ff0000", "DC" = "#ceb600", "DD" = "#048c4e")) +
    theme(plot.title=element_text(hjust = 0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        text=element_text(size=ptextsize, family=pfamily),
        legend.spacing = unit(1, "cm"),
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        rect=element_blank())

png(file="./plots/iccws.png", width = 800, height = 500)
print(pplot)
dev.off()
remove(pplot)

