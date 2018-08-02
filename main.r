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

# BASELINE
# 10 executions of 2000 steps
dset <- read.table(file="./data/BASELINE.csv", sep=",", header=TRUE)
dmelt <- melt(dset, id.vars="X.run.number.")
dtall <- subset(dmelt, (variable == "cc.percent" | variable == "cd.percent" | variable == "dc.percent" | variable == "dd.percent"))
# rename levels and values
colnames(dtall) <- c("Execution", "Strategy", "Value")
levels(dtall$Strategy)[levels(dtall$Strategy) == "cc.percent"] <- "CC"
levels(dtall$Strategy)[levels(dtall$Strategy) == "cd.percent"] <- "CD"
levels(dtall$Strategy)[levels(dtall$Strategy) == "dc.percent"] <- "DC"
levels(dtall$Strategy)[levels(dtall$Strategy) == "dd.percent"] <- "DD"

pplot <- ggplot(dtall, aes(Execution, Value, color=Strategy)) + 
    geom_point(size = 2) +
    geom_hline(yintercept = mean(dtall$Value[dtall$Strategy == "CC"]), color="#007cff") +
    geom_hline(yintercept = mean(dtall$Value[dtall$Strategy == "CD"]), color="#ff0000") +
    geom_hline(yintercept = mean(dtall$Value[dtall$Strategy == "DC"]), color="#ceb600") +
    geom_hline(yintercept = mean(dtall$Value[dtall$Strategy == "DD"]), color="#048c4e") +
    # geom_smooth(method="lm", level=0.8, linetype="dotted") + 
    ggtitle("")  +
    xlab("Executions") + 
    ylab("Strategy percentage") +
    scale_x_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1)) +
    scale_colour_manual(values = c("CC" = "#007cff", "CD" = "#ff0000", "DC" = "#ceb600", "DD" = "#048c4e")) +
    scale_fill_manual(values = c("CC" = "#007cff", "CD" = "#ff0000", "DC" = "#ceb600", "DD" = "#048c4e")) +
    theme(plot.title=element_text(hjust = 0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily),
        text=element_text(size=ptextsize, family=pfamily),
        legend.spacing = unit(1, "cm"),
        rect=element_blank()) +
    theme(axis.line.x = element_line(color="grey", size = 0.5),
        axis.line.y = element_line(color="grey", size = 0.5))

png(file="./plots/baseline.png", width = 800, height = 500)
print(pplot)
dev.off()
remove(pplot)
remove(dtall)
remove(dmelt)
remove(dset)

# ICCWS - Immigrant chance cooperate with same from 0 to 1 in 0.1 intervals
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
        rect=element_blank()) +
    theme(axis.line.x = element_line(color="grey", size = 0.5),
        axis.line.y = element_line(color="grey", size = 0.5))

png(file="./plots/iccws.png", width = 800, height = 500)
print(pplot)
dev.off()
remove(pplot)
remove(dtall)
remove(dmelt)
remove(dset)

# ICCWD - Immigrant chance cooperate with different from 0 to 1 in 0.1 intervals
# 10 executions of 2000 steps per interval
dset <- read.table(file="./data/ICCWD.csv", sep=",", header=TRUE)
dmelt <- melt(dset, id.vars="immigrant.chance.cooperate.with.different")
dtall <- subset(dmelt, (variable == "cc.percent" | variable == "cd.percent" | variable == "dc.percent" | variable == "dd.percent"))
# rename levels and values
colnames(dtall) <- c("iccwd", "Strategy", "Value")
levels(dtall$Strategy)[levels(dtall$Strategy) == "cc.percent"] <- "CC"
levels(dtall$Strategy)[levels(dtall$Strategy) == "cd.percent"] <- "CD"
levels(dtall$Strategy)[levels(dtall$Strategy) == "dc.percent"] <- "DC"
levels(dtall$Strategy)[levels(dtall$Strategy) == "dd.percent"] <- "DD"

pplot <- ggplot(dtall, aes(x=iccwd, Value, col=Strategy)) + 
    geom_point() + 
    geom_smooth(method="loess") +
    ggtitle("")  +
    xlab("Immigrant chance cooperate with different") + 
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
        rect=element_blank()) +
    theme(axis.line.x = element_line(color="grey", size = 0.5),
        axis.line.y = element_line(color="grey", size = 0.5))

png(file="./plots/iccwd.png", width = 800, height = 500)
print(pplot)
dev.off()
remove(pplot)
remove(dtall)
remove(dmelt)
remove(dset)

# ICCWD - Immigrant chance cooperate with different AND same from 0 to 1 in 0.1 intervals
# 10 executions of 2000 steps per interval
dset <- read.table(file="./data/ICCWD_ICCWS.csv", sep=",", header=TRUE)
# only keep those where ICCWD equals ICCWS
dset <- subset(dset, immigrant.chance.cooperate.with.different == immigrant.chance.cooperate.with.same)
dmelt <- melt(dset, id.vars="immigrant.chance.cooperate.with.different")
dtall <- subset(dmelt, (variable == "cc.percent" | variable == "cd.percent" | variable == "dc.percent" | variable == "dd.percent"))
# rename levels and values
colnames(dtall) <- c("iccwd", "Strategy", "Value")
levels(dtall$Strategy)[levels(dtall$Strategy) == "cc.percent"] <- "CC"
levels(dtall$Strategy)[levels(dtall$Strategy) == "cd.percent"] <- "CD"
levels(dtall$Strategy)[levels(dtall$Strategy) == "dc.percent"] <- "DC"
levels(dtall$Strategy)[levels(dtall$Strategy) == "dd.percent"] <- "DD"

pplot <- ggplot(dtall, aes(x=iccwd, Value, col=Strategy)) + 
    geom_point() + 
    geom_smooth(method="loess") +
    ggtitle("")  +
    xlab("Immigrant chance cooperate with different and same") + 
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
        rect=element_blank()) +
    theme(axis.line.x = element_line(color="grey", size = 0.5),
        axis.line.y = element_line(color="grey", size = 0.5))

png(file="./plots/iccwd_and_iccwd.png", width = 800, height = 500)
print(pplot)
dev.off()
remove(pplot)
remove(dtall)
remove(dmelt)
remove(dset)


# COG - Cost of giving from 0 to 0.5 in 0.01 intervals
# 10 executions of 2000 steps per interval
dset <- read.table(file="./data/COG.csv", sep=",", header=TRUE)
dmelt <- melt(dset, id.vars="cost.of.giving")
dtall <- subset(dmelt, (variable == "cc.percent" | variable == "cd.percent" | variable == "dc.percent" | variable == "dd.percent"))
# rename levels and values
colnames(dtall) <- c("cost", "Strategy", "Value")
levels(dtall$Strategy)[levels(dtall$Strategy) == "cc.percent"] <- "CC"
levels(dtall$Strategy)[levels(dtall$Strategy) == "cd.percent"] <- "CD"
levels(dtall$Strategy)[levels(dtall$Strategy) == "dc.percent"] <- "DC"
levels(dtall$Strategy)[levels(dtall$Strategy) == "dd.percent"] <- "DD"

pplot <- ggplot(dtall, aes(x=cost, Value, col=Strategy)) + 
    geom_point() + 
    geom_smooth(method="loess") +
    ggtitle("")  +
    xlab("Cost of giving") + 
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
        rect=element_blank()) +
    theme(axis.line.x = element_line(color="grey", size = 0.5),
        axis.line.y = element_line(color="grey", size = 0.5))

png(file="./plots/cog.png", width = 800, height = 500)
print(pplot)
dev.off()
remove(pplot)
remove(dtall)
remove(dmelt)
remove(dset)
