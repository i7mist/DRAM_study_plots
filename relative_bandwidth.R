# Read the data from a file. The data is about a study on the effect of vitamin C on tooth growth in guinea pigs. There are 4 columns: `X`, `len`, `supp` and `dose`. This example visually presents tooth growth progress depending on the delivery method: `OJ` (orange juice) or `VC` (ascorbic acid)
library(argparse)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--ytitle", required=TRUE, help="input y axis title")
parser$add_argument("--graphtitle", required=TRUE, help="input the title of the graph")
parser$add_argument("--dir", required=TRUE, help="dir of input csv files and output plots")

args <- parser$parse_args()

hp_dir <- gsub("/all/", "/highperf_dram/", args$dir)
lp_dir <- gsub("/all/", "/lp_dram/", args$dir)
ref_memory_intensity_csv <- paste(args$dir, "/average_outstanding_requests.csv", sep="")
hpipc_csv <- paste(hp_dir, "/", args$ytitle, ".csv", sep="")
lpipc_csv <- paste(lp_dir, "/", args$ytitle, ".csv", sep="")
print(hpipc_csv)
print(lpipc_csv)
print(ref_memory_intensity_csv)
print(args$ytitle)
print(args$graphtitle)

hp_normalized_ipc <- read.csv(hpipc_csv)
lp_normalized_ipc <- read.csv(lpipc_csv)
ref_memory_intensity <- read.csv(ref_memory_intensity_csv)
ytitle <- args$ytitle
graphtitle <- args$graphtitle
output_prefix <- paste(args$dir, "/", args$ytitle, sep="")
print(output_prefix)

# Summarise the data with `ddply` from the **plyr** package, group by `supp` and `dose` and compute the mean of the corresponding values under the `len` column. This creates a new data frame whose columns are: `supp`, `dose` and `length`. 

#library(plyr)
library(dplyr)
print(head(ref_memory_intensity))

ref_memory_intensity <- filter(ref_memory_intensity, DRAM == "DDR3-2133L")
print(head(ref_memory_intensity))

# The next block of code renders the plot, the columns `dose` and `length` on the x, y axes and `supp` is used to group the data and [colour](https://www.getdatajoy.com/learn/Colour_Names:_Complete_List) each line.
cbPalette <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#FF79A7")
print(head(hp_normalized_ipc))

library(ggplot2)
p1<-ggplot(hp_normalized_ipc, aes(x=reorder(workload, ref_memory_intensity), y=value,       # columns to plot
               fill=DRAM, group=DRAM, color=DRAM)) +           # colour determined by "dupp"
    scale_fill_manual(values=c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
    scale_colour_manual(values=c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
    geom_line() +
    geom_point() +
    xlab("workloads") +                               # x-axis label
    ylab(ytitle) +                             # y-axis label
    ggtitle(graphtitle)  +                          # title
    theme(axis.text.x=element_text(angle=60, vjust=0.5),
    legend.text=element_text(size=8),
    plot.margin=rep(unit(0,"cm"),each=4))


p2<-ggplot(ref_memory_intensity, aes(x=reorder(workload, ref_memory_intensity), y=ref_memory_intensity)) +
    geom_bar(stat='identity') +
    xlab("workloads") +
    ylab("average outstanding requests") +
    ylim(0,50) +
    theme(axis.text.x=element_text(angle=60, vjust=0.5, size=8),
    plot.margin=rep(unit(0,"cm"),each=4))

p3<-ggplot(lp_normalized_ipc, aes(x=reorder(workload, ref_memory_intensity), y=value,       # columns to plot
               fill=DRAM, group=DRAM, color=DRAM)) +           # colour determined by "dupp"
    scale_colour_manual(values=c("#0072B2", "#D55E00", "#CC79A7", "#BB7900")) +
    geom_line() +
    xlab("workloads") +                               # x-axis label
    ylab(ytitle) +                             # y-axis label
    ggtitle(graphtitle)  +                          # title
    theme(axis.text.x=element_text(angle=60, vjust=0.5),
    legend.text=element_text(size=8),
    plot.margin=rep(unit(0,"cm"),each=4))


library(gridExtra)
library(grid)
grid.arrange(p1,p2,p3,ncol=2,
widths = c(unit(0.6, "npc"), unit(0.4, "npc")))

g <- arrangeGrob(p1,p2,p3,ncol=2,
  widths = c(unit(0.6, "npc"), unit(0.4, "npc"))) #generates g
ggsave(file=paste(output_prefix,".png", sep=""), g) #saves g
ggsave(file=paste(output_prefix,".pdf", sep=""), g) #saves g

