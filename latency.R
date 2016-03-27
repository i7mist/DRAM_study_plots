# The data is read from a csv file, grouped in columns: Cultivar, Date, Weight, sd, n, se

library(argparse)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--dir", required=TRUE, help="input dir that contains the csv file and also the output dir for plots")

args <- parser$parse_args()

print(args$dir)

csv_path <- paste(args$dir, "/average_latency_ns.csv", sep="")
output_prefix <- paste(args$dir, "/latency", sep="")

print(csv_path)

latency_exp <- read.csv(csv_path)

# ---
# Stacked Bar Graph
# -----------------

library(dplyr)
#library(plyr)
library(ggplot2)
library(gridExtra)

workload_list <- unique(latency_exp$workload)

plots <- list()

workload_num <- length(workload_list)
newpage_cnt <- 0

for (i in 1:workload_num)
{
    workload_name <- workload_list[i]
    exp <- filter(latency_exp, workload == workload_name)
#    print(exp)

    # Create the plot using `Date` on the x axis, `Weight` on the Y axis and the column `Cultivar` to determine the right colour

    plots[[length(plots)+1]] <- ggplot(exp, aes(x=DRAM, y=value, fill=statistics)) +
        geom_bar(stat="identity", colour='black') +     # contour colour
        guides(fill=guide_legend(title=NULL), shape=guide_legend(override.aes=list(size=2))) +
        scale_fill_brewer(breaks=c("request_packet_latency_ns_avg","queueing_latency_ns_avg","DRAM_latency_ns_avg", "response_packet_latency_ns_avg"), palette="Paired") + #guide=FALSE              # colour palette
        ylim(0,250) +
        ggtitle(paste(exp$workload, exp$workload_feature, sep="\n")) +
        xlab("DRAM") +
        ylab("latency(ns)") +
#        theme_bw() +
        theme(plot.title=element_text(size=10),
        axis.text.x=element_text(angle=30, size=5, vjust=0.2),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.text=element_text(size=5))

    if (i %% 6 == 0) {
      print(i)
      g <- arrangeGrob(grobs=plots)
      ggsave(file=paste(output_prefix, newpage_cnt , ".pdf", sep=""), g)
      ggsave(file=paste(output_prefix, newpage_cnt , ".png", sep=""), g)
      plots <- list()
      newpage_cnt <- newpage_cnt + 1
    }
}

if (length(plots) > 0) {
  g <- arrangeGrob(grobs=plots)
  ggsave(file=paste(output_prefix, newpage_cnt , ".pdf", sep=""), g)
  ggsave(file=paste(output_prefix, newpage_cnt , ".png", sep=""), g)
}
