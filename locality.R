# The data is read from a csv file, grouped in columns: Cultivar, Date, Weight, sd, n, se

library(argparse)

parser <- ArgumentParser(description="Process some integers")
parser$add_argument("--dir", required=TRUE, help="input dir that contains the csv file and also the output dir for plots")

args <- parser$parse_args()

print(args$dir)

csv_path <- paste(args$dir, "/locality.csv", sep="")
output_prefix <- paste(args$dir, "/locality", sep="")

print(csv_path)

locality_exp <- read.csv(csv_path)

# ---
# Stacked Bar Graph
# -----------------

library(dplyr)
#library(plyr)
library(ggplot2)
library(gridExtra)

workload_list <- unique(locality_exp$workload)

plots <- list()

workload_num <- length(workload_list)
newpage_cnt <- 0

for (i in 1:workload_num)
{
    workload_name <- workload_list[i]
    exp <- filter(locality_exp, workload == workload_name)
#    print(exp)

    # Create the plot using `Date` on the x axis, `Weight` on the Y axis and the column `Cultivar` to determine the right colour

    plots[[length(plots) + 1]]<-ggplot(exp, aes(x=DRAM, y=value, fill=statistics)) +
        geom_bar(stat="identity", colour='black') +     # contour colour
#        guides(fill=guide_legend(title=NULL)) +
        scale_fill_brewer(breaks=c("row_conflict_rate","row_miss_rate","row_hit_rate"), palette="Paired", guide=FALSE) +              # colour palette
        ylim(0,1.01) +
        ggtitle(paste(exp$workload, exp$workload_feature, sep="\n")) +
        theme_bw() +
        theme(plot.title=element_text(size=10),
            axis.text.x=element_text(angle=30, size=5, vjust=0.5),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())

    if (i %% 11 == 0) {
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
