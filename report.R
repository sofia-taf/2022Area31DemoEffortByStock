## Prepare plots and tables for report

## Before: sofia20_proportions.csv (bootstrap/data), results.rds (model),
##         current_status.csv, stock_timeseries.csv (output)
## After:  b_over_bmsy.png, status_count.png, status_proportion.png,
##         status_summary.png, stock_cpue.pdf, stock_posterior.pdf,
##         stock_timeseries.pdf (report)

library(TAF)
taf.library(SOFIA)
suppressMessages(library(egg))  # ggarrange
library(ggplot2)  # geom_hline, geom_line, ggplot, ggsave, ggtitle
library(sraplus)  # plot_prior_posterior, plot_sraplus

mkdir("report")

## Establish factor order
levels <- c("Underfished", "Fully fished", "Overfished")

stocks <- readRDS("model/results.rds")
stock.timeseries <- read.taf("output/stock_timeseries.csv")
current_status <- read.taf("output/current_status.csv")
last_sofia <- read.taf("bootstrap/data/sofia20_proportions.csv")
current_status$status <- ordered(current_status$status, levels=levels)
last_sofia$Category <- ordered(last_sofia$Category, levels=levels)

## B over Bmsy
ggplot(stock.timeseries, aes(x=year, y=bbmsy, colour=stock, group=stock)) +
  geom_line(show.legend=TRUE) +
  geom_hline(yintercept=0.8, linetype="dashed", color="red", linewidth=2) +
  geom_hline(yintercept=1.2, linetype="dashed", color="green", linewidth=2)
ggsave("report/b_over_bmsy.png", width=12, height=6)

## Status count and proportion
taf.png("status_count")
p1 <- plotCat(stock.timeseries, method="effEdepP", cats=3, type="count")
p2 <- plotCat(stock.timeseries, method="effEdepP", cats=3, type="stock")
ggarrange(p1, p2, ncol=1)
dev.off()
taf.png("status_proportion", width=1800, height=1000)
plotCat(stock.timeseries, method="effEdepP", cats=3, type="prop")
dev.off()

## Status summary: current analysis and last SOFIA
taf.png("status_summary", width=1800, height=1000)
par(mfrow=c(1,2))
barplot(Proportion~Category, last_sofia, col=c(3,7,2), ylim=c(0,1),
        xlab="Last SOFIA")
barplot(prop.table(table(current_status$status)), col=c(3,7,2), ylim=c(0,1),
        xlab="Current analysis", ylab="Proportion")
dev.off()

## CPUE
pdf("report/stock_cpue.pdf")
for(i in seq_len(nrow(stocks)))
{
  x <- stocks$driors[[i]]$effort_years
  y <- with(stocks$driors[[i]], catch[years %in% x] / effort)
  plot(x, y, ylim=lim(y), main=stocks$stock[i], xlab="", ylab="CPUE", type="l")
}
dev.off()

## Stock posteriors, along with data and priors
pdf("report/stock_posterior.pdf")
for(i in seq_len(nrow(stocks)))
{
  p <- plot_prior_posterior(stocks$sraplus_fit[[i]], stocks$driors[[i]])
  suppressWarnings(print(p + ggtitle(stocks$stock[i])))
}
dev.off()

## Stock time series
pdf("report/stock_timeseries.pdf")
for(i in seq_len(nrow(stocks)))
  print(plot_sraplus(stocks$sraplus_fit[[i]]) + ggtitle(stocks$stock[i]))
dev.off()
