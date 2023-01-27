## Preprocess data, write TAF data tables

## Before: catch.csv, effort.csv, priors.csv (bootstrap/data)
## After:  catch_by_stock.png, catch_effort.csv, catch_relative.png,
##         catch_total.png, driors.pdf, input.rds (data)

library(TAF)
taf.library(SOFIA)
suppressMessages(library(dplyr))  # filter, group_by, mutate, summarise, ungroup
library(ggplot2)  # aes, geom_line, geom_point, ggplot, ggsave, ggtitle
library(sraplus)  # plot_driors
library(tidyr)    # nest

mkdir("data")

## Read catch data, convert to long format
catch <- read.taf("bootstrap/data/catch.csv")
catch$Total <- NULL  # not used, not a stock
catch <- taf2long(catch, c("year", "stock", "capture"))

## Plot catch
catch %>%
  group_by(year) %>%
  summarise(total_capture=sum(capture)) %>%
  ggplot(aes(year, total_capture)) +
  geom_line()
ggsave("data/catch_total.png", width=12, height=6)

## Plot catch by stock
catch %>%
  ggplot(aes(year, capture, color=stock)) +
  geom_line(show.legend=FALSE) +
  geom_point()
ggsave("data/catch_by_stock.png", width=12, height=6)

## Select stocks with min 10 years of non-zero catches...
viable_stocks <- catch %>%
  group_by(stock) %>%
  summarise(n_pos_catch=sum(capture > 0.1)) %>%
  filter(n_pos_catch > 10)

## ...and discard zero-catch years at the beginning or end of series
catch <- catch %>%
  filter(stock %in% viable_stocks$stock) %>%
  group_by(stock) %>%
  filter(year >= min(year[capture > 0.1]),
         year <= max(year[capture > 0.1]))

## Plot relative catch
catch %>%
  group_by(stock) %>%
  mutate(capture = capture / max(capture)) %>%
  ggplot(aes(year, capture, group=stock)) +
  geom_point()
ggsave("data/catch_relative.png", width=12, height=6)

## Read effort data, combine catch and effort data
effort <- read.taf("bootstrap/data/effort.csv")
effort <- taf2long(effort, c("year", "stock", "effort"))
catch_effort <- addEffort(catch, effort, same.effort=FALSE)

## Create nested tibble with 'data' column (catch and effort)
stocks <- catch_effort %>%
  group_by(stock) %>%
  nest() %>%
  ungroup()

## Read priors data, add as driors to stocks object
priors <- read.taf("bootstrap/data/priors.csv")
stocks <- addDriors(stocks, priors, same.priors=TRUE)

## Plot driors
pdf("data/driors.pdf")
for(i in seq_len(nrow(stocks)))
{
  suppressWarnings(print(plot_driors(stocks$driors[[i]]) +
                         ggtitle(stocks$stock[i])))
}
dev.off()

## Export stocks and catch_effort
saveRDS(stocks, "data/input.rds")
write.taf(catch_effort, dir="data")
