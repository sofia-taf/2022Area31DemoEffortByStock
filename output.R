## Extract results of interest, write TAF output tables

## Before: results.rds (model)
## After:  stock_tables/*.csv, current_status.csv, stock_timeseries.csv (output)

library(TAF)
suppressMessages(library(dplyr))  # case_when, count, filter, group_by, ...
library(tidyr)  # pivot_wider, unnest

mkdir("output")

## Read model results and make available in 'output' folder
stocks <- readRDS("model/results.rds")

## Categorize stock status by comparing B/Bmsy to 0.8 and 1.2
current_status <- stocks %>%
  select(stock, sraplus_summary) %>%
  unnest(cols = sraplus_summary) %>%
  filter(variable == "b_div_bmsy") %>%
  mutate(status = case_when(mean > 1.2 ~ "Underfished",
                            mean > 0.8 ~ "Fully fished",
                            TRUE ~ "Overfished"))
write.taf(current_status, "output/current_status.csv")
table(current_status$status)

## Write stock tables containing model results by year
mkdir("output/stock_tables")
for(i in seq_len(nrow(stocks))){
  filename <- paste0(chartr(" ", "_", stocks$stock[i]), ".csv")
  write.taf(stocks$sraplus_fit[i][[1]]$results,
            file=file.path("output/stock_tables", filename))
}

## Examine diagnostics
stocks$sraplus_diagnostics[[1]]

## Tabulate B/Bmsy and F/Fmsy time series for each stock
stock.timeseries <- list()
for(i in seq_len(nrow(stocks))){
  tmp <- stocks$sraplus_fit[[i]]$results %>%
    filter(variable %in% c("b_div_bmsy", "u_div_umsy")) %>%
    pivot_wider(id_cols="year", names_from="variable", values_from="mean")
  stock.timeseries[[i]] <- cbind(stock=stocks$stock[i], tmp)
}
stock.timeseries <- do.call(rbind, stock.timeseries)
names(stock.timeseries) <- c("stock", "year", "bbmsy", "ffmsy")
stock.timeseries$bbmsy.effEdepP <- stock.timeseries$bbmsy
stock.timeseries$ffmsy.effEdepP <- stock.timeseries$ffmsy
write.taf(stock.timeseries, "output/stock_timeseries.csv")
