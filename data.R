## Preprocess data, write TAF data tables

## Before: catch.csv, effort.csv (bootstrap/data)
## After:  catch_effort.csv, catch_by_stock.png, catch_relative.png,
##         catch_total.png, driors_2.png, input.rds (data)

library(TAF)
taf.library(SOFIA)
library(dplyr)   # filter, group_by, left_join, mutate, summarise, ungroup
library(ggplot2)
library(purrr)   # map2
library(sraplus) # format_driors, plot_driors
library(tidyr)   # nest, pivot_longer

mkdir("data")

stocks.combined <- FALSE

## Read catch data, convert to tibble (long format)
catch <- read.taf("bootstrap/data/catch.csv")
catch$Total <- NULL  # not used, not a stock
catch <- pivot_longer(catch, -Year, "stock", values_to="capture")
names(catch) <- tolower(names(catch))

## Plot catches
catch %>%
  ggplot(aes(year, capture, color=stock)) +
  geom_line(show.legend=FALSE) +
  geom_point()
ggsave("data/catch_by_stock.png")
catch %>%
  group_by(year) %>%
  summarise(total_capture=sum(capture)) %>%
  ggplot(aes(year, total_capture)) +
  geom_line()
ggsave("data/catch_total.png")

## Select stocks with min 10 years of non-zero catches...
viable_stocks <- catch %>%
  group_by(stock) %>%
  summarise(n_pos_catch=sum(capture > 0)) %>%
  filter(n_pos_catch > 10)

## ...and discard zero-catch years at the beginning or end of series
catch <- catch %>%
  filter(stock %in% viable_stocks$stock) %>%
  group_by(stock) %>%
  filter(year > min(year[capture > 0]),
         year <= max(year[capture > 0]))

## Plot relative catch
catch %>%
  group_by(stock) %>%
  mutate(capture = capture / max(capture)) %>%
  ggplot(aes(year, capture, group=stock)) +
  geom_point()
ggsave("data/catch_relative.png")

## Add column 'taxa'
catch$taxa <- catch$stock

## Read effort data, add column to catch data
effort <- read.taf("bootstrap/data/effort.csv")
effort <- pivot_longer(effort, -Year, "stock", values_to="effort")
names(effort) <- tolower(names(effort))
catch_effort <- addEffort(catch, effort, stocks.combined)

## Create nested tibble with 'data' column (catch and effort)
stocks <- catch_effort %>%
  group_by(stock, taxa) %>%
  nest() %>%
  ungroup()

## Read priors data, add as driors to stocks object
priors <- read.taf("bootstrap/data/priors.csv")
##Modified addDriors
addDriors<-function (stocks, priors, stocks.combined, shape_prior = 2, b_ref_type = "k", 
                     growth_rate_prior = NA, growth_rate_prior_cv = 0.2, ...) 
{
  driors <- list()
  for (i in seq_len(nrow(stocks))) {
    p <- if (stocks.combined) 
      match("All", priors$stock)
    else match(stocks$stock[i], priors$stock)
    driors[[i]] <- format_driors(taxa = stocks$taxa[i], shape_prior = 2, 
                                 catch = stocks$data[[i]]$capture, years = stocks$data[[i]]$year, 
                                 initial_state = priors$initial_state[p], initial_state_cv = priors$initial_state_cv[p], 
                                 b_ref_type = "k", 
                                 #terminal_state = priors$terminal_state[p], 
                                 #terminal_state_cv = priors$terminal_state_cv[p], 
                                 effort = stocks$data[[i]]$effort, 
                                 effort_years = na.omit(stocks$data[[i]])$year, growth_rate_prior = NA, 
                                 growth_rate_prior_cv = 0.2, ...)
  }
  stocks$driors <- driors
  stocks
}


stocks <- addDriors(stocks, priors, stocks.combined)

## Plot driors for one stock
plot_driors(stocks$driors[[2]])
ggsave("data/driors_2.png")

## Export stocks and catch_effort
saveRDS(stocks, "data/input.rds")
write.taf(catch_effort, dir="data")

