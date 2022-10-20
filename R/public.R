library("tidyverse")
library("dplyr")
library("usethis")
library("devtools")
library("roxygen2")


drg_boxplot <- function(varname, dataset="./DRG_data.csv") {
  data <- read.csv(dataset)
  data %>%
    mutate(drgCode = substr(DRG.Definition, 1,3)) %>%
    rename(groupvar = all_of(varname)) %>%
    ggplot(aes(y=groupvar, x=drgCode)) +
    geom_boxplot()
}



drg_med_stat <- function(stat_type, dataset="./DRG_data.csv") {
  data = read.csv(dataset)
  drg_stat <- data %>%
    mutate(drgCode = substr(DRG.Definition, 1,3)) %>%
    summarise(mean=mean(Average.Medicare.Payments),
              sd=sd(Average.Medicare.Payments),
              median=median(Average.Medicare.Payments))

  output <- drg_stat %>%
    rename(gvar = all_of(stat_type)) %>%
    select(gvar)

  head(output)[[1]]

}
