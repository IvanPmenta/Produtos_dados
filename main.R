library(here)
library(tidyverse)
library(tsibble)
library(forecast)

path <- here()
dados <- read_csv(here(path, "sales.csv"))

dadoslimpos <- cleaning(dados)

dadosmodelados <- modeling(dadoslimpos, "ARIMA", 5, 1, 1, 1, 1, 1, 4, 6)

tema <- theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 1, face = "italic"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
