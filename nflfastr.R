library(nflfastR)
library(nflreadr)
library(tidyverse)
library(ggrepel)
library(ggimage)

options(scipen = 9999)

data <- load_pbp(2020)

pbp <- nflfastR::load_pbp(2018:2020, qs = TRUE)
#> The "qs" argument is deprecated and replaced by "file_type" - see
#> `?nflreadr::load_pbp` for details.
#> Warning: The `qs` argument of `load_pbp()` is deprecated as of nflfastR 4.3.0.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.