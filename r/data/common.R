library(tidyverse)
library(scales)
library(frenchdata)
library(RSQLite)
library(RPostgres)
library(dbplyr)

start_date <- ymd("1996-01-01")
end_date <- ymd("2020-12-31")
