# Library ---------------------------------------------------------------
library(DBI)
library(tidyverse)
library(data.table)
library(clickhouse)
library(clckhs)

# Northwind -------------------------------------------------------------
# Connect to server
srv <- 'ssh miguel@fundamentals.rnfc.org'
system(srv)
system("ssh miguel@fundamentals.rnfc.org")
