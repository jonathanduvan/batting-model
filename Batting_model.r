rm(list = ls())
library(tidyverse)
library(retrosheet)


setwd("~/Downloads/")
??retrosheet


df <- getRetrosheet("play", 2019, "SFN")[[1]]
df1 <- getRetrosheet("play", 2019, "SFN")[[2]]
df2 <- getRetrosheet("play", 2019, "SFN")[[3]]
df3 <- getRetrosheet("play", 2019, "SFN")[[4]]
df4 <- getRetrosheet("play", 2019, "SFN")[[5]]
df5 <- getRetrosheet("play", 2019, "SFN")[[6]]
df6 <- getRetrosheet("play", 2019, "SFN")[[7]]
df7 <- getRetrosheet("play", 2019, "SFN")[[8]]

df1 <- data.frame(df["play"])
class(df)




names(df)
