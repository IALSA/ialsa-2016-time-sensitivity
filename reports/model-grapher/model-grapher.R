# knitr::stitch_rmd(script="./___/___.R", output="./___/stitched-output/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------
source("./scripts/functions-graphs.R")
source("./scripts/functions-tables.R")
source("./scripts/graph-presets.R") # pre-sets and options for graphing

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2) # graphs
library(MplusAutomation)
library(tidyverse)
requireNamespace("readr")
requireNamespace("knitr")
requireNamespace("dplyr")
requireNamespace("tidyr")

# ---- declare-globals ---------------------------------------------------------
baseSize <- 12
colors_wave_set <- c(
  "12345"  = "#000000"
  ,"1234"  = "#1b9e77"
  ,"1235"  = "#d95f02"
  ,"1245"  = "#7570b3"
  ,"1345"  = "#e7298a"
  ,"123"   = "#7fc97f"
  ,"125"   = "#beaed4"
  ,"145"   = "#fdc086"
  ,"134"   = "#ffff99"
  ,"135"   = "#386cb0"
)

ls_terms <- list(
  "level" = c("(R)-Intercept",
              "(F)-Intercept",
              "Baseline age",
              "Education",
              "Gender",
              "Systolic BP"),
  "linear"= c("(R)-Linear slope",
              "(F)-Linear slope",
              "L-slope*Baseline age",
              "L-slope*Education",
              "L-slope*Gender", 
              "L-slope*Systolic BP" ),
  "quadratic" = c( "(R)-Quadratic slope",
                   "(F)-Quadratic slope",
                   "Q-slope*Baseline age",
                   "Q-slope*Education",
                   "Q-slope*Gender",
                   "Q-slope*Systolic BP"  ),
  "error" = c("(R)-Error")
)


# ---- load-data ---------------------------------------------------------------
ls_catalog <- readRDS("./data/shared/derived/ls_catalog.rds")
ds_catalog <- readRDS("./data/shared/derived/catalog.rds")
# stencil <- readr::read_csv("./data/shared/raw/table-stencil-octo.csv")
stencil <- readr::read_csv("./data/shared/raw/table-stencil-octo-3.csv")

# ---- understand-structure -------------------
# ls_catalog is a list object, each element = model
names(ls_catalog) %>% head(10)
ls_one_model <- ls_catalog[["u1_12345_aefb_block"]]
# each model contains
names(ls_one_model)
# now we will extract a data set from each model
# and prepare it for graphing


# ---- utility-functions ---------------
quick_save <- function(g,name,width=1200,height=400,dpi=300){
  ggplot2::ggsave(
    filename= paste0(name,".png"), 
    plot=g,
    device = png,
    path = "./reports/model-grapher/graphs-1/",
    width = width,
    height = height,
    # units = "cm",
    dpi = dpi,
    limitsize = FALSE
  )
}


# ---- basic-graph -------------
centers <- c(
  "age" = 80,
  "edu" = 7,
  "sbp" = 167
)

# ds_long <- ls_catalog %>% 
#   get_model_data("u1_12345_aefb_block") %>% 
#   prep_for_graph(centers)
# 
# ds_long %>% plot_trajectories("age_at_visit",100)
sample_size <- "max"
# sample_size <- 100

ls_catalog %>% 
  trajectory_matrix("u1_12345_aefb_mmse",sample_size) %>% 
  quick_save("L-12345")

ls_catalog %>% 
  trajectory_matrix("u2_12345_aefb_mmse", sample_size) %>% 
  quick_save("Q-12345")

ls_catalog %>% 
  trajectory_matrix("u1_1234_aefb_mmse", sample_size) %>% 
  quick_save("L-1234")

ls_catalog %>% 
  trajectory_matrix("u2_1234_aefb_mmse", sample_size) %>% 
  quick_save("Q-1234")

ls_catalog %>% 
  trajectory_matrix("u1_123_aefb_mmse", sample_size) %>% 
  quick_save("L-123")

ls_catalog %>% 
  trajectory_matrix("u2_123_aefb_mmse", sample_size) %>% 
  quick_save("Q-123")

ls_catalog %>% 
  trajectory_matrix("u1_135_aefb_mmse", sample_size) %>% 
  quick_save("L-135")

ls_catalog %>% 
  trajectory_matrix("u2_135_aefb_mmse", sample_size) %>% 
  quick_save("Q-135")






