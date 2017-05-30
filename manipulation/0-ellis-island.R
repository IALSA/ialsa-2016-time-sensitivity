rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
source("./scripts/functions-graphs.R")
source("./scripts/functions-tables.R")
source("./scripts/graph-presets.R") # pre-sets and options for graphing

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2)
library(MplusAutomation)
requireNamespace("readr")
requireNamespace("knitr")
requireNamespace("dplyr")
requireNamespace("tidyr")


# ---- declare-globals ---------------------------------------------------------
path_folder <- "./output/studies/octo"
path_stencil      <- "./data/shared/raw/table-stencil-octo.csv"

# path_newc_out   <- "./manipulation/estimation/newcastle/newcastle-mmse-k-2.out"
# path_octo_out <- "./manipulation/estimation/octo/octo-mmse-k-2.out"

baseSize <- 12


# ---- load-data ---------------------------------------------------------------
path_outputs <- list.files(path_folder,pattern = ".out$",full.names = T, recursive = T)
# stencil <- readr::read_csv("./data/shared/raw/table-stencil-octo-2.csv")
stencil <- readr::read_csv("./data/shared/raw/table-stencil-octo-3.csv") # shorter names


path <- path_outputs#[639]
# grep("./output/studies/octo/info/u2_145_aef_info.out",path,value=F)
# ---- assemble-catalog -------------------------------
# create catalog list
# ls_catalog <- list()
# regex_1 <- "^(u1|u2)_(\\d+)_(\\w+)_(\\w+)"
# for(i in seq_along(path)){
#   # i <- 1
#   model_name <- gsub(".out$","",basename(path[i]))
#   model_result <- MplusAutomation::readModels(path[i])
#   if(length(model_result$errors)==0L){
#      ls_temp <- list(
#       "model_number" =  gsub(regex_1, "\\1", model_name),
#       "wave_set"     =  gsub(regex_1, "\\2", model_name),
#       "model_type"   =  gsub(regex_1, "\\3", model_name),
#       "process"      =  gsub(regex_1, "\\4", model_name),
#       "table"        =  get_estimate_table(model_result),
#       "N"            = model_result$summaries$Observations,
#       "parameters"   = model_result$summaries$Parameters,
#       "AIC"          = model_result$summaries$AIC,
#       "BIC"          = model_result$summaries$BIC,
#       "path"         =  path[i]
#     )
#   } else{
#     ls_temp <- list(
#       "model_number" = gsub(regex_1, "\\1", model_name),
#       "wave_set"     = gsub(regex_1, "\\2", model_name),
#       "model_type"   = gsub(regex_1, "\\3", model_name),
#       "process"      = gsub(regex_1, "\\4", model_name),
#       "table"        = NA,
#       "N"            = NA,
#       "parameters"   = NA,
#       "AIC"          = NA,
#       "BIC"          = NA,
#       "path"         = NA
#     )
#   }
#    ls_catalog[[model_name]] <- ls_temp
# }
# saveRDS(ls_catalog,"./data/shared/derived/ls_catalog.rds")
ls_catalog <- readRDS("./data/shared/derived/ls_catalog.rds")
#
ds_catalog <- plyr::ldply(ls_catalog, data.frame, .id = "model_name")
names(ds_catalog) <- gsub("^table.","",names(ds_catalog))
saveRDS(ds_catalog,"./data/shared/derived/catalog.rds")

ds_catalog <- readRDS("./data/shared/derived/catalog.rds")

table(ds_catalog$wave_set)

# @knitr reproduce ---------------------------------------
#   rmarkdown::render(input = "./reports/report.Rmd" ,
#                     output_format="html_document", clean=TRUE)
