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
requireNamespace("readr")
requireNamespace("knitr")
requireNamespace("dplyr")
requireNamespace("tidyr")

# ---- declare-globals ---------------------------------------------------------
baseSize <- 12

ls_terms <- list(
  "level" = c(
    "(R)-Intercept",
    "(F)-Intercept",
    "Baseline age",
    "Education",
    "Gender",
    "Systolic BP"
  ),
  "linear"= c(
    "(R)-Linear slope",
    "(F)-Linear slope",
    "L-slope*Baseline age",
    "L-slope*Education",
    "L-slope*Gender",
    "L-slope*Systolic BP"
  ),
  "quadratic" = c(
    "(R)-Quadratic slope",
    "(F)-Quadratic slope",
    "Q-slope*Baseline age",
    "Q-slope*Education",
    "Q-slope*Gender",
    "Q-slope*Systolic BP"
  ),
  "error" = c("(R)-Error"),
  "info_index"="BIC"

)
# ls_terms <- list(
#   "level" = c(
#     "(F)-Intercept",
#     "Baseline age",
#     "Education",
#     "Gender",
#     "Systolic BP",
#     "(R)-Intercept"
#   ),
#   "linear"= c(
#     "(F)-Linear slope",
#     "L-slope*Baseline age",
#     "L-slope*Education",
#     "L-slope*Gender", 
#     "L-slope*Systolic BP",
#     "(R)-Linear slope"
#   ),
#   "quadratic" = c(
#     "(F)-Quadratic slope",
#     "Q-slope*Baseline age",
#     "Q-slope*Education",
#     "Q-slope*Gender",
#     "Q-slope*Systolic BP",  
#     "(R)-Quadratic slope"
#   ),
#   "error" = c("(R)-Error"),
#   "misfit"="misfit_value"
#   
# )
# ----- define-scenarios --------------------------------
# SCENARIO 1
# 123, 1234, 12345

# SCENARIO 2
#  125, 135, 145

# SCENARION 3
# 1235, 1345, 1345

# ---- load-data ---------------------------------------------------------------
ls_catalog <- readRDS("./data/shared/derived/ls_catalog.rds")
ds_catalog <- readRDS("./data/shared/derived/catalog.rds")
# stencil <- readr::read_csv("./data/shared/raw/table-stencil-octo.csv")
stencil <- readr::read_csv("./data/shared/raw/table-stencil-octo-3.csv")

# ---- inspect-data -------------------------------------------------------------
ds_catalog %>% dplyr::glimpse()
# ---- tweak-data --------------------------------------------------------------
ds_catalog <- ds_catalog %>% 
  dplyr::mutate( 
    model_set = ifelse(model_number=="u1",paste0("L-",wave_set),
                       ifelse(model_number=="u2",paste0("Q-",wave_set),NA)),
    model_set = factor(model_set, levels= c(
      "L-12345","L-1234","L-123",
      "L-1235", "L-1245", "L-1345",
      "L-125",  "L-135", "L-145",

      "Q-12345","Q-1234","Q-123",
      "Q-1235", "Q-1245","Q-1345",
      "Q-125",  "Q-135", "Q-145"
      )
    ),
    model_set = factor(model_set, levels = rev(levels(model_set))),
    label     = factor(label,     levels = stencil$label),
    label     = factor(label,     levels = rev(levels(label))),
    wave_set  = factor(wave_set,  levels = c(
      "12345","1234","123",
      "1345", "1245","1235",
      "125",  "135", "145"
      )
    ),
    wave_set     = factor(wave_set,rev(levels(wave_set))),
    model_number = factor(model_number,
                          levels = c("u1","u2"),
                          labels = c("Linear","Quadratic")
    ),
    term      = label,
    estimate  = est,
    conf.low  = estimate - 1.96*se,
    conf.high = estimate + 1.96*se,
    sign      = ifelse(pval<=.05,T,F)
  ) %>% 
  dplyr::filter(!is.na(wave_set))
# ---- basic-table --------------------------------------------------------------

# ----- graphical-settings -------------------------

# ---- basic-graph --------------------------------------------------------------
# review the data structure: 
# d <- ds_catalog %>%
#   dplyr::filter(
#     # model_number == "u2",
#     wave_set     %in% c("12345","1234","123","135"),
#     model_type   == "aefb",
#     process      == "mmse"
#   ) %>%
#   dplyr::mutate(
#     term = label,
#     estimate = est,
#     conf.low = estimate - 1.96*se,
#     conf.high = estimate + 1.96*se,
#     sign = ifelse(pval<=.05,T,F)
#   )

ls_design_conditions <- list(
  "Figure A" = c("12345","1234","123" ),
  "Figure B" = c("12345","1235","1245","1345"),
  "Figure C" = c("12345","125" ,"135", "145"),
  "Figure D" = c("12345","1235","1345","1245","125", "135", "145"),
  "Figure E" = c("12345","1235","1345","1245","125", "135", "145"),
  "Figure F" = c("12345","1234","123", "1235","1345","1245","125","135","145"),
  "Figure G" = c("12345","1234","123", "1235","1345","1245","125","135","145")
)

ls_model_shapes <- list(
  "Figure A" = c("Linear","Quadratic"),
  "Figure B" = c("Linear","Quadratic"),
  "Figure C" = c("Linear","Quadratic"),
  "Figure D" = c("Linear"            ),
  "Figure E" = c(         "Quadratic"),
  "Figure F" = c("Linear"            ),
  "Figure G" = c(         "Quadratic")
)

print_figure <- function(x,figure_name, folder){
  # figure_name <- "Figure A"
  # define filter criteria
  conditions  <- ls_design_conditions[[figure_name]]
  model_shapes <- ls_model_shapes[[figure_name]]
  filter_criteria_condition <- lazyeval::interp(~ which_column %in% conditions, which_column = as.name("wave_set"))
  filter_criteria_modelshape <- lazyeval::interp(~ which_column %in% model_shapes, which_column = as.name("model_number"))
  # print the graph
  super_matrix(
    x = ds_catalog %>%
      dplyr::filter_(filter_criteria_condition) %>% 
      dplyr::filter_(filter_criteria_modelshape) 
    ,folder_name = folder
    ,process     = "mmse"
    ,model_type  = "aefb"
    ,suffix      = figure_name
    ,width       = 2800
    ,height      = 2000
    ,res         = 210
    )
}

# ---- print-measure ----------------------
ds_catalog %>% print_figure("Figure A", "./reports/visual-evidence/prints/")
ds_catalog %>% print_figure("Figure B", "./reports/visual-evidence/prints/")
ds_catalog %>% print_figure("Figure C", "./reports/visual-evidence/prints/")
ds_catalog %>% print_figure("Figure D", "./reports/visual-evidence/prints/")
ds_catalog %>% print_figure("Figure E", "./reports/visual-evidence/prints/")
ds_catalog %>% print_figure("Figure F", "./reports/visual-evidence/prints/")
ds_catalog %>% print_figure("Figure G", "./reports/visual-evidence/prints/")

