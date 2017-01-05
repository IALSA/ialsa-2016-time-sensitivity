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
              "Linear slope*Baseline age",
              "Linear slope*Education",
              "Linear slope*Gender", 
              "Linear slope*Systolic BP" ),
  "quadratic" = c( "(R)-Quadratic slope",
                   "(F)-Quadratic slope",
                   "Quadratic slope*Baseline age",
                   "Quadratic slope*Education",
                   "Quadratic slope*Gender",
                   "Quadratic slope*Systolic BP"  ),
  "error" = c("(R)-Error")
)

# ---- load-data ---------------------------------------------------------------
ls_catalog <- readRDS("./data/shared/derived/ls_catalog.rds")
ds_catalog <- readRDS("./data/shared/derived/catalog.rds")
# stencil <- readr::read_csv("./data/shared/raw/table-stencil-octo.csv")
stencil <- readr::read_csv("./data/shared/raw/table-stencil-octo-2.csv")

# ---- inspect-data -------------------------------------------------------------
ds_catalog %>% dplyr::glimpse()
# ---- tweak-data --------------------------------------------------------------
ds_catalog <- ds_catalog %>% 
  dplyr::mutate( 
    model_set = ifelse(model_number=="u1",paste0("L-",wave_set),
                       ifelse(model_number=="u2",paste0("Q-",wave_set),NA)),
    model_set = factor(model_set, levels=c(
       "L-12345", "Q-12345"
      ,"L-1234",  "Q-1234" 
      ,"L-1235",  "Q-1235"
      ,"L-1245",  "Q-1245"
      ,"L-1345",  "Q-1345"
      ,"L-123",   "Q-123"
      ,"L-125",   "Q-125"
      ,"L-145",   "Q-145"
      ,"L-134",   "Q-134"
      ,"L-135",   "Q-135"   
    )),
    model_set = factor(model_set, levels = rev(levels(model_set))),
    label = factor(label,levels = stencil$label),
    label = factor(label, levels = rev(levels(label))),
    wave_set = factor(wave_set, levels=c(
      "12345","1234", "1235","1245","1345","123","125","145","134","135"
    )), 
    wave_set = factor(wave_set,rev(levels(wave_set))),
    model_number = factor(model_number, 
                          levels =c("u1","u2"),labels=c("Linear","Quadratic")),

      term = label,
      estimate = est,
      conf.low = estimate - 1.96*se,
      conf.high = estimate + 1.96*se,
      sign = ifelse(pval<=.05,T,F)
    
  )  
# ---- basic-table --------------------------------------------------------------

# ----- graphical-settings -------------------------



# ---- basic-graph --------------------------------------------------------------
# d <- ds_catalog %>%
#   dplyr::filter(
#     # model_number == "u2",
#     # wave_set     %in% c("12345","1234","123","135"),
#     model_type   == "aefb",
#     process      == "grip"
#   ) %>% 
#   dplyr::mutate(
#     term = label,
#     estimate = est,
#     conf.low = estimate - 1.96*se,
#     conf.high = estimate + 1.96*se,
#     sign = ifelse(pval<=.05,T,F)
#   )

super_matrix(ds_catalog,"aefb","mmse")


# ---- dummy ------------------

# plot_coef <- function (
#   x, 
#   mapping           = aes_string(
#     y     = "wave_set", 
#     x     = "estimate",
#     color = "sign",
#     # fill  = "wave_set", 
#     shape = "model_number"
#   ), 
#   conf.int          = FALSE, 
#   exclude_intercept = TRUE, 
#   vline             = TRUE, 
#   vline_intercept   = 0, 
#   vline_color       = "gray50", 
#   vline_linetype    = "dotted",
#   vline_size        = 1, 
#   errorbar_color    = "gray25", 
#   errorbar_height   = 0, 
#   errorbar_linetype = "solid", 
#   errorbar_size     = 0.5,
#   ...
# ){
#   if(exclude_intercept){
#     x <- x %>% dplyr::filter(!label == "(F)-Intercept")
#   }
#   # x = g_coef
#   g <-  ggplot2::ggplot(x, mapping = mapping) 
#   g <- g + geom_vline(xintercept = vline_intercept,
#                       color      = vline_color, 
#                       linetype   = vline_linetype, 
#                       size       = vline_size)
#   if(conf.int){
#     g <- g + geom_errorbarh(aes_string(xmin = "conf.low", xmax = "conf.high"),
#                             color = errorbar_color, 
#                             height = errorbar_height, 
#                             linetype = errorbar_linetype, 
#                             size = errorbar_size)
#   }
#   # g <- g + geom_point(shape = 21, size = 2, ...)
#   # g <- g + scale_shape_manual(values = c("TRUE"=21,"FALSE"=1))
#   g <- g + scale_shape_manual(values = c("Linear"=124,"Quadratic"=0))
#   g <- g + geom_point(size = 3, ...)
#   g <- g + scale_color_manual(values = c("TRUE"="red","FALSE"="black"))
#   g <- g + scale_fill_manual(values = colors_wave_set)
#   g <- g + facet_wrap(facets = "label")
#   # g <- g + geom_jitter(height=.5)
#   g
# }  
# 
# d %>% plot_coef() + main_theme


plot_coef <- function (
  x,
  mapping           = aes_string(
    y     = "term",
    x     = "estimate",
    color = "wave_set",
    fill  = "wave_set",
    shape = "sign"
  ),
  conf.int          = FALSE,
  exclude_intercept = TRUE,
  vline             = TRUE,
  vline_intercept   = 0,
  vline_color       = "gray50",
  vline_linetype    = "dotted",
  vline_size        = 1,
  errorbar_color    = "gray25",
  errorbar_height   = 0,
  errorbar_linetype = "solid",
  errorbar_size     = 0.5,
  ...
){
  if(exclude_intercept){
    x <- x %>% dplyr::filter(!label == "(F)-Intercept")
  }
  # x = g_coef
  g <-  ggplot2::ggplot(x, mapping = mapping)
  g <- g + geom_vline(xintercept = vline_intercept,
                      color      = vline_color,
                      linetype   = vline_linetype,
                      size       = vline_size)
  if(conf.int){
    g <- g + geom_errorbarh(aes_string(xmin = "conf.low", xmax = "conf.high"),
                            color = errorbar_color,
                            height = errorbar_height,
                            linetype = errorbar_linetype,
                            size = errorbar_size)
  }
  # g <- g + geom_point(shape = 21, size = 2, ...)
  # g <- g + scale_shape_manual(values = c("TRUE"=21,"FALSE"=1))
  g <- g + scale_shape_manual(values = c("TRUE"=21,"FALSE"=42))
  g <- g + geom_jitter(size = 4, alpha=.5, ...)
  g <- g + scale_color_manual(values = colors_wave_set)
  g <- g + scale_fill_manual(values = colors_wave_set)
  # g <- g + geom_jitter(height=.5)
  g
}

ds_catalog %>% 
  dplyr::filter(
    model_type == "aefb",
    model_number == "u1") %>% 
  plot_coef() + main_theme



# ----- select-measure ------------------------

# ----- define-printing-functions ----------------------


# ---- print-measure ----------------------
ds_catalog %>% spread_by_wave_set("aefb","mmse",c("L-123","Q-12345"))

