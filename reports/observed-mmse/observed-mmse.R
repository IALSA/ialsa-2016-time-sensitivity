# knitr::stitch_rmd(script="./___/___.R", output="./___/stitched-output/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------
source("./scripts/functions-graphs.R")
source("./scripts/functions-tables.R")
source("./scripts/graph-presets.R") # pre-sets and options for graphing
source("./scripts/functions-missing.R")

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2) # graphs
library(MplusAutomation)
library(dplyr)
requireNamespace("readr")
requireNamespace("knitr")
requireNamespace("dplyr")
requireNamespace("tidyr")

# ---- declare-globals ---------------------------------------------------------
path_input <- "./data/unshared/derived/octo/data-long-plus.rds"
# ---- load-data -------------------------------------------------------------
ds_wide <- readRDS(path_input)

# ---- inspect-data -------------------------------------------------------------
ds_wide %>% dplyr::glimpse()

# ---- utility-functions ---------------------------------------------------
# local to the script
quick_save <- function(
  g,            # ggplot object to be saved
  name,         # name of the file to be saved   
  width  = 800, # width in pixels  
  height = 1100, # height in pixesl  
  dpi    = 400  # resolution, dots per inch 
){
  ggplot2::ggsave(
    plot=g,
    # filename= paste0(name,".jpg"), 
    # device = jpg,
    filename= paste0(name,".png"),
    device = png,
    path = "./reports/observed-mmse/",
    width = width,
    height = height,
    # units = "cm",
    dpi = dpi,
    limitsize = FALSE
  )
}


# ---- tweak-data --------------------------------------------------------------
colnames(ds_wide) <- tolower(colnames(ds_wide))

static_variables <-  c("case", "pairid","twinid","female", "compage1",
                       "educyrs","height1","smoke","cvd1","diabyn1",
                       "demever", "sbp1")
dynamic_variables <- setdiff(colnames(ds_wide),static_variables)
ds_long <- ds_wide %>% 
  tidyr::gather_("variable","value", dynamic_variables) %>% 
  # tibble::as.tibble() %>% 
  dplyr::mutate(
    wave     = gsub("^(\\w+?)_(\\d+)", "\\2", variable) %>% as.integer(),
    variable = gsub("^(\\w+?)_(\\d+)", "\\1", variable)
  ) %>% 
  tidyr::spread(variable, value) %>% 
  dplyr::rename(
    id           = case,
    age_at_visit = compage1,
    edu_years    = educyrs,
    height_cm    = height1,
    cvd          = cvd1,
    diabetes     = diabyn1,
    sbp          = sbp1
  ) %>% 
  dplyr::select(
     id
    # ,pairid
    # ,twinid
    ,female
    ,height_cm
    ,edu_years
    ,smoke
    ,sbp
    ,cvd
    ,diabetes
    ,demever
    ,wave
    ,age_at_visit
    ,years_since_bl
    ,mmse
    # ,dplyr::everything()
  ) %>% 
  dplyr::arrange(id)
str(ds_long)
# make all na the same
ds_long[is.na(ds_long)] <- NA
# structure and inspection
ds_long <- ds_long %>% tibble::as_tibble()
ds_long %>% dplyr::glimpse()
ds_long %>% distinct(id) %>% count()

# exploration into missingness
# ds_long %>% missing_summary()
# ds_long %>% show_missing_points(xvar="wave", yvar = "mmse")
# ds_long %>% show_missing_points(yvar="wave", xvar = "mmse")
# ds_long %>% expose_missing(pivot = "wave", varname = "mmse")
# ds_long %>% expose_missing(pivot = "mmse", varname = "wave")
# ds_long %>% expose_missing(pivot = "age_at_visit", varname = "mmse")
# ds_long %>% missing_counts_var(print_table = T, sort = T)
# 
# ds_long %>% comissing_raster()

# ---- tally-response-patterns -----------------

# count frequencies of all distinct patterns of response on MMSE
d1 <- ds_long %>%   
  dplyr::select(id, wave, mmse) %>% 
  dplyr::arrange(id) %>% 
  dplyr::group_by(id,wave) %>% 
  dplyr::summarize(
    # mmse = !is.na(mmse),
    # mmse = ifelse(is.na(mmse), ".", wave)
    mmse = ifelse(is.na(mmse), NA, wave)
  ) %>% 
  dplyr::mutate(
    wave = paste0("wave_",as.character(wave))
  ) %>% 
  tidyr::spread(wave, mmse) %>% 
  as.data.frame()
d1 %>% distinct(id) %>% count()
# replace all NA by a dot
d1[is.na(d1)] <- "."
d1 %>% tibble::as_tibble()
# construct the response vector as a character value
d2 <- d1 %>% 
  dplyr::mutate(
    response_pattern = paste(wave_1, wave_2, wave_3, wave_4, wave_5, sep = "-")
  ) 
# compute frequencies of pattern occurence
d3 <- d2 %>% 
  dplyr::group_by(wave_1, wave_2, wave_3, wave_4, wave_5, response_pattern) %>% 
  dplyr::summarize(n_people = n()) %>% 
  dplyr::arrange(desc(wave_1), desc(wave_2), desc(wave_3), desc(wave_4), desc(wave_5))
# print the table
d3 %>% neat(output_format = "pandoc")
#  augment original data with response pattern
ds_long <- ds_long %>% 
  dplyr::left_join( d2 %>% dplyr::select(id, response_pattern), by = "id")
# rm(list = c("d1","d2","d3"))

# ---- graphing-functions --------------------------------------------------------------
# define simple plot, to be tiled in a matrix
plot_trajectories <- function(
  d,
  time_var,
  sample_size=100
){
  # dd <- d
  # d <- ds_long
  # time_var = "years_since_bl"
  # # time_var = "wave"
  # sample_size = 100
  # 
  if(!sample_size=="max"){
    set.seed(42)
    ids <- sample(unique(d$id),sample_size)
    dd <- d %>% dplyr::filter(id %in% ids)
  }else{dd <- d}
  # compute sample size
  n_people <- length(unique(dd$id))
  # dd <- d
  g1 <-  dd %>%  
    ggplot2::ggplot(aes_string(x=time_var,y="mmse")) +
    geom_smooth(method="loess", color="black",size=1, fill="black", alpha=.2, linetype="solid", na.rm=T, span=1.5)+
    geom_line(aes(group=id),size=.5,alpha=.06)+
    geom_point(size=2.5, alpha=.4, shape =21)+
    geom_rug(size = 1, sides = "b", alpha = .1)+
    scale_y_continuous(limits = c(-.5,30.5), breaks=seq(-0,30,5))+
    scale_x_continuous(limits = c(0,9), breaks=seq(0,8,2),minor_breaks = seq(0,8,1))+
    geom_text(x=.2, y=1, label = paste0("N = ",scales::comma(n_people)))+
    main_theme+
    theme(text = element_text(size=baseSize+4)) +
    labs(x = "Years since baseline", y = "Mini Mental State Exam")
 return(g1)
}
# usage demo:
# ds_long %>% 
#   plot_trajectories(time_var = "years_since_bl", sample_size = 100) 

# define complext plot, matrix of simple views
matrix_plot <- function(
   d, # ds_long
   patterns
){
  # create a list of plots to facet with ggmatrix
  ls <- list()
  # patterns <- c("1-2-3-.-.", "1-2-3-4-.", "1-2-3-4-5")
  for(pat in patterns){
    ls[[pat]] <- ds_long %>% 
      dplyr::filter(response_pattern == pat) %>% 
      plot_trajectories(time_var = "years_since_bl", sample_size = "max") 
  }
  # place the plots into a single ggmatrix
  mplot <- GGally::ggmatrix(
    ls,
    ncol = 1, nrow = length(patterns),
    title = "Observed MMSE scores for three types of response patterns",
    yAxisLabels = patterns,
    # yAxisLabels = c("0-2-4", "0-2-4-6", "0-2-4-6-8"), 
    xlab = "Years since baseline", ylab = "Mini Mental State Exam (MMSE) Score"
    # xAxisLabels = "MMSE score",
    # legend = 1
  ) + theme(
    legend.position = "right",
    strip.text.x = element_text(size=baseSize+2)
    
  )
  mplot
}
# usage demo:
# ds_long %>% matrix_plot()


# Sonata form report structure
# ---- print-displays ---------------------------------
# print the matrix plot
g <- ds_long %>% 
  matrix_plot( patterns = c("1-2-3-.-.", "1-2-3-4-.", "1-2-3-4-5") )

g %>% 
  quick_save(
    name  = "prints/figure-1"
  )

# g <- ds_long %>% 
#   matrix_plot() 

ds_long %>% 
  matrix_plot( patterns = c("1-.-.-.-.", "1-2-.-.-.", "1-2-3-4-5") ) %>% 
  quick_save(
    name  = "prints/figure-2"
  )

# ---- place-displays ---------------------------------
ds_long %>% 
  matrix_plot( patterns = c("1-2-3-.-.", "1-2-3-4-.", "1-2-3-4-5") ) 


ds_long %>% 
  matrix_plot( patterns = c("1-.-.-.-.", "1-2-.-.-.", "1-2-3-4-5") ) 

# ---- publish ---------------------------------------
path_report_1 <- "./reports/observed-mmse/observed-mmse.Rmd"

allReports <- c(path_report_1)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}

