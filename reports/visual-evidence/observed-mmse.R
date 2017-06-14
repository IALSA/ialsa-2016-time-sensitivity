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
# ---- tweak-data --------------------------------------------------------------
colnames(ds_wide) <- tolower(colnames(ds_wide))

static_variables <-  c("case", "pairid","twinid","female", "compage1",
                       "educyrs","height1","smoke","cvd1","diabyn1",
                       "demever", "sbp1")
dynamic_variables <- setdiff(colnames(ds_wide),static_variables)
ds_long <- ds_wide %>% 
  tidyr::gather_("variable","value", dynamic_variables) %>% 
  tibble::as.tibble() %>% 
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
  ) 
ds_long %>% dplyr::glimpse()
ds_long %>% group_by(smoke) %>% count()

# ---- utility-functions ---------------------------------------------------
# local to the script
quick_save <- function(
  g,            # ggplot object to be saved
  name,         # name of the file to be saved   
  width  = 900, # width in pixels  
  height = 700, # height in pixesl  
  dpi    = 300  # resolution, dots per inch 
){
  ggplot2::ggsave(
    filename= paste0(name,".png"), 
    plot=g,
    device = png,
    path = "./reports/mmse/prints/",
    width = width,
    height = height,
    # units = "cm",
    dpi = dpi,
    limitsize = FALSE
  )
}

# ---- basic-graph --------------------------------------------------------------

plot_trajectories <- function(
  d,
  time_var,
  sample_size=100
){
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
  # dd <- d
  g <-  dd %>%  
    # ggplot2::ggplot(aes_string(x="age_at_visit",y="value",color="female")) +
    ggplot2::ggplot(aes_string(x=time_var,y="mmse")) +
    geom_smooth(method="loess", color="black",size=1, fill="grey70", alpha=.2, linetype="solid", na.rm=T, span=1.5)+
    geom_line(aes(group=id),size=.5,alpha=.06)+
    geom_point(size=2.5, alpha=.4, shape =21)+
    geom_rug(size = 1, sides = "b", alpha = .1)+
    # geom_smooth(aes(group=id), method="loess",color="red", size=3 )+
    # geom_smooth(method="lm", color="blue")+
    # facet_grid(.~source)+
    scale_y_continuous(limits = c(-.5,30.5), breaks=seq(-0,30,10))+
    scale_x_continuous(limits = c(0,9), breaks=seq(0,8,2),minor_breaks = seq(0,8,1))+
    # scale_color_manual(values = color_scale)+
    # labs(y="MMSE",x="Time until death", color = group_label)+
    main_theme+
    theme(text = element_text(size=baseSize+4)) +
    labs(x = "Years since baseline", y = "Mini Mental State Exam")
    
  # print(length(unique(dd$id)))
    # ggExtra::ggMarginal(g, type="density", xparams = list()
    ggExtra::ggMarginal(g,margins = "y", type = "histogram", binwidth = 1)
    # ggExtra::ggMarginal(g, type = "density")
}
# Usage:
ds_long %>% plot_trajectories( "years_since_bl","max") %>% 
    quick_save("observed_mmse")

ds_long %>% 



# Sonata form report structure
# ---- dev-a-0 ---------------------------------
# ---- dev-a-1 ---------------------------------
# ---- dev-a-2 ---------------------------------
# ---- dev-a-3 ---------------------------------
# ---- dev-a-4 ---------------------------------
# ---- dev-a-5 ---------------------------------

# ---- dev-b-0 ---------------------------------
# ---- dev-b-1 ---------------------------------
# ---- dev-b-2 ---------------------------------
# ---- dev-b-3 ---------------------------------
# ---- dev-b-4 ---------------------------------
# ---- dev-b-5 ---------------------------------

# ---- recap-0 ---------------------------------
# ---- recap-1 ---------------------------------
# ---- recap-2 ---------------------------------
# ---- recap-3 ---------------------------------


# ---- publish ---------------------------------------
path_report_1 <- "./reports/*/report_1.Rmd"
path_report_2 <- "./reports/*/report_2.Rmd"
allReports <- c(path_report_1,path_report_2)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      # "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}

