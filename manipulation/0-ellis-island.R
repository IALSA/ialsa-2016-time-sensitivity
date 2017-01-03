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
stencil <- readr::read_csv("./data/shared/raw/table-stencil-octo-2.csv")


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




ds_catalog %>% spread_by_wave_set("aefb","mmse",c("L-12345","L-135"))

########## developmental script below ###################


dummy <- list(
  "study_label" = NA,
  "model"       = NA,
  "stencil"     = NA,
  "estimates"   = list("1"=NA,"2"=NA),
  "class_size" = list("1"=NA,"2"=NA),
  "class_labels" = NA
)

lst <- list(
  "newcastle" = dummy,
  "octo"      = dummy
)
lapply(lst,names)

lst$newcastle$study_label = "Newcastle 85+"
lst$newcastle$model <- MplusAutomation::readModels(path_newc_out)
lst$newcastle$stencil <- readr::read_csv(path_stencil_newcastle)
lst$newcastle$class_labels <- c("2" = "Slow decliners", "1" = "Fast decliners")

lst$octo$study_label <- "OCTO-Twin"
lst$octo$model <- MplusAutomation::readModels(path_octo_out)
lst$octo$stencil <-  readr::read_csv(path_stencil_octo)
lst$octo$class_labels <- c("1" = "Slow decliners", "2" = "Fast decliners")

# ---- populate-list-object --------------------------

for(study in c("newcastle","octo")){
  for(class in c(1,2)){
    lst[[study]][["estimates"]][[paste0(class)]] <- get_estimate_table(lst,study,class)
    lst[[study]][["class_size"]][[paste0(class)]] <- get_class_size(lst,study,class)
  }#close class
}#close study
lapply(lst,names)


# ---- assemble-table -----------------------------
one_column <- function(lst,study,class){
  # study = "octo"
  # class = 1
  # 
  class_ = as.character(class)
  l <- list(
    lst[[study]]$study_label,
    lst[[study]]$class_labels[class_] %>% as.character(),
    class_size_pretty(lst,study,class),
    get_estimate_table(lst,study,class) %>% dplyr::select(effect,label,dense)
  )
  n <- do.call(rbind,l)
  n[1:3,1:2] <- ""
  return(n)
}
one_column(lst,"octo",1)

l <- list(
  one_column(lst, "newcastle",1) %>% dplyr::select(effect,label),
  one_column(lst,"newcastle",1) %>% dplyr::select(dense),
  one_column(lst,"newcastle",2)%>% dplyr::select(dense),
  one_column(lst,"octo",2)%>% dplyr::select(dense),
  one_column(lst,"octo",1)%>% dplyr::select(dense)
)
t1 <- do.call(cbind,l) %>% as.data.frame()

# ---- prepare-data-newcastle --------------------------------------------------------------
# prepare data for Newscastle
ds_wide <- lst$newcastle$model$savedata 
head(ds_wide)
ds_wide$id <- 1:nrow(ds_wide) # create person id
ds_wide <- ds_wide %>% dplyr::arrange(id) # sort for visual inspection
head(ds_wide); nrow(ds_wide)
(variables_longitudinal <- c(paste0("Y",1:3),paste0("DTIMEC",1:3)))
(variables_static <- setdiff(colnames(ds_wide), variables_longitudinal))
ds_long_newc <- ds_wide %>% 
  tidyr::gather_(key="g",value="value",variables_longitudinal) %>% 
  dplyr::mutate(
    varname = gsub("(\\w+)(\\d+)$", "\\1", g, perl=T),
    wave    = gsub("(\\w+)(\\d+)$", "\\2", g, perl=T)
  ) %>% 
  dplyr::select(-g) %>% 
  dplyr::arrange(id) %>% 
  tidyr::spread(key=varname, value=value) %>% 
  dplyr::mutate( 
    class = factor(C,levels=c(1,2),labels=c("Fast decliners","Slow decliners")),
    edu           = EDUC + 9.91,
    educat        = car::Recode(edu,"
                              0:9  = '-1'; 
                                10:11 =  '0';
                                12:30 =  '1';
                                "),
    educat        = factor(educat,
                           levels = c(-1,             0,             1), 
                           labels = c("0-9 years", "10-11 years", ">11 years")),
    
    female        = factor(FEMALE,levels=c(0,1),labels=c("male","female")),
    ses_group     = HIGHSOCC * 2, # to match  octo levels
    ses_group     = factor(ses_group, levels=c(0,2),labels=c("low","high")),
    dementia_ever = factor(DEM, levels = c(0,1),labels=c("yes","no"))
  )
# ---- prepare-data-octo ------------------------------
# prepare data for OCTO-Twin
ds_wide <- lst$octo$model$savedata 
head(ds_wide)
ds_wide$id <- 1:nrow(ds_wide) # create person id
ds_wide <- ds_wide %>% dplyr::arrange(id) # sort for visual inspection
head(ds_wide); nrow(ds_wide)
(variables_longitudinal <- c(paste0("Y",1:5),paste0("DTIMEC",1:5)))
(variables_static <- setdiff(colnames(ds_wide), variables_longitudinal))
ds_long_octo <- ds_wide %>% 
  tidyr::gather_(key="g",value="value",variables_longitudinal) %>% 
  dplyr::mutate(
    varname = gsub("(\\w+)(\\d+)$", "\\1", g, perl=T),
    wave    = gsub("(\\w+)(\\d+)$", "\\2", g, perl=T)
  ) %>% 
  dplyr::select(-g) %>% 
  dplyr::arrange(id) %>% 
  tidyr::spread(key=varname, value=value) %>% 
  dplyr::mutate( 
    class = factor(C,levels=c(2,1),labels=c("Fast decliners","Slow decliners")),
    edu           = EDUC + 7,
    educat        = car::Recode(edu,"
                                 0:9  = '-1'; 
                                10:11 =  '0';
                                12:30 =  '1';
                                "),
    educat        = factor(educat,
                           levels = c(-1,             0,             1), 
                           labels = c("0-9 years", "10-11 years", ">11 years")),
    
    female        = factor(FEMALE,levels=c(0,1),labels=c("female","male")),
    ses_group     = SESGRP - 1, # to match levels
    ses_group     = factor(ses_group, levels=c(0,1,2),labels=c("low","medium","high")),
    dementia_ever = factor(DEMEVER, levels = c(0,1),labels=c("yes","no"))
  )

# ---- data-average-trajectories ----------------
# propare data to plot average estimated trajectories
dummy <- c("level"=NA,"slope"=NA,"quad"=NA)
lest <- list(
  "newcastle" = list("1"=dummy,"2"=dummy),
  "octo" = list("1"=dummy,"2"=dummy)
)
# if you need to assing manual values:
lest[["newcastle"]][["1"]]["level"] <- 18.16
lest[["newcastle"]][["1"]]["slope"] <- -4.67
lest[["newcastle"]][["1"]]["quad"]  <- NA
lest[["newcastle"]][["2"]]["level"] <- 27.33
lest[["newcastle"]][["2"]]["slope"] <-  -.34
lest[["newcastle"]][["2"]]["quad"]  <- NA
lest[["octo"]][["1"]]["level"]      <- 21.10
lest[["octo"]][["1"]]["slope"]      <- -2.4
lest[["octo"]][["1"]]["quad"]       <-  -.14 
lest[["octo"]][["2"]]["level"]      <- 26.93
lest[["octo"]][["2"]]["slope"]      <-  -.62
lest[["octo"]][["2"]]["quad"]       <-  -.08
lest_manual <- lest
# extract 

est <- list(
  "newcastle" =  lst$newcastle$model$parameters$unstandardized,
  "octo"      = lst$octo$model$parameters$unstandardized
)
get_one_intercept <- function(
  ls,
  study,
  parameter,
  class
){
  value <- ls[[study]] %>% 
    dplyr::filter(
      paramHeader=="Intercepts",
      param==parameter,
      LatentClass==class
    ) %>% 
    dplyr::select(est) %>% as.numeric()
  return(value)
}
# get_one_intercept(est,"newcastle","I",1)
lest[["newcastle"]][["1"]]["level"] <- get_one_intercept(est,"newcastle","I",1)
lest[["newcastle"]][["1"]]["slope"] <- get_one_intercept(est,"newcastle","S",1)
lest[["newcastle"]][["1"]]["quad"]  <- NA
lest[["newcastle"]][["2"]]["level"] <- get_one_intercept(est,"newcastle","I",2)
lest[["newcastle"]][["2"]]["slope"] <- get_one_intercept(est,"newcastle","S",2)
lest[["newcastle"]][["2"]]["quad"]  <- NA
lest[["octo"]][["1"]]["level"]      <- get_one_intercept(est,"octo","I",1)
lest[["octo"]][["1"]]["slope"]      <- get_one_intercept(est,"octo","S",1)
lest[["octo"]][["1"]]["quad"]       <- get_one_intercept(est,"octo","Q",1)
lest[["octo"]][["2"]]["level"]      <- get_one_intercept(est,"octo","I",2)
lest[["octo"]][["2"]]["slope"]      <- get_one_intercept(est,"octo","S",2)
lest[["octo"]][["2"]]["quad"]       <- get_one_intercept(est,"octo","Q",2)
make_ds_fixed <- function(
  lest
){
  make_one_ds <- function(lest,study,class){ 
    d <- data.frame( 
      time = seq(-4,0,1),
      class = class,
      study = study,
      level = lest[[study]][[class]]["level"],
      slope = lest[[study]][[class]]["slope"],
      quad =  lest[[study]][[class]]["quad"]
    ) 
    return(d) 
  }
  d1 <- make_one_ds(lest,"newcastle",1)# 18.16, -4.67
  d2 <- make_one_ds(lest,"newcastle",2)# 27.33, -.34
  d3 <- make_one_ds(lest,"octo",1) # 21.10 , -2.4, -.14
  d4 <- make_one_ds(lest,"octo",2) # 26.93, -.62 ,-.08
  dto <- list(d1,d2,d3,d4)
  dd <- plyr::ldply(dto,data.frame) %>% 
    dplyr::mutate( 
      level = as.double(level),
      slope = as.double(slope),
      quad = as.double(quad),
      mmse = ifelse(study=="newcastle",level + slope*time,ifelse(
        study=="octo",level + slope*time + quad*time, NA)),
      class = factor(class,levels=c(1,2),labels=c("Fast decliners","Slow decliners"))
    ) 
  return(dd)
}
ds_lest_manual <- make_ds_fixed(lest_manual)
ds_lest  <- make_ds_fixed(lest)

# ---- define-graphical-settings ------------------------
colors_classes <- c(
  "Fast decliners" = "#d95f02",
  "Slow decliners" = "#7570b3"
)
#1b9e77
#d95f02
#7570b3
colors_sex <- c(
  "female" = "#d95f02",
  "male" = "#7570b3"
)

colors_dem <- c(
  "yes" = "#d95f02",
  "no" = "#7570b3"
)

colors_ses <- c(
  "low"    ="#1b9e77",
  "medium" ="#d95f02", 
  "high"   ="#7570b3" 
)

colors_edu <- c(
  "0-9 years"   ="#1b9e77",
  "10-11 years" ="#d95f02", 
  ">11 years"   ="#7570b3" 
)

#d73027
# ----- define-graphing-functions ---------------------------
quick_save <- function(g,name,width=550,height=400,dpi=100){
  ggplot2::ggsave(
    filename= paste0(name,".png"), 
    plot=g,
    device = png,
    path = "./reports/combined-1/graphs/",
    width = width,
    height = height,
    # units = "cm",
    dpi = dpi,
    limitsize = FALSE
  )
}


# ----- print-graphs ------------------

pm <- GGally::ggmatrix(
  list(
    "newc" = plot_trajectories(ds_long_newc),
    "octo" = plot_trajectories(ds_long_octo)
  ),
  nrow = 1, ncol = 2,
  title = "Figure 1: MMSE trajectories plotted as a function of years to death\n(intercept placed at two years before death)", 
  xAxisLabels = c("Newcastle 85+","OCTO-Twin"), 
  yAxisLabels = "Mini Mental State Exam",
  legend = 1#,
  # gg = element_text(size=20)
) + theme(
  legend.position = "left",
  text = element_text(size=baseSize+6)
)  
pm %>% quick_save("fig-1-trajectories",900,450,400)


class_traj_matrix(ds_long_newc, ds_long_octo,"female","Gender: ",colors_sex) %>% 
  quick_save("fig-1-trajectories-gender",1200,450,600)
class_traj_matrix(ds_long_newc, ds_long_octo,"ses_group","SES group: ",colors_ses)%>% 
  quick_save("fig-1-trajectories-ses",1200,450,600)
class_traj_matrix(ds_long_newc, ds_long_octo,"educat","Education: ", colors_edu)%>% 
  quick_save("fig-1-trajectories-education",1200,450,600)
class_traj_matrix(ds_long_newc, ds_long_octo,"dementia_ever","Dementia", colors_dem)%>% 
  quick_save("fig-1-trajectories-dementia",1200,450,600)



# g <- ds_lest_old %>% 
g <- ds_lest %>%
  # ggplot2::ggplot(aes(x=time,y=mmse, color=class,linetype=study))+
  ggplot2::ggplot(aes(x=time,y=mmse, color=study,color=study,linetype=class))+
  geom_line(size=2)+
  labs(color="Study",linetype="Latent class")+
  # scale_color_manual(values=colors_classes)+
  scale_linetype_manual(values=c("solid", "dotted"))+ # Change linetypes
  main_theme +
  labs(title = "Figure 2: Average estimated MMSE linear trajectories", x="Years to death",y="MMSE")+
  theme(legend.position = "right", text = element_text(size=baseSize+5))

g %>% quick_save("fig-2-average-trajectories",width=600,height=400,dpi = 400)


# @knitr reproduce ---------------------------------------
#   rmarkdown::render(input = "./reports/report.Rmd" ,
#                     output_format="html_document", clean=TRUE)