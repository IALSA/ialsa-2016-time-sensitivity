# knitr::stitch_rmd(script="./___/___.R", output="./___/stitched-output/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# load functions that generate scripts
source("./scripts/mplus/functions-to-generate-Mplus-scripts.R")
source("./scripts/functions-tables.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
# requireNamespace("car") # For it's `recode()` function.
requireNamespace("reshape2") # data transformations
requireNamespace("data.table") # data transformations
requireNamespace("MplusAutomation")
requireNamespace("stringr")
requireNamespace("IalsaSynthesis")

# ---- declare-globals ---------------------------------------------------------
options(width=160)
path_generic_data  <- "./data/unshared/derived/octo/wide-dataset.dat"
path_generic_names <- "./data/unshared/derived/octo/wide-variable-names.txt"

varnames_physical <- c(
  "pef",             # forced expiratory volume
  "gait",      # Gait Speed - MAP
  "grip"          # Extremity strength
)
varnames_cognitive <- c(
   "block"
  ,"digitspanbackward"
  ,"digitspanforward"
  ,"symbol"
  ,"prose"
  ,"info"
  ,"synonyms"
  # ,"psif" # has only 4 waves, exclude for now
  ,"figure"
  ,"mirrecall"
  ,"mmse"
  # ,"mirnaming" # exibited poor performance before
  ,"mirrecog"
  ,"clock"
  )
# note: psfi has only 4 waves

varnames_tested <- c(varnames_physical, varnames_cognitive)

# ---- load-data ---------------------------------------------------------------
# ds_long <- readRDS("./data/unshared/derived/octo/data-long.rds")
ds_wide <- readRDS("./data/unshared/derived/octo/data-wide.rds")


testit::assert("File does not exist",file.exists(path_generic_data))
testit::assert("File does not exist",file.exists(path_generic_names))

# file.copy(from=path_generic_names,to= "./sandbox/pipeline-demo-1/outputs/",overwrite = T)

# ---- inspect-data -------------------------------------------------------------
ds_wide %>% dplyr::glimpse(80)
# ---- tweak-data --------------------------------------------------------------
ds_wide <- ds_wide %>% tibble::as_tibble()


ds <- ds_wide %>% 
  dplyr::select(Case, 
                # dplyr::starts_with("years_since_bl_"), 
                dplyr::starts_with("mmse_") ) %>% 
  dplyr::rename(id = Case) 

static_variables <-  c("id")
dynamic_variables <- setdiff(colnames(ds),static_variables)
  
ds_long  <- ds %>% 
  tidyr::gather_("variable","value", dynamic_variables) %>% 
  dplyr::mutate(
    wave     = gsub("^(\\w+?)_(\\d+)", "\\2", variable) %>% as.integer(),
    variable = gsub("^(\\w+?)_(\\d+)", "\\1", variable)
  )%>% 
  tidyr::spread(variable, value) 

ds_long

ds_long %>% dplyr::glimpse()

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
  tidyr::spread(wave, mmse) 
d1[is.na(d1)] <- "."
d1
d1 <- d1 %>% 
  dplyr::mutate(
    pattern = paste(wave_1, wave_2, wave_3, wave_4, wave_5, sep = "-")
  )  
# add mmse response pattern to the original data
ds_wide <- ds_wide %>% 
  dplyr::left_join(d1, by = c("Case" = "id") )

# summary table of the conditions  
d2 <- ds_wide %>% 
  dplyr::group_by(wave_1, wave_2, wave_3, wave_4, wave_5, pattern) %>% 
  dplyr::summarize(n = n()) %>% 
  dplyr::arrange(desc(wave_1), desc(wave_2), desc(wave_3), desc(wave_4), desc(wave_5)) %>% 
  dplyr::ungroup()
d2 %>% neat(output_format = "pandoc")

d2 %>% dplyr::summarize(N = sum(n))

# create a tag variable 
ds_wide <- ds_wide %>% 
  dplyr::filter(pattern %in% c("1-2-3-4-5","1-2-3-4-.", "1-2-3-.-.")) %>% 
  dplyr::select(-wave_1,-wave_2,-wave_3,-wave_4,-wave_5 ) 

ds_wide %>% 
  dplyr::group_by(pattern) %>% 
  dplyr::summarize(n=n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(N = sum(n))
# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------



## Run the lines above to load the needed functions
## Execute script snippets for each pair individually below this
# ---- create-predictor-selector -----------------------------
ls_model_number <- list(
   "univariate_linear"    = "u1"
  ,"univariate_quadratic" = "u2"
)
ls_subgroup = list(
   "12345" = "12345"
   # missing one wave
   ,"1234" = "1234"
   ,"1235" = "1235"
   ,"1245" = "1245"
   ,"1345" = "1345"
   # missing two waves
   ,"123" = "123"
   ,"125" = "125"
   ,"145" = "145"
   ,"134" = "134"
   ,"135" = "135"
)
ls_model_type <- list(
   "a"       = c("age_c80")
  ,"ae"      = c("age_c80","edu_c7")
  ,"aef"     = c("age_c80","edu_c7","female")
  ,"aefb"    = c("age_c80","edu_c7","female","sbp_c167")
)



############################################################ GRIP #####
## @knitr dummy_1
# Use the first example as the template for further pairs

# wave_set_modeled <-  c(1,2,3,4,5)
subset_condition_1 = "dementia_ever NE 1" 
folder_data        = "./data/unshared/derived/octo"
path_prototype     = "./estimation/octo/prototype-u-octo.inp"
folder_output      = "./output/studies/octo"



# single model
# mplus_generator_univariate(
#    model_number       = "u2"
#   ,subgroup           = "123"
#   ,model_type         = "aefb"
#   ,process_a          = 'block'# item name of process (A), goes into file name
#   ,subset_condition_1 = "dementia_ever NE 1" # subset data to member of this group
#   ,path_prototype     = path_prototype
#   ,folder_data        = folder_data
#   ,folder_output      = folder_output
#   ,run_models         = TRUE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files
# )

# loop over conditions
# for(measure in "mmse"){
for(measure in varnames_tested){
  # for(model_number in "u1"){
  for(model_number in "u2"){
    for(subgroup in names(ls_subgroup)){
      for(model_type in names(ls_model_type)){
        mplus_generator_univariate(
          model_number        = model_number
          ,subgroup           = subgroup
          ,model_type         = model_type
          ,process_a          = measure# item name of process (A), goes into file name
          ,subset_condition_1 = subset_condition_1 # subset data to member of this group
          ,path_prototype     = path_prototype
          ,folder_data        = folder_data
          ,folder_output      = folder_output
          ,run_models         = TRUE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files
        )
      }
    }
  }
}




# ---- examine-created-output ----------------
source("./scripts/mplus/mplus.R") # downloaded from http://www.statmodel.com/mplus-R/mplus.R
path_gh5 <- "./sandbox/syntax-creator/outputs/grip-mmse/male_5.gh5"

# view options: https://www.statmodel.com/mplus-R/GH5_R.shtml

mplus.list.variables(path_gh5) # variables in the gh5 file
mplus.view.plots(path_gh5)  # available graphs for this type of gh5 file
# histograms
mplus.plot.histogram(path_gh5, "SA") # slope of process A
mplus.plot.histogram(path_gh5, "SB") # slope of process B
# scatterplots
mplus.plot.scatterplot(path_gh5, "IA", "IB") # intercepts
mplus.plot.scatterplot(path_gh5, "SA", "SB") # slopes
mplus.plot.scatterplot(path_gh5, "IA", "SA") # physical
mplus.plot.scatterplot(path_gh5, "IB", "SB") # cognitive

ds <- mplus.get.data(path_gh5, "SA")

summary(ds)
head(ds)

#### ----- development ----------------------

# Grip - Boston Naming Task #
# # from "./sandbox/syntax-creator/extraction_functions.R  script
# collect_model_results(folder = "outputs/pairs/grip_bnt") # collect and save into the same folder
# ds <- readRDS(file.path(pathFolder,"grip_bnt.rds")) # load the data for outcome pair
# # from "./scripts/graphs/koval_brown_profiles.R"
# kb_profiles(ds,  vertical="wave_count",  border=5) # produces the kb_profile graph
#




