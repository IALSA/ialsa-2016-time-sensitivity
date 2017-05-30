
numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) }

# returns a data frame with formated values
get_estimate_table <- function(
  lst
  ){
  # lst <- model_result
  d1 <- lst[["parameters"]][["unstandardized"]]
  d2 <- stencil %>% 
    dplyr::left_join(d1,by=c("paramHeader","param")) %>% 
    dplyr::mutate(
      est_pretty  = numformat( est),
      se_pretty   = numformat( se),
      pval_pretty = ifelse(pval<.001,"<.001",numformat(pval)),
      dense = sprintf("%4s(%4s), %5s",est_pretty, se_pretty, pval_pretty),
      dense = ifelse(is.na(est),"",dense)
    )
  return(d2)
}

# prints a table with columns corresponding to wave sets 
spread_by_wave_set <- function(
  catalog,
  model_type_,
  process_,
  custom_columns=NULL,
  stencil_=stencil
){
  # values for testing and development
  # catalog = ds_catalog
  # stencil_ = stencil
  # model_type_    = "aefb"
  # process_      = "mmse"
  # define the order of the rows in the table
  custom_sort <- stencil_ %>% 
    dplyr::mutate(
      effect_label = paste0(effect,label)
    ) %>% 
    dplyr::select(effect_label) %>% 
    as.data.frame()
  custom_sort <- custom_sort[,"effect_label"]
  custom_sort <- c(custom_sort,"N","parameters","AIC","BIC")
  # subset a working ds for the table
  d1 <- ds_catalog %>% 
    dplyr::filter(
      # model_number == model_number_,
      # wave_set     == col,
      model_type   == model_type_,
      process      == process_
    ) %>% 
    dplyr::mutate(
      number_letter = ifelse(model_number=="u1","L","Q"), 
      number_set = paste0(number_letter,"-",wave_set)
    ) %>% dplyr::select(-number_letter)
  # define what columns the table will have
  columns <- unique(d1[,"number_set"]) %>% 
    as.character() 
  
  # columns <- c("12345","1245")
  ls_cata <- sapply(columns,function(x) NULL)
  for(col in columns){
    # col <- "Q-1235"
    d2 <- d1 %>% 
      dplyr::filter(number_set == col) 
    # gather model information indices
    info <- data.frame(
      "effect" = NA,
      "label" = c("N","parameters", "AIC","BIC"),
      "dense" = c(
        scales::comma(d2[1,"N"]),
        scales::comma(d2[1,"parameters"]),
        scales::comma(round(d2[1,"AIC"]),0),
        scales::comma(round(d2[1,"BIC"]),0)
      )
    )
    # assemble a single table
    ls_cata[[col]] <- d2 %>% 
      dplyr::select(effect,label,dense) %>% 
      dplyr::bind_rows(info) %>% 
      dplyr::mutate(
        effect = ifelse(is.na(effect),"",effect)
      )
  } # close loop
  # combine list into the table
  ds_cata <- ls_cata %>% 
    plyr::ldply(data.frame, .id = "wave_set") %>% 
    tidyr::spread(wave_set, dense) %>% 
    dplyr::mutate(effect_label = paste0(effect, label)) %>% 
    dplyr::slice(match(custom_sort,effect_label)) %>% 
    dplyr::select(-effect_label)
  # forma the table
  ds_cata["label"] <- format(ds_cata["label"],justify = "left")
  
  if(!is.null(custom_columns)){
    ds_cata <- ds_cata[, c( c("effect","label"),custom_columns )]
  }
  
  return(ds_cata)
}
# ds_catalog %>% spread_by_wave_set("aefb","mmse",c("L-12345","L-135"))


# prints a table with columns corresponding to wave sets for a specific model_number
spread_by_wave_set_v2 <- function(
  catalog,
  model_number_, 
  model_type_,
  process_,
  stencil_=stencil
){
  # values for testing and development
  model_number_ = "u2"
  model_type_    = "aefb"
  process_      = "mmse"
  # define the order of the rows in the table
  custom_sort <- stencil_ %>% 
    dplyr::mutate(
      effect_label = paste0(effect,label)
    ) %>% 
    dplyr::select(effect_label) 
  custom_sort <- custom_sort[,"effect_label"]
  custom_sort <- c(custom_sort,"N","parameters","AIC","BIC")
  # subset a working ds for the table
  d1 <- ds_catalog %>% 
    dplyr::filter(
      model_number == model_number_,
      # wave_set     == col,
      model_type   == model_type_,
      process      == process_
    )
  # define what columns the table will have
  columns <- unique(d1[,"wave_set"]) %>% 
    as.character() %>% 
    as.numeric %>% 
    sort(decreasing = T) %>% 
    as.character()
  
  # columns <- c("12345","1245")
  ls_cata <- sapply(columns,function(x) NULL)
  for(col in columns){
    # col <- "1234"
    d2 <- d1 %>% 
      dplyr::filter(wave_set == col) 
    # gather model information indices
    info <- data.frame(
      "effect" = NA,
      "label" = c("N","parameters", "AIC","BIC"),
      "dense" = c(
        scales::comma(d2[1,"N"]),
        scales::comma(d2[1,"parameters"]),
        scales::comma(round(d2[1,"AIC"]),0),
        scales::comma(round(d2[1,"BIC"]),0)
      )
    )
    # assemble a single table
    ls_cata[[col]] <- d2 %>% 
      dplyr::select(effect,label,dense) %>% 
      dplyr::bind_rows(info) %>% 
      dplyr::mutate(
        effect = ifelse(is.na(effect),"",effect)
      )
  } # close loop
  # combine list into the table
  ds_cata <- ls_cata %>% 
    plyr::ldply(data.frame, .id = "wave_set") %>% 
    tidyr::spread(wave_set, dense) %>% 
    dplyr::mutate(effect_label = paste0(effect, label)) %>% 
    dplyr::slice(match(custom_sort,effect_label)) %>% 
    dplyr::select(-effect_label)
  # forma the table
  ds_cata["label"] <- format(ds_cata["label"],justify = "left")
  return(ds_cata)
}
# ds_catalog %>% spread_by_wave_set_v2("u1","aefb","mmse")

