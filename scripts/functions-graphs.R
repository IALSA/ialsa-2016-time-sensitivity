########################
##### Plotting Coefficients   ###########
#######################

plot_coef <- function(
  x,
  label_,
  model_type_="aefb",  
  process_ ="mmse" 
){
  # x <- ds_catalog
  # label_ = "(R)-Error"
  # model_type_="aefb"
  # process_ ="mmse"
  
  if(!is.na(label_)){
    x <- x %>% 
      dplyr::filter(label == label_) %>% 
      dplyr::filter(model_type == model_type_) %>% 
      dplyr::filter(process == process_)
    
    g <-  ggplot2::ggplot(x,aes_string(y     ="model_set", 
                                       x     = "estimate",
                                       color = "sign",
                                       fill  = "sign",
                                       shape = "model_number"))  
    g <- g + geom_vline(xintercept = 0, 
                        color      = "gray50",  
                        linetype   = "dotted", 
                        size       = 1)   
    g <- g + geom_errorbarh(aes_string(xmin = "conf.low", xmax = "conf.high", color="model_number"),
                            color    = "gray25", 
                            height   = 0, 
                            linetype = "solid", 
                            size     = 0.5) 
    g <- g + geom_point(size = 3, color="black") 
    # g <- g + scale_shape_manual(values = c("TRUE"=21,"FALSE"=42))
    g <- g + scale_shape_manual(values = c("Linear"=24,"Quadratic"=22))
    g <- g + scale_color_manual(values = c("TRUE"="black","FALSE"=NA))
    g <- g + scale_fill_manual(values = c("TRUE"="black","FALSE"="white"))
    g <- g + main_theme
    g <- g + labs(shape = "", color="p < .05", fill = "p < .05")
    g <- g + theme(axis.text.y = element_text(size=baseSize))
    if(label_=="(R)-Error"){
      g + guides(fill=FALSE, color=FALSE)
    }else{
     g + guides(fill=FALSE, color=FALSE, shape=FALSE)
      
    }
    
  }else{
    g <-  ggplot2::ggplot(x,aes_string(y     ="model_set", 
                                       x     = "estimate",
                                       color = "sign",
                                       shape = "model_number"))  
    g <- g + geom_blank() + theme_void()
  } 
  
}   
# ds_catalog %>% plot_coef("(R)-Error")

# graphs a single model information index (LL, AIC, BIC)
plot_info_one <- function(
  x,
  model_type_="aefb",  
  process_ ="mmse",
  index_name = "BIC"
){
  # values for testing and development
  # x <- ds_catalog
  # model_type_="aefb"
  # process_ ="mmse"
  # index_name = "BIC"
  
  # name of the variable to story prettified index
  index_pretty <- paste0(index_name,"_pretty")
  # 
  d <- x %>% 
    dplyr::filter(model_type == model_type_) %>% 
    dplyr::filter(process == process_) %>% 
    # dplyr::select_(.dots = c())
    dplyr::distinct(
      model_name, model_number, wave_set, model_set, model_type, process, N, parameters, AIC, BIC
    ) %>%
    # tidyr::gather_(key = "index",value = "misfit_value", c("AIC","BIC")) %>% 
    dplyr::mutate(
      # index = factor(index),
      # counts = paste0("N = ",scales::comma(N),", p = ", parameters)
      n_p = paste0("  ",scales::comma(N),"-", parameters,"  ")
      
    )
  d[,index_pretty] <- sprintf("%1.0f", d[,index_name])
  d[,index_pretty] <- scales::comma(as.numeric(d[,index_pretty]))
  
  dd <- d %>% dplyr::distinct(model_set, n_p)  %>% dplyr::arrange(desc(model_set) )
  persons_parameters <- dd$n_p
  # custom_lables = levels(d %>% dplyr::select())
  # max_misfit <- max(d %>% dplyr::select(misfit_value))
  # max_misfit <- ceiling(max_misfit + .1*max_misfit)
  g <-  ggplot2::ggplot(d,aes_string(y     = "model_set", 
                                     x     = index_name
                                     # shape = "index"
                                     # color = "sign",
                                     # fill  = "sign",
                                     # shape = "model_number"
  ))  
  # g <- g + geom_point(size = 7, shape = 124)
  g <- g + scale_x_continuous(labels = scales::comma)
  # g <- g + geom_text(aes_string(label=index_pretty), size=4, vjust=.5, hjust = -.1)
  # g <- g + geom_text(aes_string(label=index_pretty), size=4, vjust=0, hjust = 0)
  g <- g + geom_text(aes_string(label=index_pretty), size=3)
    # g <- g + geom_text(aes(label = counts, x=Inf), hjust=-1)
  # g <- g + geom_text(aes(label = counts, x=Inf), hjust = .1)
  g <- g + scale_y_discrete(position = "left", labels = persons_parameters )
  # g <- g + scale_shape_manual(values = c("AIC"=65, "BIC"=66))
  # g <- g + guides(fill=FALSE, color=FALSE)
  g <- g + guides(fill=FALSE, color=FALSE, shape = FALSE)
  g <- g + labs(x = toupper(index_name), y = NULL)
  g <- g + main_theme
  # g <- g + theme(legend.position=c(.5,.5)) 
  # g <- g + theme(legend.background=element_rect(fill="white", colour="black"))
  g <- g + theme(axis.text.y = element_text(size=baseSize))
  g
}   
# ds_catalog %>% plot_info_one()



# # graph model information (LL, AIC, BIC)
# offers a number of indices of the name nanuter (misfit)
# plot_info_general <- function(
#   x,
#   model_type_="aefb",  
#   process_ ="mmse" 
# ){
#   # x <- ds_catalog
#   # model_type_="aefb"
#   # process_ ="mmse"
# 
#    d <- x %>% 
#       # dplyr::filter(label == label_) %>% 
#       dplyr::filter(model_type == model_type_) %>% 
#       dplyr::filter(process == process_) %>% 
#       dplyr::distinct(
#         model_name, model_number, wave_set,model_set, model_type, process, N, parameters, AIC, BIC
#       ) %>%
#       tidyr::gather_(key = "index",value = "misfit_value", c("AIC","BIC")) %>% 
#       dplyr::mutate(
#         index = factor(index),
#         # counts = paste0("N = ",scales::comma(N),", p = ", parameters)
#         n_p = paste0("  ",scales::comma(N),"-", parameters,"  ")
#         
#       )
#     dd <- d %>% dplyr::distinct(model_set, n_p)  %>% dplyr::arrange(desc(model_set) )
#    
#     # custom_lables = levels(d %>% dplyr::select())
#     # max_misfit <- max(d %>% dplyr::select(misfit_value))
#     # max_misfit <- ceiling(max_misfit + .1*max_misfit)
#     g <-  ggplot2::ggplot(d,aes_string(y     = "model_set", 
#                                        x     = "misfit_value",
#                                        shape = "index"
#                                        # color = "sign",
#                                        # fill  = "sign",
#                                        # shape = "model_number"
#                                        ))  
#     g <- g + geom_point(size = baseSize-9)
#     g <- g + scale_x_continuous(labels = scales::comma)
#     # g <- g + geom_text(aes(label = counts, x=Inf), hjust=-1)
#     # g <- g + geom_text(aes(label = counts, x=Inf), hjust = .1)
#     g <- g + scale_y_discrete(position = "left", labels = dd$n_p )
#     g <- g + scale_shape_manual(values = c("AIC"=65, "BIC"=66))
#     # g <- g + guides(fill=FALSE, color=FALSE)
#     g <- g + guides(fill=FALSE, color=FALSE, shape = FALSE)
#     g <- g + labs(x = "Information Criteria: (A)kaike & (B)ayesian", y = NULL)
#     g <- g + main_theme
#     # g <- g + theme(legend.position=c(.5,.5)) 
#     # g <- g + theme(legend.background=element_rect(fill="white", colour="black"))
#     g <- g + theme(axis.text.y = element_text(size=baseSize))
#     g
# }   
# # ds_catalog %>% plot_info()

# 
# levels(ds_catalog$label)

# uses plot_coef() to create a matrix of plots
matrix_coef <- function(
  x,
  term_group,
  model_type,
  process,
  info_index="BIC"
){
  # x <- ds_catalog
  # model_type="aefb"
  # process ="mmse"
  # # # term_group = "level"
  # # # term_group = "error"
  # term_group ="info_index"
  
  terms <- ls_terms[[term_group]]
  lst <- list()
  if(term_group=="info_index"){
    for(i in seq_along(terms)){
      lst[[i]] <- x %>% plot_info_one(model_type,process,index_name = ls_terms[["info_index"]])
    }
  }else{
    for(i in seq_along(terms)){
      lst[[i]] <- x %>% plot_coef(terms[i],model_type,process)
    }
  }
  pm <- GGally::ggmatrix(
    lst,
    nrow = 1, ncol = length(terms),
    # title = "MMSE",
    xAxisLabels = terms
    # yAxisLabels = "MMSE score",
    # legend = 1
  ) + theme(
    legend.position = "right",
    strip.text.x = element_text(size=baseSize+2)
    
  )
  pm
}
# matrix_coef(ds_catalog,"level","aefb","mmse")
# matrix_coef(ds_catalog,"misfit","aefb","mmse")

# used matrix_coef() to create a supermatrix of plots
super_matrix <- function(
  x,
  folder_name,
  process, 
  model_type,
  suffix = F,
  width, 
  height,
  res
){
  # x <- ds_catalog
  # model_type = "aefb"
  # process = "mmse"
  # folder_name = "./reports/model-examiner/graph2/"
  # height = 1200
  # width = 1400
  # res = 600
  # 
  # assemble the name of the file to be saved
  if(is.character(suffix)) {
    # suffix = "1"
    path_save  <- paste0(folder_name,process,"-",model_type,"-",suffix,".png")
    main_title <- paste0(suffix, ": estimated parameters modeling ",toupper(process) )               
  }else{
    path_save = paste0(folder_name,process,"-",model_type,".png")
    main_title <- paste0(toupper(process)," : model parameters")
  }
  
  
  g1 <- matrix_coef(x,"level",    model_type,process)
  g2 <- matrix_coef(x,"linear",   model_type,process)
  g3 <- matrix_coef(x,"quadratic",model_type,process)
  g4 <- matrix_coef(x,"error",    model_type,process)
  g5 <- matrix_coef(x,"info_index",   model_type,process) #+ theme(axis.text.y = element_blank())
  # g5 <- matrix_coef(x,"misfit",   model_type,process) + theme(strip.text = element_text)

  
  n_columns <- 18
  column_unit <- 100/n_columns
  # open PNG connection
  png(filename = path_save, width = width, height = height,res = res)
  vpLayout <- function(rowIndex, columnIndex) { return( viewport(layout.pos.row=rowIndex, layout.pos.col=columnIndex) ) }
  grid::grid.newpage()
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid::grid.layout(nrow=5, ncol=n_columns,
                              widths=grid::unit( rep(column_unit,n_columns) ,rep("null",n_columns)),
                              heights=grid::unit(c(.05,.24,.24,.24,.24), rep("null",5))
  )
  grid::pushViewport(grid::viewport(layout=layout))
  grid::grid.text(main_title, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2:12),hjust = "1")
  # print(g1, vp=grid::viewport(layout.pos.row=2,layout.pos.col=1:18))
  # print(g2, vp=grid::viewport(layout.pos.row=3,layout.pos.col=1:18))
  # print(g3, vp=grid::viewport(layout.pos.row=4,layout.pos.col=1:18))
  # print(g4, vp=grid::viewport(layout.pos.row=5,layout.pos.col=1:4))
  # print(g5, vp=grid::viewport(layout.pos.row=5,layout.pos.col=5:12))
  print(g1, vp=grid::viewport(layout.pos.row=3,layout.pos.col=1:18))
  print(g2, vp=grid::viewport(layout.pos.row=4,layout.pos.col=1:18))
  print(g3, vp=grid::viewport(layout.pos.row=5,layout.pos.col=1:18))
  print(g4, vp=grid::viewport(layout.pos.row=2,layout.pos.col=1:4))
  print(g5, vp=grid::viewport(layout.pos.row=2,layout.pos.col=5:12))
  
  dev.off() # close PNG device
  # return(grid::popViewport(0))
}


########################
##### Prep Data for Anatomy   ###########
#######################

# reaches into ls_catalog and extract modeled data 
get_model_data <- function(
  list_object,
  model_name
){
  # model <- ls_catalog[["u1_12345_aefb_block"]]
  # model <- ls_catalog[["u1_1234_aefb_block"]]
  model <- list_object[[model_name]]
  
  # ds <- MplusAutomation::getSavedata_Data(model[["path"]])
  
  # function to elongate extracted data
  ds_obs <- MplusAutomation::getSavedata_Data(model[["path"]])
  # (names(ds_obs) <- gh5_variables)
  head(ds_obs)
  ds_obs$id <- 1:nrow(ds_obs) # create person id
  ds_obs <- ds_obs[order(ds_obs$id), ] # sort for visual inspection
  ds_obs[ds_obs$id==1, ] # structure of data for one individual
  head(ds_obs)
  
  # define variable groups for elongation
  possible_timepoints <- c(paste0("A_0",1:5),paste0("TIME_0",1:5))
  (variables_longitudinal <- intersect(possible_timepoints,colnames(ds_obs)))
  
  # establish a wide format
  ds_long <- ds_obs %>%
    tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
    dplyr::mutate(
      varname = gsub("(\\w+)_(\\d+)","\\1", variable, perl=T),
      wave    = gsub("(\\w+)_(\\d+)","\\2", variable, perl=T) %>% as.numeric()
    ) %>% 
    dplyr::select(-variable) %>% 
    dplyr::arrange(id) %>% 
    tidyr::spread(varname,value)  
  # compute predicted scores
  if( any(colnames(ds_long)=="QA")){ 
    ds_long <- ds_long %>% 
      dplyr::mutate(
        A_HAT = IA + SA*TIME + QA*TIME
      )
  }else{
    ds_long <- ds_long %>% 
      dplyr::mutate(
        A_HAT = IA + SA*TIME 
      )
  }
  ds_long <- ds_long %>% 
    tidyr::gather_(key="source", value = "value", c("A","A_HAT")) %>%
    dplyr::mutate( 
      source = car::Recode(source,"'A'='observed';'A_HAT'='predicted'")
    ) 
  # colnames(ds_long) <- tolower(colnames(ds_long))
  return(ds_long)
}
# Usage:
# ds_long <- ls_catalog %>% get_model_data("u1_12345_aefb_block")

# takes the product of get_model_data() and covariate centers data and
# 1) recenters predictors and 2) creates factors 
prep_for_graph <- function(
  ds_long,
  centers
){
  # centers
  # d <- ds_long
  d <- ds_long %>%
    # recover centers
    dplyr::mutate(
      age_bl = AGE_C80 + centers["age"],
      edu = EDU_C7 + centers["edu"],
      sbp = SBP_C167 + centers["sbp"]
    ) %>% 
    # create age_at_visit
    dplyr::mutate(
      age_at_visit   = age_bl + TIME,
      years_since_bl = TIME
    ) %>% 
  # make factors
  dplyr::mutate(
    age_group_bl = Hmisc::cut2(age_bl,g=5 ),
    age_group_bl = factor(age_group_bl, levels = rev(levels(age_group_bl))),
    edu_group    = Hmisc::cut2(edu,g=5),
    edu_group    = factor(edu_group, levels = rev(levels(edu_group))),
    sbp_group    = Hmisc::cut2(sbp,g=5),
    sbp_group    = factor(sbp_group, levels = rev(levels(sbp_group))),
    female       = factor(FEMALE,c(0,1),c("male","female"))
  ) 
  # colnames(ds_long) <- tolower(colnames(ds_long))  
  d <- d %>%
    dplyr::select(-AGE_C80, -EDU_C7, -SBP_C167, -X_WEIGHT, -TIME, -FEMALE)
  # d <- d %>% 
  #   tidyr::gather_(key="metric",value="time", c("years_since_bl","age_at_visit")) %>% 
  #   dplyr::mutate(
  #     source_metric = paste0(source,"_",metric),
  #     source_metric = factor(
  #       source_metric,
  #       levels = c(
  #         "observed_years_since_bl",
  #         "predicted_years_since_bl",
  #         "observed_age_at_visit" ,
  #         "predicted_age_at_visit"
  #       ),
  #       labels = c(
  #         "Observed over TIME",
  #         "Observed over AGE",
  #         "Modeled over TIME",
  #         "Modeled over AGE"
  #       ))
  #   )
  return(d)
}
# Usage: 
# ds_long <- ds_long %>% prep_for_graph(centers)


########################
##### Trajectories   ###########
#######################
# takes a data set for a single model, output of prep_for_graph()
plot_trajectories <- function(
  d,
  time_var,
  sample_size=100
){
  d <- ds_long
  time_var = "years_since_bl"
  # time_var = "wave"
  sample_size = 100
  
  if(!sample_size=="max"){
    set.seed(42)
    ids <- sample(unique(d$id),sample_size)
    dd <- d %>% dplyr::filter(id %in% ids)
  }else{dd <- d}
  # dd <- d
  g <-  dd %>%  
    # ggplot2::ggplot(aes_string(x="age_at_visit",y="value",color="female")) +
    ggplot2::ggplot(aes_string(x=time_var,y="mmse")) +
    geom_line(aes(group=id),size=.9,alpha=.075)+
    geom_point(size=1.8, alpha=.15, shape =21)+
    # geom_smooth(aes(group=id), method="loess",color="red", size=3 )+
    # geom_smooth(method="lm", color="blue")+
    geom_smooth(method="loess", color="black",size=.8, fill="grey50", alpha=.3, linetype="dashed", na.rm=T, span=1.5)+
    # facet_grid(.~source)+
    scale_y_continuous(limits = c(-1,33), breaks=seq(-0,30,10))+
    # scale_y_continuous(limits = c(-10,30), breaks=seq(-10,30,10))+
    # scale_color_manual(values = color_scale)+
    # labs(y="MMSE",x="Time until death", color = group_label)+
    main_theme+
    theme(text = element_text(size=baseSize+4))
  # print(length(unique(dd$id)))
  g
}
# Usage:
# plot_trajectories(ds_long, "age_at_visit","max")
# plot_trajectories(ds_long, "age_at_visit",100)

# used output of plot_trajectories()
trajectory_matrix <- function(
  ls_catalog,
  model_name,
  sample_size_
){
  # model_name <- "u1_12345_aefb_mmse"
  # sample_size_ = 100
  # sample_size_ = "max"
  
  
  d <- ls_catalog %>% 
    get_model_data(model_name) %>% 
    prep_for_graph(centers) 
  
  model_number <- ls_catalog[[model_name]]$model_number
  wave_set <- ls_catalog[[model_name]]$wave_set
  process <- toupper(ls_catalog[[model_name]]$process)
  
  model_shape <- ifelse(model_number=="u1","Linear",
                        ifelse(model_number=="u2","Quadratic",NA))
  model_shape <- ifelse(model_number=="u1","L",
                        ifelse(model_number=="u2","Q",NA))
  model_id <- paste0(model_shape,"-",wave_set)
  
  g1 <- d %>% plot_trajectories("years_since_bl",sample_size_)
  g1 <- g1 + coord_cartesian(xlim=c(0,8))+
    scale_x_continuous(breaks=seq(0,8,2))
  
  g2 <- d %>% plot_trajectories("age_at_visit",sample_size_)
  g2 <- g2 + coord_cartesian(xlim=c(80,100),expand = TRUE)+
    scale_x_continuous(breaks=seq(80,100,5))
  
  
  pm <- GGally::ggmatrix(
    list(
      "time" = g1,
      "age" =  g2
    ),
    nrow = 1, ncol = 2,
    # title = paste0(process," trajectories as a function of time. Shape: ",model_shape,", Waveset: ", wave_set), 
    title = paste0(process," trajectories as a function of time. Model: ",model_shape,"-", wave_set), 
    xAxisLabels = c("Years since baseline","Age at visit"), 
    yAxisLabels = "MMSE score"#,
    # legend = 2
  ) + theme(
    plot.title = element_text(size=baseSize+7),
    strip.text = element_text(size=baseSize+5)
    # strip.text.y = element_text(size=baseSize+7)
  )
  pm
}
# Usage:
# ls_catalog %>% trajectory_matrix("u1_12345_aefb_mmse", 100)





trajectory_supermatrix <- function(
  ls_catalog
  # folder_name,
  # width, 
  # height,
  # res
){
  sample_size = 100
  # x <- ds_long
  # model_type = "aefb"
  # process = "mmse"
  folder_name = "./reports/model-grapher/graphs-1/"
  height = 1200
  width = 1400
  res = 600
  
  g1 <- ls_catalog %>% 
    get_model_data("u1_12345_aefb_mmse") %>% 
    prep_for_graph(centers) %>%  
    trajectory_matrix(sample_size)
  
  g2 <- ls_catalog %>% 
    get_model_data("u2_12345_aefb_mmse") %>% 
    prep_for_graph(centers) %>%  
    trajectory_matrix(sample_size)
  
  g3 <-ls_catalog %>% 
    get_model_data("u1_1234_aefb_mmse") %>% 
    prep_for_graph(centers) %>%  
    trajectory_matrix(sample_size)
  
  g4 <-ls_catalog %>% 
    get_model_data("u2_1234_aefb_mmse") %>% 
    prep_for_graph(centers) %>%  
    trajectory_matrix(sample_size)
  
  g5 <-ls_catalog %>% 
    get_model_data("u1_123_aefb_mmse") %>% 
    prep_for_graph(centers) %>%  
    trajectory_matrix(sample_size)
  
  g6 <-ls_catalog %>% 
    get_model_data("u2_123_aefb_mmse") %>% 
    prep_for_graph(centers) %>%  
    trajectory_matrix(sample_size)
  
  g7 <-ls_catalog %>% 
    get_model_data("u1_135_aefb_mmse") %>% 
    prep_for_graph(centers) %>%  
    trajectory_matrix(sample_size)
  
  g8 <-ls_catalog %>% 
    get_model_data("u2_135_aefb_mmse") %>% 
    prep_for_graph(centers) %>%  
    trajectory_matrix(sample_size)
  
  path_save = paste0(folder_name,"mmse",".png")
  png(filename = path_save, width = width, height = height,res = res)
  
  vpLayout <- function(rowIndex, columnIndex) { return( viewport(layout.pos.row=rowIndex, layout.pos.col=columnIndex) ) }
  grid::grid.newpage()
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid::grid.layout(nrow=8, ncol=1,
                              widths=grid::unit(c(1) ,rep("null",1)),
                              heights=grid::unit(rep(.125,8), rep("null",8))
  )
  grid::pushViewport(grid::viewport(layout=layout))
  # main_title <- toupper(dsL$study_name[1])
  # main_title <- paste0(toupper(mp$study_name), " \n ", mp$subgroup, " \n ",
  #                      "N = ", sample_N)
  # main_title <- paste0(toupper(process)," : model parameters")
  # grid::grid.text(main_title, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1:3))
  print(g1, vp=grid::viewport(layout.pos.row=1,layout.pos.col=1))
  print(g2, vp=grid::viewport(layout.pos.row=2,layout.pos.col=1))
  print(g3, vp=grid::viewport(layout.pos.row=3,layout.pos.col=1))
  print(g4, vp=grid::viewport(layout.pos.row=4,layout.pos.col=1))
  print(g5, vp=grid::viewport(layout.pos.row=5,layout.pos.col=1))
  print(g6, vp=grid::viewport(layout.pos.row=6,layout.pos.col=1))
  print(g7, vp=grid::viewport(layout.pos.row=7,layout.pos.col=1))
  print(g8, vp=grid::viewport(layout.pos.row=8,layout.pos.col=1))
  
  
  dev.off()
  # return(grid::popViewport(0))
}

# ls_catalog %>% trajectory_supermatrix()


