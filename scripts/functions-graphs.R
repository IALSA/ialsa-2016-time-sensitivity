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

# 
# levels(ds_catalog$label)

matrix_coef <- function(x,term_group,model_type, process){
  # x <- ds_catalog
  # model_type="aefb"
  # process ="mmse"
  # term_group = "level"
  # term_group = "error"
  
  terms <- ls_terms[[term_group]]
  lst <- list()
  for(i in seq_along(terms)){
    lst[[i]] <- x %>% plot_coef(terms[i],model_type,process)
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

super_matrix <- function(x,model_type,process, folder_name,
                         width, height,res){
  # x <- ds_catalog
  # model_type = "aefb"
  # process = "mmse"
  # folder_name = "./reports/model-examiner/graph2/"
  # height = 1200
  # width = 1400
  # res = 600
  
  g1 <- matrix_coef(x,"level",    model_type,process)
  g2 <- matrix_coef(x,"linear",   model_type,process)
  g3 <- matrix_coef(x,"quadratic",model_type,process)
  g4 <- matrix_coef(x,"error",    model_type,process)
  
  path_save = paste0(folder_name,process,"-",model_type,".png")
  png(filename = path_save, width = width, height = height,res = res)
  
  vpLayout <- function(rowIndex, columnIndex) { return( viewport(layout.pos.row=rowIndex, layout.pos.col=columnIndex) ) }
  grid::grid.newpage()
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid::grid.layout(nrow=5, ncol=5,
                              widths=grid::unit(c(.2,.2,.2,.2,.2) ,c("null","null","null","null","null")),
                              heights=grid::unit(c(.05,.24,.24,.24,.24), c("null","null","null","null"))
  )
  grid::pushViewport(grid::viewport(layout=layout))
  # main_title <- toupper(dsL$study_name[1])
  # main_title <- paste0(toupper(mp$study_name), " \n ", mp$subgroup, " \n ",
  #                      "N = ", sample_N)
  main_title <- paste0(toupper(process)," : model parameters")
  grid::grid.text(main_title, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1:3))
  print(g1, vp=grid::viewport(layout.pos.row=2,layout.pos.col=1:5))
  print(g2, vp=grid::viewport(layout.pos.row=3,layout.pos.col=1:5))
  print(g3, vp=grid::viewport(layout.pos.row=4,layout.pos.col=1:5))
  print(g4, vp=grid::viewport(layout.pos.row=5,layout.pos.col=1))
  dev.off()
  # return(grid::popViewport(0))
}


##### old functions from terminal declide project ----------

plot_trajectories <- function(d,sample_size=200){
  set.seed(42)
  ids <- sample(unique(d$id),sample_size)
  g <-  d %>% 
    ggplot2::ggplot(aes(x=DTIMEC,y=Y,color=class)) +
    geom_line(aes(group=id), size=.8, alpha=.7)+
    labs(y="MMSE",x="Time until death",color="Latent class")+
    scale_colour_manual(values=colors_classes)+
    main_theme#+
  # theme(text = element_text(size=baseSize+6))
}

class_trajectories <- function(d,group,group_label,color_scale,sample_size=200){
  # d <- ds_long_newc
  # group <- "female" 
  set.seed(42)
  ids <- sample(unique(d$id),sample_size)
  g <-  d %>% dplyr::filter(id %in% ids) %>% 
    ggplot2::ggplot(aes_string(x="DTIMEC",y="Y",color=group)) +
    geom_line(aes(group=id),size=.8,alpha=.7)+
    facet_grid(.~class)+
    scale_color_manual(values = color_scale)+
    labs(y="MMSE",x="Time until death", color = group_label)+
    main_theme+
    theme(text = element_text(size=baseSize+4))
  g
}

class_traj_matrix <- function(newc,octo,group,group_label,color_scale){
  pm <- GGally::ggmatrix(
    list(
      "newc" = class_trajectories(newc,group,group_label,color_scale),
      "octo" = class_trajectories(octo,group,group_label,color_scale)
    ),
    nrow = 1, ncol = 2,
    title = "MMSE trajectories plotted as a function of years to death\n(intercept placed at two years before death)", 
    xAxisLabels = c("Newcastle 85+","OCTO-Twin"), 
    yAxisLabels = "MMSE score",
    legend = 2
  ) + theme(
    legend.position = "bottom"
    
  )
  pm
}