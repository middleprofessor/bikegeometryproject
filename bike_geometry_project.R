## ----setup, message = FALSE, warning = FALSE-------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      knitr.kable.NA = '')
# wrangling packages
library(here) # here makes a project transportable
library(janitor) # clean_names
library(readxl) # read excel, duh!
library(openxlsx) # write excel
library(data.table) # magical data frames
library(magrittr) # pipes
library(stringr) # string functions
library(forcats) # factor functions
library(arules) # discretize function

# analysis packages
library(emmeans) # the workhorse for inference
library(nlme) # gls and some lmm
library(lme4) # linear mixed models
library(lmerTest) # linear mixed model inference
library(afex) # ANOVA linear models
library(glmmTMB) # generalized linear models
library(MASS) # negative binomial and some other functions
library(car) # model checking and ANOVA
library(DHARMa) # model checking
library(mvtnorm)

# graphing packages
library(ggsci) # color palettes
library(ggpubr) # publication quality plots
library(ggforce) # better jitter
library(cowplot) # combine plots
library(knitr) # kable tables
library(kableExtra) # kable_styling tables
library(ggdendro) # dendrogram
library(dendextend) # better dendrogram
library(bootcluster) # bootstrap dendrogram
library(fpc) # cluster stability
library(mclust) # clustering

#library(pvc) # dendorogram stability
library(ggiraph)
library(GGally)

# plotly
library(plotly)
library(flexdashboard)
library(crosstalk)

# ggplot_the_model.R packages not loaded above
library(insight)
library(lazyWeave)

# use here from the here package
here <- here::here
# use clean_names from the janitor package
clean_names <- janitor::clean_names
# use transpose from data.table
transpose <- data.table::transpose

# load functions used by this text written by me
# ggplot_the_model.R needs to be in the folder "R"
# if you didn't download this and add to your R folder in your
# project, then this line will cause an error
#source_path <- here("R", "ggplot_the_model.R")
#source(source_path)

data_folder <- "data"
image_folder <- "images"
output_folder <- "output"

pal_okabe_ito <- c(
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7"
)
pal_okabe_ito_7 <- pal_okabe_ito[c(2,3,1,4,5,6,7)]
pal_okabe_ito_3 <- pal_okabe_ito[c(2,3,1)]
pal_okabe_ito_4 <- c(pal_okabe_ito_3, pal_okabe_ito[c(6)])


## ----deg_2_rad-------------------------------------------------------------------------------------
deg_2_rad <- function(x){
  rad <- x*pi/180
  return(rad)
}
  


## ----ggdendro-extensions---------------------------------------------------------------------------
# https://atrebas.github.io/post/2019-06-08-lightweight-dendrograms/
dendro_data_k <- function(hc, k) {
  hcdata    <-  ggdendro::dendro_data(hc, type = "rectangle")
  seg       <-  hcdata$segments
  labclust  <-  cutree(hc, k)[hc$order]
  segclust  <-  rep(0L, nrow(seg))
  heights   <-  sort(hc$height, decreasing = TRUE)
  height    <-  mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)
  
  for (i in 1:k) {
    xi      <-  hcdata$labels$x[labclust == i]
    idx1    <-  seg$x    >= min(xi) & seg$x    <= max(xi)
    idx2    <-  seg$xend >= min(xi) & seg$xend <= max(xi)
    idx3    <-  seg$yend < height
    idx     <-  idx1 & idx2 & idx3
    segclust[idx] <- i
  }
  
  idx                    <-  which(segclust == 0L)
  segclust[idx]          <-  segclust[idx + 1L]
  hcdata$segments$clust  <-  segclust
  hcdata$segments$line   <-  as.integer(segclust < 1L)
  hcdata$labels$clust    <-  labclust
  
  hcdata
}

set_labels_params <- function(nbLabels,
                              direction = c("tb", "bt", "lr", "rl"),
                              fan       = FALSE) {
  if (fan) {
    angle       <-  360 / nbLabels * 1:nbLabels + 90
    idx         <-  angle >= 90 & angle <= 270
    angle[idx]  <-  angle[idx] + 180
    hjust       <-  rep(0, nbLabels)
    hjust[idx]  <-  1
  } else {
    angle       <-  rep(0, nbLabels)
    hjust       <-  0
    if (direction %in% c("tb", "bt")) { angle <- angle + 45 }
    if (direction %in% c("tb", "rl")) { hjust <- 1 }
  }
  list(angle = angle, hjust = hjust, vjust = 0.5)
}

plot_ggdendro <- function(hcdata,
                          direction   = c("lr", "rl", "tb", "bt"),
                          fan         = FALSE,
                          scale.color = NULL,
                          branch.size = 1,
                          label.size  = 3,
                          nudge.label = 0.01,
                          expand.y    = 0.1) {
  

  direction <- match.arg(direction) # if fan = FALSE
  ybreaks   <- pretty(segment(hcdata)$y, n = 5)
  ymax      <- max(segment(hcdata)$y)
  
  ## branches
  p <- ggplot() +
    geom_segment(data         =  segment(hcdata),
                 aes(x        =  x,
                     y        =  y,
                     xend     =  xend,
                     yend     =  yend,
                     linetype =  factor(line),
                     colour   =  factor(clust)),
                 lineend      =  "round",
                 show.legend  =  FALSE,
                 size         =  branch.size)
  
  ## orientation
  if (fan) {
    p <- p +
      coord_polar(direction = -1) +
      scale_x_continuous(breaks = NULL,
                         limits = c(0, nrow(label(hcdata)))) +
      scale_y_reverse(breaks = ybreaks)
  } else {
    p <- p + scale_x_continuous(breaks = NULL)
    if (direction %in% c("rl", "lr")) {
      p <- p + coord_flip()
    }
    if (direction %in% c("bt", "lr")) {
      p <- p + scale_y_reverse(breaks = ybreaks)
    } else {
      p <- p + scale_y_continuous(breaks = ybreaks)
      nudge.label <- -(nudge.label)
    }
  }
  
  # labels
  labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
  hcdata$labels$angle <- labelParams$angle
  
  p <- p +
    geom_text(data        =  label(hcdata),
              aes(x       =  x,
                  y       =  y,
                  label   =  label,
                  colour  =  factor(clust),
                  angle   =  angle),
              vjust       =  labelParams$vjust,
              hjust       =  labelParams$hjust,
              nudge_y     =  ymax * nudge.label,
              size        =  label.size,
              show.legend =  FALSE)
  
  # theme
    # p <- p + theme_pubr() +
    #   theme(axis.text.x=element_blank())
  
  # colors and limits
  if (!is.null(scale.color)) {
    scale.color <- c("#000000", scale.color) #my addition
    p <- p + scale_color_manual(values = scale.color)
  }
  
  ylim <- -round(ymax * expand.y, 1)
  p    <- p + expand_limits(y = ylim)

  
  p
}



## ----treed-----------------------------------------------------------------------------------------
get_tree <- function(geobike_subset,
                  y_cols,
                  scale_it = TRUE,
                  center_it = TRUE,
                  hclust_method = "ward.D2"
){
  # dd <- dist(scale(geobike_subset[, .SD, .SDcols = y_cols],
  #                  center = center_it,
  #                  scale = scale_it),
  #            method = "euclidean")
  # dendro <- hclust(dd, method = hclust_method) %>%
  #   as.dendrogram() %>%
  #   place_labels(paste(geobike_subset[, model],
  #                      geobike_subset[, frame_size],
  #                      sep = ", "))
  
  cluster_data <- geobike_subset[, .SD, .SDcols = y_cols] %>%
    data.frame
  row.names(cluster_data) <- paste(geobike_subset[, model],
                                   geobike_subset[, frame_size],
                                   sep = ", ")
  d_matrix <- dist(scale(cluster_data,
                         center = center_it,
                         scale = scale_it),
                   method = "euclidean")
  hc <- hclust(d_matrix, method = hclust_method)
  return(hc)
  
}


## ----bike-geometry-helpers-------------------------------------------------------------------------
compute_axle_crown <- function(){
  
}

compute_chainstay_h <- function(bike){
  # the horizontal component of chainstay length 
  # bbd = bottom bracket drop
  # csl = chainstay length
  chainstay_h <- with(bike,
                      sqrt(chainstay_length^2 - bottom_bracket_drop^2))
  return(chainstay_h)
}


compute_offset_h <- function(bike){
  # the horizontal component of fork offset
  offset_h <- with(bike,
                 fork_offset_rake/sin(deg_2_rad(head_tube_angle)))
  return(offset_h)
}

compute_head_tube_h <- function(bike){
  # the horizontal component of head_tube
  head_tube_h <- with(bike,
               head_tube_length*cos(deg_2_rad(head_tube_angle)))
  return(head_tube_h)
}
compute_head_tube_v <- function(bike){
  # the vertical component of head_tube
  head_tube_v <- with(bike,
                      head_tube_length*sin(deg_2_rad(head_tube_angle)))
  return(head_tube_v)
}

compute_fork_angle <- function(bike){
  # angle of fork axle-crown axis to horizontal
  # beta is angle of fork axle-crow to offset line
  beta <- with(bike,
               acos(fork_offset_rake/axle_crown)*180/pi)
  # delta is angle from offset line to horizontal
  delta <- with(bike,
                90 - head_tube_angle)
  fork_angle <- beta - delta
  return(fork_angle)
}

compute_steering_v <- function(bike){
  # steering_v is the vertical component of the steering axis from top of head tube
  # to the horizontal line through wheel axles - so height from axle to head tube crown
  steering_v <- with(bike,
                     stack - bottom_bracket_drop)
  return(steering_v)
}

compute_steering_h <- function(bike){
  steering_v <- compute_steering_v(bike)
  steering_h <- with(bike,
                     steering_v/tan(deg_2_rad(head_tube_angle)))
  return(steering_h)
}



## ----missing data----------------------------------------------------------------------------------
compute_wheelbase <- function(bike){
  steering_h <- compute_steering_h(bike[is.na(wheelbase)])
  offset_h <- compute_offset_h(bike[is.na(wheelbase)])
  chainstay_h <- compute_chainstay_h(bike[is.na(wheelbase)])
  reach <- bike[is.na(wheelbase), reach]
  wheelbase <- chainstay_h + reach + steering_h + offset_h
  return(wheelbase)
}

# Solace OM3 does not specify head tube length. This can be
# computed using specs of Whisky MCX fork assuming this is
# the fork used to spec wheelbase
head_tube_length <- function(bike){
  offset_h <- compute_offset_h(bike)
  fork_angle <- compute_fork_angle(bike)
}

# Vagabond Genesis does not specify chainstay length.
compute_chainstay_length <- function(bike){
  head_tube_h <- compute_head_tube_h(bike)
  head_tube_v <- compute_head_tube_v(bike)
  fork_v <- with(bike,
                 stack - bottom_bracket_drop - head_tube_v)
  
  fork_h1 = with(bike,
                 fork_v/tan(deg_2_rad(head_tube_angle)))
  offset_h <- compute_offset_h(bike)
  chainstay_h <- with(bike,
                      wheelbase - reach - head_tube_h - fork_h1 - 
                        offset_h)
  
  chainstay <- with(bike,
                    sqrt(chainstay_h^2 + bottom_bracket_drop^2))
  
  return(chainstay)
}

compute_fork_offset <- function(bike){
  # steer_axis_h is base of triangle from top-head-tube to vertex created by steering axis and wheelbase.
  # tan hta <- stack/steer_axis_h
  steer_axis_v <- with(bike,
                       stack - bottom_bracket_drop)
  steer_axis_h <- with(bike,
                       steer_axis_v /
                         tan(deg_2_rad(head_tube_angle)))
  chainstay_h <- compute_chainstay_h(bike)
  offset_h <- with(bike,
                   wheelbase - chainstay_h - reach - steer_axis_h)
  offset <- with(bike,
                 offset_h * sin(deg_2_rad(head_tube_angle)))
  return(offset)
}

compute_effective_top_tube_length <- function(bike){
  # amigo bug out is missing this
  #
  seat_h <- with(bike,
                 stack/tan(deg_2_rad(seat_tube_angle)))
  effective_top_tube_length <- with(bike,
                                    seat_h + reach)
  return(effective_top_tube_length)
}


## --------------------------------------------------------------------------------------------------
estimate_axle_crown <- function(bike){
  # wb = rear_center + reach + headtube_h + headset_h + fork_h + rake_h
  hta <- bike[, head_tube_angle]
  wb <- bike[, wheelbase]
  rc <- bike[, rear_center]
  reach <- bike[, reach]
  headtube_h <- bike[, head_h]
  # would really need headset info, integrated vs zero stack vs external
  headset_l <- 2
  headset_h <- headset_l * cos(hta * pi/180)
  rake_h <- compute_offset_h(bike)
  zero_offset_fork_h <- wb - rc - reach - headtube_h - headset_h - rake_h # fork h wi
  fork_v <- tan(hta * pi/180) * zero_offset_fork_h
  fork_h <- zero_offset_fork_h + rake_h
  axle_crown <- sqrt(fork_v^2 + fork_h^2)
#  axle_crown <- rep(mean(axle_crown), length(axle_crown))
  return(axle_crown)

}


## --------------------------------------------------------------------------------------------------
geom_checker <- function(chainstay_length, # chainstay length
                         bottom_bracket_drop, # bottom bracket drop
                         reach,
                         stack,
                         head_tube_angle, # head tube angle
                         rake, # head tube length
                         wheelbase){ # wheelbase
  # do all the horizontal components add to wheelbase?
  chainstay_length_h <- compute_chainstay_h(bike)
  steer_axis_v <- stack - bottom_bracket_drop
  steer_axis_h <- steer_axis_v /
    tan(deg_2_rad(head_tube_angle))

  offset_h <- compute_offset_h(bike)
  wheelbase_computed <- chainstay_length_h + reach +
    steer_axis_h + offset_h

  }


## --------------------------------------------------------------------------------------------------
geometry_with_sus_fork <- function(frame,
                                   sus_length = 435,
                                   sus_travel = 40,
                                   sus_rake = 45,
                                   sus_sag = 0.1,
                                   headset_stack = 12){
  
  # note headset stack doesn't matter for this
  # frame <- geobike[model == "BMC URS AL 2022" & frame_size == "M", ]
  # frame <- geobike[model == "Sklar SS Ti 2025" & frame_size == "ML", ]
  # frame <- geobike[model == "Santa Cruz Stigmata CC 2023" & frame_size == "M", ]
  # check geom
  # stack = bbdrop + fork.v + headtube.v
  
  hta <- frame[1, head_tube_angle]
  sta <- frame[1, seat_tube_angle]
  rake <- frame[1, fork_offset_rake]
  wheelbase <- frame[1, wheelbase]
  fork_length <- frame[1, axle_crown]
  headtube_length <- frame[1, head_tube_length]
  bb_drop <- frame[1, bottom_bracket_drop]
  front_center <- frame[1, front_center]
  rear_center <- frame[1, rear_center]
  stack <- frame[1, stack]
  reach <- frame[1, reach]
 
  
  hta_r <- hta * pi/180
  fork_angle_r <- hta_r - asin(rake/fork_length)
  fork_angle <- fork_angle_r * 180/pi
  
  headtube.h <- headtube_length * cos(hta_r)
  headtube.v <- headtube_length * sin(hta_r)
  headset_h <- headset_stack * cos(hta_r)
  headset_v <- headset_stack * sin(hta_r)
  fork.h <- fork_length * cos(fork_angle_r)
  fork.v <- fork_length * sin(fork_angle_r)
  # check geom
  # bb_drop + fork.v + headtube.v + headset_v # computed stack
  # stack
  # rear_center + reach + headtube.h + headset_h + fork.h
  # wheelbase
  
  # check geom
  # stack = bbdrop + fork.v + headtube.v
  

  # new fork
  sag_length <- sus_length - sus_sag * sus_travel
  sus_fork_angle_r <- hta_r - asin(sus_rake/sag_length)
  sus_fork_angle <- sus_fork_angle_r * 180/pi # axle is now below horizon
  fork.h.new <- sag_length * cos(sus_fork_angle_r)
  fork.v.new<- sag_length * sin(sus_fork_angle_r)
  # sqrt(fork.x.new^2 + fork.y.new^2)

  dv = fork.v.new - fork.v
  dh = fork.h.new - fork.h

  alpha_r <- atan(dv/(wheelbase + dh)) # angle of rotation about rear axle
  alpha <- alpha_r * 180/pi
  wheelbase.new <- sqrt(dv^2 + (wheelbase + dh)^2)
  d_bb_drop <- rear_center/wheelbase * dv
  d_stack <- (rear_center+reach)/wheelbase * dv
  
  # rotate frame coordinates by alpha_r
  x_coords <- frame[1, .SD, .SDcols = paste0("x", 1:7)] |>
    as.numeric()
  y_coords <- frame[1, .SD, .SDcols = paste0("y", 1:7)] |>
    as.numeric()
  bike_coords <- matrix(c(x_coords, y_coords), ncol = 2)
  row.names(bike_coords) <- c("rear_axle", "top_effect_seat_tube", "top_head_tube",
                              "top_crown", "front_axle", "bottom_bracket",
                              "top_seat_tube")
  H = matrix(c(cos(alpha_r), -sin(alpha_r), sin(alpha_r), cos(alpha_r)),
             nrow = 2)
  new_coords <- bike_coords %*% H
  stack.new <- new_coords["top_head_tube", 2] - new_coords["bottom_bracket", 2]
  reach.new <- new_coords["top_head_tube", 1] - new_coords["bottom_bracket", 1]
  hta.new <- hta - alpha
  sta.new <- sta - alpha
  bb_drop.new <- -new_coords["bottom_bracket", 2]
  rear_center.new <- new_coords["bottom_bracket", 1]
  front_center.new <- wheelbase.new - rear_center.new

  # trail
  radius <- (ifelse(frame$wheel_size == 700 | frame$wheel_size == 29, 622, 584) + frame$tire_width_spec*2)/2
  offset_h <- sus_rake/sin(deg_2_rad(hta.new))

  trail.new <- radius/tan(hta_r) - offset_h
  
  # rear_axle = 1, top_seat = 2, top_head_tube = 3, top_crown = 4,
  # front_axle = 5, bottom_bracket = 6, top_seat_tube = 7
  
  measure_set <- c(
    sag_length = sag_length,
    wheelbase = wheelbase.new,
    stack = stack.new,
    reach = reach.new,
    hta = hta.new,
    sta = sta.new,
    bb_drop = bb_drop.new,
    rc = rear_center.new,
    fc = front_center.new,
    trail = trail.new
  )
  names(measure_set) <- c("axle_crown_sag", "wheelbase", "stack", "reach", "hta", "sta", "bb_drop", "rc", "fc", "trail")

  return(measure_set)


}  

  


## ----sus-checker, eval=FALSE-----------------------------------------------------------------------
#     sus_measures <- geometry_with_sus_fork(frame,
#                                            sus_length = 435,
#                                            sus_travel = 40,
#                                            sus_rake = 45,
#                                            sus_sag = .1,
#                                            headset_stack = 0)
# 
#   measure_list <- c("axle_crown", "wheelbase", "stack", "reach", "head_tube_angle", "seat_tube_angle", "bottom_bracket_drop", "rear_center", "front_center", "trail")
# 
#   rigid_measures <- frame[, .SD, .SDcols = measure_list]
# 
#   data.table(
#     measure = measure_list,
#     rigid = rigid_measures |> as.numeric(),
#     sus = sus_measures |> as.numeric()
#   )


## --------------------------------------------------------------------------------------------------
get_frame_size_letters <- function(frame_size){
  frame_size_letters <- str_replace_all(frame_size, "[^A-Za-z0-9]", "")
  
  frame_size_letters <- str_replace(frame_size_letters, "2X", "XX")
  frame_size_letters <- str_replace(frame_size_letters, "3X", "XXX")

# Extract letters
  frame_size_letters <- str_extract_all(frame_size_letters,
                                        "[A-Za-z]+")[[1]] |>
    paste0(collapse = "")

  frame_size_letters <- toupper(frame_size_letters)
  frame_size_letters <- str_remove(frame_size_letters, "CM")
  frame_size_letters <- str_remove(frame_size_letters, "SIZE")
  frame_size_letters <- str_replace(frame_size_letters, "SMALL", "S")
  frame_size_letters <- str_replace(frame_size_letters, "MEDIUM", "M")
  frame_size_letters <- str_replace(frame_size_letters, "LARGE", "L")
  frame_size_letters <- str_replace(frame_size_letters, "SM", "S")
  frame_size_letters <- str_replace(frame_size_letters, "MD", "M")
  frame_size_letters <- str_replace(frame_size_letters, "LG", "L")
  frame_size_letters <- str_replace(frame_size_letters, "MED", "M")
  frame_size_letters <- str_replace(frame_size_letters, "LRG", "L")
  frame_size_letters <- str_replace(frame_size_letters, "EXTRA", "X")
  frame_size_letters <- str_replace(frame_size_letters, "XM", "ML")
  
  # something like "SmallMedium" maps to small but should map to "SM"
  if(frame_size == "Small/Medium"){frame_size_letters <- "SM"}
  
  return(frame_size_letters)
}


## --------------------------------------------------------------------------------------------------
get_frame_size_numbers <- function(frame_size){
  frame_size_numbers <- str_replace_all(frame_size, "[^A-Za-z0-9]", "")
  frame_size_numbers <- str_replace(frame_size_numbers, "2X", "XX")
  frame_size_numbers <- str_replace(frame_size_numbers, "3X", "XXX")

# Extract numbers
  frame_size_numbers <- str_extract_all(frame_size_numbers,
                                        "[0-9]+")[[1]] |>
    paste0(collapse = "")
  frame_size_numbers <- str_replace(frame_size_numbers, "4446", "45")
  frame_size_numbers <- str_replace(frame_size_numbers, "4750", "49")
  frame_size_numbers <- str_replace(frame_size_numbers, "5153", "52")
  frame_size_numbers <- str_replace(frame_size_numbers, "5456", "55")
  frame_size_numbers <- str_replace(frame_size_numbers, "5759", "58")
  frame_size_numbers <- str_replace(frame_size_numbers, "6062", "61")
  if(nchar(frame_size_numbers) > 0){
    if(as.numeric(frame_size_numbers) > 100){
      frame_size_numbers <- as.numeric(frame_size_numbers)/10 
      frame_size_numbers <- as.character(frame_size_numbers)
    }
  }
  if(frame_size_numbers == ""){
    frame_size_numbers <- as.numeric(NA)
  }
  frame_size_numbers <- as.numeric(frame_size_numbers)

  return(frame_size_numbers)
}


## ----read-bike-function, echo=FALSE----------------------------------------------------------------
# data_path <- here(data_folder, "ghost_grappler.txt")
# dt <- fread(data_path)
# bike_label = "Tumbleweed Stargazer 2022"
# bike_range = "b1:h21"

read_bike <- function(bike_label = "Alchemy Lycos 2023",
                      bike_range = "a1:h23",
                      data_file = "gravel.xlsx",
                      sheet = "Sheet1"){
  data_path <- here(data_folder, data_file)
  bike_wide <- read_excel(data_path,
                          sheet = sheet,
                          range = bike_range) |>
    data.table()
  # re-read with coltype = numeric
  # col_type_list <- c("text", "text", rep("numeric", ncol(bike_wide)-2))
  # bike_wide <- read_excel(data_path,
  #                         sheet = bike_label,
  #                         range = bike_range,
  #                         col_types = col_type_list) %>%
  #   data.table
  
  
  bike_model <- substr(bike_label, 1, nchar(bike_label) - 5)
  model_year <- substr(bike_label,
                       nchar(bike_label) - 3,
                       nchar(bike_label))
  bike_wide <- bike_wide[, -2]
  bike <- data.table(
    model = paste(bike_model, model_year),
    year = model_year,
    transpose(bike_wide,
              keep.names = "frame_size",
              make.names = 1)
  )

  if(!("rider_min" %in% colnames(bike))){
    bike[, rider_min := as.numeric(NA)]
    bike[, rider_max := as.numeric(NA)]
  }
  if(!("trail" %in% colnames(bike))){
    bike[, trail := as.numeric(NA)]
  }
  if("axle_to_crown" %in% colnames(bike)){
    setnames(bike, old = "axle_to_crown", new = "axle_crown")
  }
  if(!("axle_crown" %in% colnames(bike))){
    bike[, axle_crown := as.numeric(NA)]
  }
  bike[, axle_crown_in_data := ifelse(!is.na(axle_crown), TRUE, FALSE)]

  keep_names <- c("model", "year", "frame_size",
                  "stack", "reach", "seat_tube_length", "top_tube_effective_length",
                  "head_tube_length", "seat_tube_angle", "head_tube_angle",
                  "chainstay_length", "wheelbase", "bottom_bracket_drop",
                  "fork_offset_rake", "axle_crown_in_data", "axle_crown", "trail",
                  "standover", "stem_length", "handlebar_width", "crank_length",
                  "wheel_size", "tire_width_spec", "tire_width_max",
                  "rider_min", "rider_max"
  )
  bike <- bike[, .SD, .SDcols = keep_names]
  
  # clean tire_width_max
  bike[, tire_width_max := round(tire_width_max, 0)]
  
  # clean frame size
  
  bike[, frame_size_letters := lapply(frame_size, get_frame_size_letters) |>
         unlist()]
  bike[, frame_size_numbers := lapply(frame_size, get_frame_size_numbers) |>
         unlist()]
  bike[, frame_size_orig := frame_size]
  bike[!is.na(frame_size_numbers) & !is.na(frame_size_letters),
       frame_size := paste0(frame_size_numbers, frame_size_letters)]
  bike[!is.na(frame_size_numbers) & is.na(frame_size_letters),
       frame_size := frame_size_numbers |> as.character()]
  bike[is.na(frame_size_numbers) & !is.na(frame_size_letters),
       frame_size := frame_size_letters]
  
  bike[, rear_center := sqrt(chainstay_length^2 - bottom_bracket_drop^2)] # horizontal
  
  # decompositions
  # seat_tube_v and seat_tube_h are decomp of seat tube
  bike[, seat_tube_v := seat_tube_length *
         sin(deg_2_rad(seat_tube_angle))]
  bike[, seat_tube_h := seat_tube_length *
         cos(deg_2_rad(seat_tube_angle))]
  # seat_v and seat_h are decomp of seat positioned at stack height
  # tan(STA) = seat_h/seat_v
  bike[, seat_v := stack]
  bike[, seat_h := stack /
         tan(deg_2_rad(seat_tube_angle))]
  # head_v and head_h are decomp of head tube
  bike[, head_v := head_tube_length * sin(deg_2_rad(head_tube_angle))]
  bike[, head_h := head_tube_length * cos(deg_2_rad(head_tube_angle))]

  # fill in missing
  # wheelbase
  bike[is.na(wheelbase), wheelbase := compute_wheelbase(bike[is.na(wheelbase)])]

  # chainstay_length
  bike[is.na(chainstay_length), chainstay_length := compute_chainstay_length(bike)]

  # fork_offset_rake
  bike[is.na(fork_offset_rake), fork_offset_rake := compute_fork_offset(bike)]

  # top_tube_effective_length
  bike[is.na(top_tube_effective_length), top_tube_effective_length := compute_effective_top_tube_length(bike)]

  # axle crown
  bike[, axle_crown_est := estimate_axle_crown(bike)]
  axle_crown_mean <- mean(bike[, axle_crown_est])
  bike[is.na(axle_crown), axle_crown := axle_crown_mean]
  
  # more stuff
  # effective seat post length
  bike[, seat_tube_effective_length := stack/sin(seat_tube_angle*pi/180)]
  
  # top tube angle
  bike[, seat_post_length := seat_tube_effective_length - seat_tube_length]
  bike[, top_tube_length := 
         sqrt(top_tube_effective_length^2 +
         seat_post_length^2 -
         2 * top_tube_effective_length * seat_post_length *
         cos(seat_tube_angle*pi/180))]
  bike[, top_tube_angle := 
         acos(
           -(seat_post_length^2 - 
              top_tube_length^2 - 
              top_tube_effective_length^2)/ 
             (2*top_tube_length*top_tube_effective_length)) * 180/pi]
  
  # constructed measures
  ## trail
  radius <- (ifelse(bike$wheel_size == 700 | bike$wheel_size == 29, 622, 584) + bike$tire_width_spec*2)/2
  # offset <- bike[, fork_offset_rake]
  # offset_h <- compute_offset_h(bike)
  # hta_r <- bike[, head_tube_angle] * pi/180
  # trail_est1 <- radius/tan(hta_r) - offset_h
  # trail_est2 <- (radius*cos(hta_r) - offset)/sin(hta_r)
  # trail_table <- data.table(
  #   radius = radius,
  #   offset = offset,
  #   offset_h = offset_h,
  #   hta = hta_r*180/pi,
  #   trail1 = trail_est1,
  #   trail2 = trail_est2
  # )
  
  # bike[, trail_est := radius/tan(head_tube_angle*pi/180) - 
  #        compute_offset_h(bike)]
  bike[, trail_est := (radius * cos(head_tube_angle*pi/180) - fork_offset_rake) /
         sin(head_tube_angle * pi / 180)]
  bike[is.na(trail), trail := trail_est]
  
  # trail_45 -- the trail that would occur with a 45 mmm tire on a 700c wheel
  radius_45 <- (622 + 45*2)/2
  bike[, trail_45 := (radius_45 * cos(head_tube_angle*pi/180) - fork_offset_rake) /
         sin(head_tube_angle * pi / 180)]
  
  ## bb height
  bike[, bb_height := radius - bottom_bracket_drop]

  
  bike[, model_size := paste(model, frame_size)]
  bike[, front_center := wheelbase - rear_center] # horizontal distance from bb to front axle
  bike[, front_end := front_center - reach] # horizontal distance from head tube to front axle
  bike[, seat_center := stack/tan(deg_2_rad(seat_tube_angle))]
  
  # ratios
  bike[, stack_reach := stack/reach]
  bike[, front_rear := front_center/rear_center]
  bike[, front_reach := front_end/reach]
  bike[, rear_wheelbase := rear_center/wheelbase]
  bike[, front_wheelbase := front_center/wheelbase]
  bike[, sta_hta := seat_tube_angle/head_tube_angle]

  
  bike[, effective_frame_reach := reach + seat_tube_h]
  bike[, effective_reach := reach + seat_tube_h + stem_length]

  # landmarks with rear axle as origin
  bike[, x1 := 0] # rear axle
  bike[, y1 := 0]
  bike[, x2 := rear_center - seat_h] # seat at stack height
  bike[, y2 := stack - bottom_bracket_drop]
  bike[, x3 := rear_center + reach] # head tube top
  bike[, y3 := stack - bottom_bracket_drop]
  bike[, x4 := x3 + head_h] # head tube base
  bike[, y4 := y3 - head_v]
  bike[, x5 := wheelbase] # front axle
  bike[, y5 := 0]
  bike[, x6 := rear_center] # bottom bracket
  bike[, y6 := -bottom_bracket_drop]
  bike[, x7 := rear_center - seat_tube_h] # seat tube
  bike[, y7 := seat_tube_v]
  
  # landmarks_named
  bike[, rear_x := x1]
  bike[, rear_y := y1]
  bike[, seat_x := x2]
  bike[, seat_y := y2]
  bike[, head_x := x3]
  bike[, head_y := y3]
  bike[, crown_x := x4]
  bike[, crown_y := y4]
  bike[, front_x := x5]
  bike[, front_y := y5]
  bike[, bottom_x := x6]
  bike[, bottom_y := y6]
  bike[, seattube_x := x7]
  bike[, seattube_y := y7]
  
  # geometry_with_sus_fork <- function(frame,
  #                                  sus_length = 425,
  #                                  sus_travel = 30,
  #                                  sus_rake = 45,
  #                                  headset_stack = 12){

  for(row_i in 1:nrow(bike)){
    sus_length = 425
    sus_travel = 30
    sus_rake = 45
    sus_sag = 0.1
    headset_stack = 0
    sus_measures <- geometry_with_sus_fork(
      bike[row_i], sus_length, sus_travel, sus_rake, sus_sag, headset_stack)
    bike[row_i, (paste("axle_crown", sus_travel, sus_rake)) :=
           sus_measures["axle_crown_sag"]]
    bike[row_i, (paste("wheelbase", sus_travel, sus_rake)) :=
           sus_measures["wheelbase"]]
    bike[row_i, (paste("stack", sus_travel, sus_rake)) :=
           sus_measures["stack"]]
    bike[row_i, (paste("reach", sus_travel, sus_rake)) :=
           sus_measures["reach"]]
    bike[row_i, (paste("head_tube_angle", sus_travel, sus_rake)) :=
           sus_measures["hta"]]
    bike[row_i, (paste("seat_tube_angle", sus_travel, sus_rake)) :=
           sus_measures["sta"]]
    bike[row_i, (paste("bottom_bracket_drop", sus_travel, sus_rake)) :=
           sus_measures["bb_drop"]]
    bike[row_i, (paste("rear_center", sus_travel, sus_rake)) :=
           sus_measures["rc"]]
    bike[row_i, (paste("front_center", sus_travel, sus_rake)) :=
           sus_measures["fc"]]
    bike[row_i, (paste("trail", sus_travel, sus_rake)) :=
           sus_measures["trail"]]

    sus_length = 435
    sus_travel = 40
    sus_rake = 45
    sus_measures <- geometry_with_sus_fork(
      bike[row_i], sus_length, sus_travel, sus_rake, sus_sag, headset_stack)
    bike[row_i, (paste("axle_crown", sus_travel, sus_rake)) :=
           sus_measures["axle_crown_sag"]]
    bike[row_i, (paste("wheelbase", sus_travel, sus_rake)) :=
           sus_measures["wheelbase"]]
    bike[row_i, (paste("stack", sus_travel, sus_rake)) :=
           sus_measures["stack"]]
    bike[row_i, (paste("reach", sus_travel, sus_rake)) :=
           sus_measures["reach"]]
    bike[row_i, (paste("head_tube_angle", sus_travel, sus_rake)) :=
           sus_measures["hta"]]
    bike[row_i, (paste("seat_tube_angle", sus_travel, sus_rake)) :=
           sus_measures["sta"]]
    bike[row_i, (paste("bottom_bracket_drop", sus_travel, sus_rake)) :=
           sus_measures["bb_drop"]]
    bike[row_i, (paste("rear_center", sus_travel, sus_rake)) :=
           sus_measures["rc"]]
    bike[row_i, (paste("front_center", sus_travel, sus_rake)) :=
           sus_measures["fc"]]
    bike[row_i, (paste("trail", sus_travel, sus_rake)) :=
           sus_measures["trail"]]

    sus_length = 425
    sus_travel = 30
    sus_rake = 51
    sus_measures <- geometry_with_sus_fork(
      bike[row_i], sus_length, sus_travel, sus_rake, sus_sag, headset_stack)
    bike[row_i, (paste("axle_crown", sus_travel, sus_rake)) :=
           sus_measures["axle_crown_sag"]]
    bike[row_i, (paste("wheelbase", sus_travel, sus_rake)) :=
           sus_measures["wheelbase"]]
    bike[row_i, (paste("stack", sus_travel, sus_rake)) :=
           sus_measures["stack"]]
    bike[row_i, (paste("reach", sus_travel, sus_rake)) :=
           sus_measures["reach"]]
    bike[row_i, (paste("head_tube_angle", sus_travel, sus_rake)) :=
           sus_measures["hta"]]
    bike[row_i, (paste("seat_tube_angle", sus_travel, sus_rake)) :=
           sus_measures["sta"]]
    bike[row_i, (paste("bottom_bracket_drop", sus_travel, sus_rake)) :=
           sus_measures["bb_drop"]]
    bike[row_i, (paste("rear_center", sus_travel, sus_rake)) :=
           sus_measures["rc"]]
    bike[row_i, (paste("front_center", sus_travel, sus_rake)) :=
           sus_measures["fc"]]
    bike[row_i, (paste("trail", sus_travel, sus_rake)) :=
           sus_measures["trail"]]

    sus_length = 435
    sus_travel = 40
    sus_rake = 51
    sus_measures <- geometry_with_sus_fork(
      bike[row_i], sus_length, sus_travel, sus_rake, sus_sag, headset_stack)
    bike[row_i, (paste("axle_crown", sus_travel, sus_rake)) :=
           sus_measures["axle_crown_sag"]]
    bike[row_i, (paste("wheelbase", sus_travel, sus_rake)) :=
           sus_measures["wheelbase"]]
    bike[row_i, (paste("stack", sus_travel, sus_rake)) :=
           sus_measures["stack"]]
    bike[row_i, (paste("reach", sus_travel, sus_rake)) :=
           sus_measures["reach"]]
    bike[row_i, (paste("head_tube_angle", sus_travel, sus_rake)) :=
           sus_measures["hta"]]
    bike[row_i, (paste("seat_tube_angle", sus_travel, sus_rake)) :=
           sus_measures["sta"]]
    bike[row_i, (paste("bottom_bracket_drop", sus_travel, sus_rake)) :=
           sus_measures["bb_drop"]]
    bike[row_i, (paste("rear_center", sus_travel, sus_rake)) :=
           sus_measures["rc"]]
    bike[row_i, (paste("front_center", sus_travel, sus_rake)) :=
           sus_measures["fc"]]
    bike[row_i, (paste("trail", sus_travel, sus_rake)) :=
           sus_measures["trail"]]

    sus_length = 445
    sus_travel = 50
    sus_rake = 45
    sus_measures <- geometry_with_sus_fork(
      bike[row_i], sus_length, sus_travel, sus_rake, sus_sag, headset_stack)
    bike[row_i, (paste("axle_crown", sus_travel, sus_rake)) :=
           sus_measures["axle_crown_sag"]]
    bike[row_i, (paste("wheelbase", sus_travel, sus_rake)) :=
           sus_measures["wheelbase"]]
    bike[row_i, (paste("stack", sus_travel, sus_rake)) :=
           sus_measures["stack"]]
    bike[row_i, (paste("reach", sus_travel, sus_rake)) :=
           sus_measures["reach"]]
    bike[row_i, (paste("head_tube_angle", sus_travel, sus_rake)) :=
           sus_measures["hta"]]
    bike[row_i, (paste("seat_tube_angle", sus_travel, sus_rake)) :=
           sus_measures["sta"]]
    bike[row_i, (paste("bottom_bracket_drop", sus_travel, sus_rake)) :=
           sus_measures["bb_drop"]]
    bike[row_i, (paste("rear_center", sus_travel, sus_rake)) :=
           sus_measures["rc"]]
    bike[row_i, (paste("front_center", sus_travel, sus_rake)) :=
           sus_measures["fc"]]
    bike[row_i, (paste("trail", sus_travel, sus_rake)) :=
           sus_measures["trail"]]
    
    sus_length = 445
    sus_travel = 50
    sus_rake = 50
    sus_measures <- geometry_with_sus_fork(
      bike[row_i], sus_length, sus_travel, sus_rake, sus_sag, headset_stack)
    bike[row_i, (paste("axle_crown", sus_travel, sus_rake)) :=
           sus_measures["axle_crown_sag"]]
    bike[row_i, (paste("wheelbase", sus_travel, sus_rake)) :=
           sus_measures["wheelbase"]]
    bike[row_i, (paste("stack", sus_travel, sus_rake)) :=
           sus_measures["stack"]]
    bike[row_i, (paste("reach", sus_travel, sus_rake)) :=
           sus_measures["reach"]]
    bike[row_i, (paste("head_tube_angle", sus_travel, sus_rake)) :=
           sus_measures["hta"]]
    bike[row_i, (paste("seat_tube_angle", sus_travel, sus_rake)) :=
           sus_measures["sta"]]
    bike[row_i, (paste("bottom_bracket_drop", sus_travel, sus_rake)) :=
           sus_measures["bb_drop"]]
    bike[row_i, (paste("rear_center", sus_travel, sus_rake)) :=
           sus_measures["rc"]]
    bike[row_i, (paste("front_center", sus_travel, sus_rake)) :=
           sus_measures["fc"]]
    bike[row_i, (paste("trail", sus_travel, sus_rake)) :=
           sus_measures["trail"]]

    
  }
  
  return(bike)
}



## ----size-classes----------------------------------------------------------------------------------

assign_std_size <- function(geobike){
  
  # Trek checkpoint mapping old (numbers) to new (sizes) sizing
  # xs - less than 49
  # small - 49, 52
  # medium - 52, 54
  # ml - 56
  # large - 58
  # xl - 61
  
  # get some data on average size for M and L
  # average M is 175
  # average L is 183.5
  median_sizes <- c(
    # XS
    (median(geobike[frame_size_letters == "XS", rider_min], na.rm=TRUE) +
      median(geobike[frame_size_letters == "XS", rider_max], na.rm=TRUE))/2,
    # S
    (median(geobike[frame_size_letters == "S", rider_min], na.rm=TRUE) +
      median(geobike[frame_size_letters == "S", rider_max], na.rm=TRUE))/2,
    # M
    (median(geobike[frame_size_letters == "M", rider_min], na.rm=TRUE) +
      median(geobike[frame_size_letters == "M", rider_max], na.rm=TRUE))/2,
    # L
    (median(geobike[frame_size_letters == "L", rider_min], na.rm=TRUE) +
      median(geobike[frame_size_letters == "L", rider_max], na.rm=TRUE))/2,
    # XL
    (median(geobike[frame_size_letters == "XL", rider_min], na.rm=TRUE) +
      median(geobike[frame_size_letters == "XL", rider_max], na.rm=TRUE))/2
  ) |> round(0)


#  geobike[, frame_size_std := as.numeric(NA)]
  for(i in 1:nrow(geobike)){
    rider_min <- geobike[i, rider_min]
    rider_max <- geobike[i, rider_max]
    if(!is.na(rider_min)){
      for(j in 1:length(median_sizes)){
        if(median_sizes[j] > rider_min & median_sizes[j] <= rider_max){
          geobike[i, frame_size_std := median_sizes[j]]
        }
      }
    }else{
      geobike[i, frame_size_std := NA]
    }
  }
  
# add size Medium
  model_list <- unique(geobike[, model])
  for(i in 1:length(model_list)){
    frame_size_std_list <- geobike[model == model_list[i], frame_size_std]
    frame_size_letters_list <- geobike[model == model_list[i],
                                       frame_size_letters]
    if(!175 %in% frame_size_std_list){
      geobike[model == model_list[i] & frame_size_letters == "M", frame_size_std := 175]
    }
  }

  for(i in 1:length(model_list)){
    frame_size_std_list <- geobike[model == model_list[i], frame_size_std]
    frame_size_numbers_list <- geobike[model == model_list[i],
                                       frame_size_letters]
    if(!175 %in% frame_size_std_list){
      geobike[model == model_list[i] &
                frame_size_numbers >= 53 &
                frame_size_numbers <= 54, frame_size_std := 175]
    }
  }
  
  # total models
  # length(model_list)
  # nrow(geobike[frame_size_std == 175, ])
  # setdiff(model_list, geobike[frame_size_std == 175, model])
  # geobike[model == "Rose Backroad XPLR", frame_size]

  return(geobike)
}


## ----import-bikes, echo=FALSE----------------------------------------------------------------------
# need to modify so all imports use this function
import_bikes <- function(style = "gravel",
                         prefix = ""){
  bike_list_file = paste0(style, "_list.txt")
  bike_file = paste0(style, ".xlsx")
  data_path <- here(data_folder, bike_list_file)
  bike_list <- fread(data_path)
  bike_data <- data.table(NULL)
  for(i in 1:nrow(bike_list)){
    bike_label_i <- as.character(bike_list[i, "model"])
    bike_range_i <- as.character(bike_list[i, "data_range"])
    bike_i <- read_bike(bike_label = bike_label_i,
                        bike_range = bike_range_i,
                        data_file = bike_file)
    bike_i[, my_fit := ifelse(frame_size == c(bike_list[i, "my_fit"]), TRUE, FALSE)]
    bike_data <- rbind(bike_data, bike_i)
  }
  bike_data[, restyle := paste0(prefix, str_to_sentence(style))]
  bike_data[, model_size := paste(model, frame_size)]
  return(bike_data)
}




## --------------------------------------------------------------------------------------------------
import_bikes_excel_multiple <- function(style = "gravel",
                         prefix = ""){
  bike_list_file = paste0(style, "_list.txt")
  bike_file = paste0(style, ".xlsx")
  data_path <- here(data_folder, bike_list_file)
  bike_list <- fread(data_path)
  geobike <- data.table(NULL)
  for(i in 1:nrow(bike_list)){
    bike_label_i <- as.character(bike_list[i, "model"])
    bike_range_i <- as.character(bike_list[i, "data_range"])
    bike_i <- read_bike(bike_label = bike_label_i,
                        bike_range = bike_range_i,
                        data_file = "gravel.xlsx",
                        sheet = bike_label_i)
    bike_i[, my_fit := ifelse(frame_size == c(bike_list[i, "my_fit"]), TRUE, FALSE)]
    geobike <- rbind(geobike, bike_i)
  }
  
 # add new specs
  bike_spec_list <- c("material", "udh", "dropout", "x700c_width_max", "x650b_tire_max", "suspension_corrected", "fork_suspension", "stem_suspension", "rear_suspension", "rear_axle", "front_axle", "q_factor", "seatpost_diameter", "routing_shifter", "routing_dropper", "routing_dynamo", "front_derailleur", "chainring_1", "chainring_2", "bottom_bracket", "mounts_inner", "mounts_under", "mounts_top", "mounts_fork", "mounts_fender_rear", "mounts_fender_front", "mounts_rack", "iso_astm", "rotor_max_front", "rotor_max_rear", "frameset", "base_build", "currency", "internal_storage")
  
  bike_specs <- data.table(
    bike_spec_list = bike_spec_list,
    value = as.numeric(NA)
  ) |>
    transpose(make.names = 1)

  geobike <- cbind(geobike, bike_specs)
  
 return(geobike)
}


## --------------------------------------------------------------------------------------------------
import_bikes_excel_single <- function(style_folder = "gravel",
                         prefix = ""){
  bike_list_file = paste0(paste0(style_folder, "/"), style_folder, "_list.txt")
  bike_list_path <- here(data_folder, bike_list_file)
  bike_list <- fread(bike_list_path, sep = "\t", header = FALSE)
  geobike <- data.table(NULL)
  for(i in 1:nrow(bike_list)){
    bike_file = bike_list[i]
    bike_path <- here(data_folder, style_folder, bike_file)
    model_data <- read_excel(bike_path, range = "A1:B5", col_names = FALSE,
                          .name_repair = "unique_quiet") |>
      data.table()
    
    # geometry range
    geom_range <- read_excel(bike_path, range = "B7:B7", col_names = FALSE,
                          .name_repair = "unique_quiet") |> as.character()
    last_row <- substr(geom_range, (nchar(geom_range)-1), nchar(geom_range)) |>
      as.integer()
    
    # specs table
    first_row <- last_row + 2
    last_row <- first_row + 100
    specs_range <- paste0("a", first_row, ":b", last_row)
    
    bike_specs <- read_excel(bike_path, range = specs_range, col_names = TRUE) |>
      clean_names() |>
      data.table()
    bike_specs <- bike_specs[!apply(is.na(bike_specs), 1, all), ]
    bike_specs_t <- transpose(bike_specs, make.names = 1)
    
    bike_label <- paste(model_data[1, 2], model_data[2, 2], model_data[3, 2])
    bike_i <- read_bike(bike_label = bike_label,
                        bike_range = geom_range,
                        data_file = paste0(style_folder,"/", bike_file))
    bike_i[, my_fit := NA]
    bike_i <- cbind(bike_i, bike_specs_t )
    geobike <- rbind(geobike, bike_i)
  }
 return(geobike)
}


## ----import-bike-list, echo=FALSE------------------------------------------------------------------
import_bike_list <- function(style = "gravel",
                         prefix = ""){
  
  geobike <- rbind(
    import_bikes_excel_single("gravel", prefix),
    import_bikes_excel_single("gravel incomplete", prefix)
  )
  geobike[, id := .I]

  
  # add column of shape id for plots
  shape_list <- c(15,17,19,0,2)
  n_shapes <- length(shape_list)
  n_models <- length(unique(geobike[, model]))
  n_recycles <- floor(n_models/n_shapes)
  left_over <- n_models - n_recycles*n_shapes
  model_2_shape_map <- c(rep(shape_list, n_recycles), shape_list[1:left_over])
  geobike[, shape_id := model_2_shape_map[as.integer(as.factor(model))]]
  
  y_cols <- c("rear_x", "rear_y",
              "seat_x", "seat_y",
              "head_x", "head_y",
              "crown_x", "crown_y",
              "front_x", "front_y",
              "bottom_x", "bottom_y",
              "seattube_x", "seattube_y")
  
  # center X at bottom bracket
  geobike[, rear_x := rear_x - bottom_x]
  geobike[, seat_x := seat_x - bottom_x]
  geobike[, head_x := head_x - bottom_x]
  geobike[, crown_x := crown_x - bottom_x]
  geobike[, front_x := front_x - bottom_x]
  geobike[, bottom_x := bottom_x - bottom_x]
  geobike[, seattube_x := seattube_x - bottom_x]
 
  setorder(geobike, "model")
  
  # get classifier size medium
  geobike <- assign_std_size(geobike)
  geobike[, frame_size_std_m := ifelse(frame_size_std == 175, TRUE, FALSE)]

  # set my_fit or classifier fit
  geobike[, my_fit := frame_size_std_m] 
  
  # max_tire size classes
  max_tire <- c("45", "47", "50", "53", "57", "57+")
  geobike[tire_width_max >= 45, tire_class := "45"]
  geobike[tire_width_max >= 47, tire_class := "47"]
  geobike[tire_width_max >= 50, tire_class := "50"]
  geobike[tire_width_max >= 53, tire_class := "53"]
  geobike[tire_width_max >= 57, tire_class := "57"]
  geobike[tire_width_max > 57, tire_class := "57+"]
  
  # axle-crown classes
  axle_crown_list <- c("< 409", "409", "415", "425", "435", "> 435")
  geobike[axle_crown >= 409, axle_crown_class := axle_crown_list[2]]
  geobike[axle_crown >= 415, axle_crown_class := axle_crown_list[3]]
  geobike[axle_crown >= 425, axle_crown_class := axle_crown_list[4]]
  geobike[axle_crown >= 435, axle_crown_class := axle_crown_list[5]]
  geobike[axle_crown > 435, axle_crown_class := axle_crown_list[6]]


  # order in legend of plots is S/M, S, M/L, M. Solution:
  # 1) add U200B after " S" and " M"
  # 2) replace U200B/ with U200F/

  # geobike[, frame_size := paste0(" ", frame_size)]
  # geobike[, frame_size := str_replace(frame_size, " XXS", paste0("\U200B", " XXS"))]
  # geobike[, frame_size := str_replace(frame_size, " XS", paste0("\U200C", " XS"))]
  # geobike[, frame_size := str_replace(frame_size, " S", paste0("\U200D", " S", "\U200B"))]
  # geobike[, frame_size := str_replace(frame_size, " M", paste0("\U200E", " M", "\U200B"))]
  # geobike[, frame_size := str_replace(frame_size, " XM", paste0("\U200E", " XM"))]
  # geobike[, frame_size := str_replace(frame_size, " L", paste0("\U200F", " L"))]
  # geobike[, frame_size := str_replace(frame_size, " XL", paste0("\U200F", " XL"))]
  # geobike[, frame_size := str_replace(frame_size, " XXL", paste0("\U200F", " XXL"))]
  
  # this screws up Scott addict gravel in a way that I cannot understand so this is a very kludgy wrangle
  # model != "Scott Addict Gravel" |
  # geobike[model != "Devinci Hatchet" & model != "Scott Addict Gravel",
  #         frame_size := str_replace(frame_size, paste0("\U200B", "/"), paste0("\U200F", "/"))]
  
  # add model-frame_size column
  geobike[, model_size := paste(model, frame_size)]
  setorder(geobike, model)
  geobike[, model_size := factor(model_size,
                                 levels = unique(model_size))]
  
  # create size classes
  size_classes <- c("xxs","xs", "s", "m", "ml", "l", "xl", "xxl")
  geobike[, top_tube_size := discretize(top_tube_effective_length, method = "cluster", breaks = 8)]
  geobike[, frame_size_working := size_classes[as.integer(top_tube_size)]]
  geobike[, frame_size_working := factor(frame_size_working,
                                         levels = size_classes)]
  
  # create size class columns
  # for(height in seq(150, 195, by = 5)){
  #   geobike[, paste0("size",height) := ifelse(height >= rider_min & height < rider_max, TRUE, NA)] # ties go to larger bike
  # }
  
  # create my_fit alternative
  geobike[, Size := ifelse(my_fit == TRUE, "M/L (54-56)", "All sizes")] # was focal/non-focal

  return(geobike)
}


## --------------------------------------------------------------------------------------------------
classifier_scores <- function(dt, means){
  # create scores 
  # Get variables for each centroid
  V <- A_classifier_cols
  # example for A/B split
  W <- B_classifier_cols  # example for B1/B2 split
  data_scaled_A <- cbind(data_scaled[, .SD, .SDcols = c("model", "frame_size", "kmeans_cut_1")],
                         Y1)
  data_scaled_B <- cbind(data_scaled[, .SD, .SDcols = c("model", "frame_size", "kmeans_cut_2")],
                         Y3)
  # Compute centroids
  centroid_A <- colMeans(data_scaled_A[kmeans_cut_1 == 1, ..V]) |> t()
  centroid_B1 <- colMeans(data_scaled_B[kmeans_cut_2 == 1, ..W]) |> t()
  centroid_B2 <- colMeans(data_scaled_B[kmeans_cut_2 == 2, ..W]) |> t()
  
  # Compute distances (Euclidean)
  n <- nrow(data_scaled_A)
  k <- nrow(centroid_A)
  pA <- length(V)
  pB <- length(W)
  distances <- as.matrix(dist(rbind(centroid_A, data_scaled_A[, ..V])))
  dist_to_A <- distances[(k+1):(k+n), 1:k]/sqrt(pA)
  distances <- as.matrix(dist(rbind(centroid_B1, data_scaled_B[, ..W])))
  dist_to_B1 <- distances[(k+1):(k+n), 1:k]/sqrt(pB)
  distances <- as.matrix(dist(rbind(centroid_B2, data_scaled_B[, ..W])))
  dist_to_B2 <- distances[(k+1):(k+n), 1:k]/sqrt(pB)
  
  # Convert to similarity scores
  sim_A <- 1 / (dist_to_A + 1e-6)
  sim_B1 <- 1 / (dist_to_B1 + 1e-6)
  sim_B2 <- 1 / (dist_to_B2 + 1e-6)
  total <- sim_A + sim_B1 + sim_B2
  
  data_scaled <- data_scaled[, .SD, .SDcols = c("model", "frame_size", "kmeans_style")]
  data_scaled[, kmeans_racy_score := sim_A/total]
  data_scaled[, kmeans_relaxed_score := sim_B1/total]
  data_scaled[, kmeans_rowdy_score := sim_B2/total]
  
}


## --------------------------------------------------------------------------------------------------
get_centroids <- function(Y, cluster){
  centroids <- rowsum(Y, group = cluster) / as.vector(table(cluster))
  return(centroids)
}


## --------------------------------------------------------------------------------------------------
mclust_classifier <- function(dt,
                              k = 3,
                              classifier_cols = c(
                                "stack",
                                "reach",
                                "front_center",
                                "rear_center",
                                "head_tube_angle",
                                "seat_tube_angle",
                                "trail_45"
                              )){
  # dt should be my_fit
  data_info <- dt[, .SD,
                         .SDcols = c("model", "frame_size")]
  model <- data_info[, model]
  Y <- dt[my_fit == TRUE, .SD, .SDcols = classifier_cols] |>
    scale()
  row.names(Y) <- model

  mclust_result <- Mclust(Y, G = k)

  mclust_class <- data.table(
    data_info,
    init_cluster = mclust_result$classification
  )
  # check class so order is racy, relaxed, rowdy
  racy_class <- mclust_class[model == "Specialized Crux 2025", init_cluster]
  relaxed_class <- mclust_class[model == "Tumbleweed Stargazer 2022", init_cluster]
  rowdy_class <- mclust_class[model == "Santa Cruz Stigmata CC 2023", init_cluster]
  mclust_class[init_cluster == racy_class, cluster := 1]
  mclust_class[init_cluster == relaxed_class, cluster := 2]
  mclust_class[init_cluster == rowdy_class, cluster := 3]

  styles <- c("racy", "relaxed", "rowdy")
  mclust_class[, init_style := styles[cluster]]

  # get cluster means and reorder
  init_means <- mclust_result$parameters$mean
  centroids <- init_means[, c(racy_class, relaxed_class, rowdy_class)]
  colnames(centroids) <- styles
  centroids_t <- t(centroids)
  distances <- as.matrix(dist(rbind(centroids_t, Y)))
  distances <- distances[(k+1):nrow(distances), 1:3]
  scores <- 1/(distances + 1e-06)
  totals <- apply(scores, 1, sum)
  scores <- scores/totals
  colnames(scores) <- paste("init_", styles)
  # totals <- apply(similarities, 1, sum) #check!
  mclust_class <- cbind(mclust_class, scores)
  
  # recluster
  recluster <- apply(mclust_class[, .SD, .SDcols = paste("init_", styles)], 1, which.max)
  mclust_class[, mclust_style := factor(styles[recluster], levels = styles)]
  centroids_t <- get_centroids(Y, recluster)
  rownames(centroids_t) <- paste0("mclust_", styles)
  distances <- as.matrix(dist(rbind(centroids_t, Y)))
  distances <- distances[(k+1):nrow(distances), 1:3]
  scores <- 1/(distances + 1e-06)
  totals <- apply(scores, 1, sum)
  scores <- scores/totals * 100
  # totals <- apply(similarities, 1, sum) #check!
  mclust_class <- cbind(mclust_class, scores)
  return(mclust_class[, .SD, .SDcols = c("model", "frame_size", "mclust_style",
                                         "mclust_racy", "mclust_relaxed", "mclust_rowdy")])
}


## --------------------------------------------------------------------------------------------------
kmeans_classifier <- function(dt,
                              k = 3,
                              classifier_cols = c(
                                "stack",
                                "reach",
                                "front_center",
                                "rear_center",
                                "head_tube_angle",
                                "seat_tube_angle",
                                "trail_45"
                              )){
  # dt should be my_fit
  dt_info <- dt[, .SD,
                         .SDcols = c("model", "frame_size")]
  model <- dt_info[, model]
  Y <- dt[my_fit == TRUE, .SD, .SDcols = classifier_cols] |>
    scale()
  row.names(Y) <- model

  kmeans_result <- kmeans(Y, centers = k)

  kmeans_class <- data.table(
    dt_info,
    init_cluster = kmeans_result$cluster
  )
  # check class so order is racy, relaxed, rowdy
  racy_class <- kmeans_class[model == "Specialized Crux 2025", init_cluster]
  relaxed_class <- kmeans_class[model == "Tumbleweed Stargazer 2022", init_cluster]
  rowdy_class <- kmeans_class[model == "Santa Cruz Stigmata CC 2023", init_cluster]
  kmeans_class[init_cluster == racy_class, cluster := 1]
  kmeans_class[init_cluster == relaxed_class, cluster := 2]
  kmeans_class[init_cluster == rowdy_class, cluster := 3]

  styles <- c("racy", "relaxed", "rowdy")
  kmeans_class[, kmeans_style := styles[cluster]]
  
  centroids_t <- get_centroids(Y, kmeans_class$cluster)
  rownames(centroids_t) <- paste0("kmeans_", styles)
  distances <- as.matrix(dist(rbind(centroids_t, Y)))
  distances <- distances[(k+1):nrow(distances), 1:3]
  scores <- 1/(distances + 1e-06)
  totals <- apply(scores, 1, sum)
  scores <- scores/totals * 100
  # totals <- apply(similarities, 1, sum) #check!
  kmeans_class <- cbind(kmeans_class, scores)
  return(kmeans_class[, .SD, .SDcols = c("model", "frame_size", "kmeans_style",
                                         "kmeans_racy", "kmeans_relaxed", "kmeans_rowdy")])
}


## --------------------------------------------------------------------------------------------------
kmeans_classifier_explore <- function(dt){

  A_classifier_cols <- c(
      "stack",
      "reach",
      "front_center",
      "rear_center",
      "head_tube_angle",
      "seat_tube_angle",
      "trail_45"
  )
  dt_subset <- dt[my_fit == TRUE, .SD,
                         .SDcols = c("model", "frame_size", A_classifier_cols)]
  dt_info <- dt[my_fit == TRUE, .SD,
                         .SDcols = c("model", "frame_size")]
  model <- dt_subset[, model]
  Y1 <- dt_subset[, .SD, .SDcols = A_classifier_cols] |>
    scale()
  row.names(Y1) <- model
  dt_scaled_1 <- cbind(dt_info, Y1)
  
  # get kmeans
  kmeans_result_1 <- kmeans(Y1, centers = 2)
  dt_scaled_1[, kmeans_cut_1 := kmeans_result_1$cluster]
  # which class is santa cruz stigmata?
  rowdy_class <- dt_scaled[model == "Santa Cruz Stigmata CC 2023", kmeans_cut_1]
  if(rowdy_class == 1){
    # switch cluster id
    dt_scaled_1[, kmeans_cut_1 := ifelse(kmeans_cut_1 == 1, 2, 1)]
    rowdy_class <- 2
  }
  dt_subset <- dt_scaled_1[, .SD, .SDcols = c("model", "frame_size", "kmeans_cut_1")]
  dt <- merge(dt, dt_subset, by = c("model", "frame_size"), all.x = TRUE)

  B_classifier_cols <- c(
    "stack_reach",
    "rear_center"
  )
  racy_class <- 1
  dt_subset <- dt[my_fit == TRUE & kmeans_cut_1 == rowdy_class, .SD,
                         .SDcols = c("model", "frame_size", B_classifier_cols)]
  dt_info <- dt[my_fit == TRUE & kmeans_cut_1 == rowdy_class, .SD,
                         .SDcols = c("model", "frame_size")]
  model <- dt_subset[, model]
  Y2 <- dt_subset[, .SD, .SDcols = B_classifier_cols] |>
    scale()
  row.names(Y2) <- model
  dt_scaled_2 <- cbind(dt_info, Y2)
  kmeans_result_2 <- kmeans(Y2, centers = 2)
  dt_scaled_2[, kmeans_cut_2 := kmeans_result_2$cluster]
  if(dt_scaled_2[model == "Santa Cruz Stigmata CC 2023", kmeans_cut_2] == 1){
    # cluster 1 to cluster 2
    dt_scaled_2[, kmeans_cut_2 := ifelse(kmeans_cut_2 == 1, 2, 1)]
  }
  dt_scaled <- merge(dt_scaled_1, dt_scaled_2, by = c("model", "frame_size"), all.x = TRUE)
  dt_scaled[, kmeans_clust := ifelse(kmeans_cut_1 == racy_class, kmeans_cut_1, kmeans_cut_2 + 1)]
  style_levels <- c("Racy", "Relaxed", "Rowdy")
  dt_scaled[, kmeans_style := style_levels[kmeans_clust] |>
                  factor(levels = style_levels)]
  k_means_class_table <- merge(dt[, .SD, .SDcols = c("model", "frame_size")],
                               dt_scaled[, .SD, .SDcols = c("model", "kmeans_style")])
  
# get scaled values for B_classifier_cols for all bikes, centered and scaled by rows where kmeans_cut_1 = 2).
  dt_subset <- dt[my_fit == TRUE, .SD,
                         .SDcols = c(B_classifier_cols)]
  means <- attr(Y2, "scaled:center") |> t()
  sds <- attr(Y2, "scaled:scale") |> t()
  Y3 <- as.matrix(dt_subset) - matrix(means, nrow = nrow(dt_subset),
                                        ncol = length(means), byrow = TRUE)
  Y3 <- Y3/matrix(sds, nrow = nrow(dt_subset),
                                        ncol = length(sds), byrow = TRUE)
# create scores 
# Get variables for each centroid
V <- A_classifier_cols
      # example for A/B split
W <- B_classifier_cols  # example for B1/B2 split
dt_scaled_A <- cbind(dt_scaled[, .SD, .SDcols = c("model", "frame_size", "kmeans_cut_1")],
                       Y1)
dt_scaled_B <- cbind(dt_scaled[, .SD, .SDcols = c("model", "frame_size", "kmeans_cut_2")],
                       Y3)
# Compute centroids
centroid_A <- colMeans(dt_scaled_A[kmeans_cut_1 == 1, ..V]) |> t()
centroid_B1 <- colMeans(dt_scaled_B[kmeans_cut_2 == 1, ..W]) |> t()
centroid_B2 <- colMeans(dt_scaled_B[kmeans_cut_2 == 2, ..W]) |> t()

# Compute distances (Euclidean)
n <- nrow(dt_scaled_A)
k <- nrow(centroid_A)
pA <- length(V)
pB <- length(W)
distances <- as.matrix(dist(rbind(centroid_A, dt_scaled_A[, ..V])))
dist_to_A <- distances[(k+1):(k+n), 1:k]/sqrt(pA)
distances <- as.matrix(dist(rbind(centroid_B1, dt_scaled_B[, ..W])))
dist_to_B1 <- distances[(k+1):(k+n), 1:k]/sqrt(pB)
distances <- as.matrix(dist(rbind(centroid_B2, dt_scaled_B[, ..W])))
dist_to_B2 <- distances[(k+1):(k+n), 1:k]/sqrt(pB)

# Convert to similarity scores
sim_A <- 1 / (dist_to_A + 1e-6)
sim_B1 <- 1 / (dist_to_B1 + 1e-6)
sim_B2 <- 1 / (dist_to_B2 + 1e-6)
total <- sim_A + sim_B1 + sim_B2

dt_scaled <- dt_scaled[, .SD, .SDcols = c("model", "frame_size", "kmeans_style")]
dt_scaled[, kmeans_racy_score := sim_A/total]
dt_scaled[, kmeans_relaxed_score := sim_B1/total]
dt_scaled[, kmeans_rowdy_score := sim_B2/total]


return(k_means_class_table)
}


## --------------------------------------------------------------------------------------------------
jack_clust <- function(data, classifier_cols, two_deep = FALSE){
  
  #   kmeans k = 2, p.s = .84 cols = "stack", "reach", "front_center", "rear_center", "head_tube_angle", "seat_tube_angle", "trail_45",

  classifier_cols <- c(
      "stack",
      "reach",
      "front_center",
      "rear_center",
      "head_tube_angle",
      "seat_tube_angle",
      "trail_45",
      # # "trail",
      # # "axle_crown",
      # "tire_width_max",
      # "fork_offset_rake",
      # "bottom_bracket_drop",
      # "top_tube_angle",
      # "front_reach",
      # "front_rear",
      # "stack_reach",
      # "sta_hta",
      NULL)
  
  data <- geobike_import
  data_subset <- data[my_fit == TRUE, .SD,
                         .SDcols = c("model", "frame_size", classifier_cols)] |> na.omit()
  model <- data_subset[, model]

  Y <- data_subset[, .SD, .SDcols = classifier_cols] |>
    scale()
  row.names(Y) <- model
  prediction_strength <- prediction.strength(Y, Gmin=2, Gmax=5, M = 500,
                                             clustermethod = kmeansCBI)
  prediction_strength$mean.pred
  
  BIC <- mclustBIC(Y)
  plot(BIC)
  summary(BIC)

  mclust_result <- Mclust(Y, G = 3)
  mclust_scores <- data.table(
    model = model,
    class = mclust_result$classification
  )

  prediction_strength <- prediction.strength(Y, Gmin=2, Gmax=4, M = 50,
                                             clustermethod = hclustCBI,
                                             method = "ward.D2")
  prediction_strength$mean.pred
  
  kmeans_result <- kmeans(Y, centers = 2)
  data_subset[, kmeans_cut_1 := kmeans_result$cluster]
  data_subset <- data_subset[, .SD, .SDcols = c("model", "frame_size", "kmeans_cut_1")]
  data_out <- merge(data, data_subset, by = c("model", "frame_size"))

  # reclassify lev_1 = 2
    reclassifier_cols <- c(
      # "stack",
      # "reach",
      # "front_center",
      "rear_center",
      # "head_tube_angle",
      # "seat_tube_angle",
      # "trail_45",
      # "trail",
      # "axle_crown",
      # "tire_width_max",
      # "fork_offset_rake",
      # "bottom_bracket_drop",
      # "top_tube_angle",
      # "front_reach",
      # "front_rear",
      "stack_reach",
      NULL)

  stig_class <- data_subset[model == "Santa Cruz Stigmata CC 2023", kmeans_cut_1]

  data_subset2 <- data_out[my_fit == TRUE & kmeans_cut_1 == stig_class, .SD,
                         .SDcols = c("model", "frame_size", reclassifier_cols)] |> na.omit()
  model <- data_subset2[, model]
  Y2 <- data_subset2[, .SD, .SDcols = reclassifier_cols] |>
    scale()
  row.names(Y2) <- model
  prediction_strength <- prediction.strength(Y2, Gmin=2, Gmax=5, M = 500,
                                             clustermethod = kmeansCBI)
  prediction_strength$mean.pred
  
  
  prediction_strength <- prediction.strength(Y2, Gmin=2, Gmax=4, M = 50,
                                             clustermethod = hclustCBI,
                                             method = "ward.D2")
  prediction_strength$mean.pred

  kmeans_2 <- kmeans(Y2, centers = 2)
  data_subset2[, kmeans_cut_2 := kmeans_2$cluster]


  prediction_strength <- prediction.strength(Y2, Gmin=2, Gmax=4, M = 50,
                                             clustermethod = hclustCBI,
                                             method = "ward.D2")
  prediction_strength$mean.pred


  

  p <- ncol(Y)
  # single column deletes
  for(j in 1:p){
    Y_j <- Y[, -j]
    kmeans_result <- kmeans(Y_j, centers = 3)
    put_label <- paste0("v",j)
    kmeans_class[, (put_label) := kmeans_result$cluster]
  }
  
  if(two_deep == TRUE){
    # two column deletes
    for(j in 1:(p-1)){
      for(k in (j+1):p){
        Y_j <- Y[, -c(j,k)]
        kmeans_result <- kmeans(Y_j, centers = 3)
        put_label <- paste0("v",j,"_",k)
        kmeans_class[, (put_label) := kmeans_result$cluster]
      }
    }
  }
  
  kmeans_matrix <- kmeans_class[, 2:ncol(kmeans_class)]

  # convert each column to a pair of columns (not1 and not2)
  n <- nrow(kmeans_matrix)
  p <- ncol(kmeans_matrix)
  labels <- names(kmeans_matrix)
  l <- 0
  for(j in 1:p){
    l <- l+1
    get_label <- labels[j]
    put_label <- paste0("binary", l)
    kmeans_matrix[, (put_label) := ifelse(get(get_label)== 1, 0, 1)]
    l <- l+1
    put_label <- paste0("binary", l)
    kmeans_matrix[, (put_label) := ifelse(get(get_label)== 2, 0, 1)]
  }
  binary_cols <- paste0("binary", 1:l)
  cluster_data <- kmeans_matrix[, .SD, .SDcols = binary_cols] |>
    data.frame()
  row.names(cluster_data) <- model
  d_matrix <- dist(cluster_data, method = "binary")

  hclust_method = "complete"
  hc <- hclust(d_matrix, method = hclust_method)
  tree_v2_color <- dendro_data_k(hc, k = 3)

 
  

  

  
}
  
  


## ----gravel-classifier, echo=FALSE-----------------------------------------------------------------
tree_classifier <- function(
    data,
    y_cols,
    pca = FALSE,
    method = "ward.D2"
){
  
   classifier_cols <- c(
      "stack",
      "reach",
      "front_center",
      "rear_center",
      "head_tube_angle",
      "seat_tube_angle",
      "trail_45"
  )
  
  geobike_subset <- data[, .SD, .SDcols = c("model", "frame_size", classifier_cols)]

  Y <- geobike_subset[, .SD, .SDcols = classifier_cols] |>
    scale()

  if(pca == TRUE){
    eigen_decomp <- eigen(cov(Y, use = "pairwise.complete.obs"))
    E <- eigen_decomp$vectors
    L <- eigen_decomp$values
    rel_L <- L/sum(L)
    scores <- Y %*% E
    colnames(scores) <- paste0("pc", 1:ncol(Y))
    geobike_subset <- cbind(geobike_subset, scores)
    y_cols <- colnames(scores)
    scale_it <- FALSE
    center_it <- FALSE
  }
  
  style_levels <- c("Racy", "Relaxed", "Rowdy")

  # hierarchical cluster
  style_table <- geobike_subset[, .SD, .SDcols = c("model", "frame_size")]

  tree_v2 <- get_tree(geobike_subset,
                      classifier_cols,
                      scale_it = TRUE,
                      center_it = TRUE,
                      hclust_method = method) #"ward.D2"
  tree_v2_color <- dendro_data_k(tree_v2, k = 3)
  

  style_class <- tree_v2_color$labels %>%
    data.table()
  style_class[, model := tstrsplit(label, ",", keep = 1)]
  
  cluster_labels <- numeric(3)
  rowdy <- "Santa Cruz Stigmata CC 2023"
  cluster_labels[style_class[model == rowdy, clust]] <- "Rowdy"
  racy <- "OPEN U.P. 2022"
  cluster_labels[style_class[model == racy, clust]] <- "Racy"
  cluster_labels[which(cluster_labels == 0)] <- "Relaxed"
  
  style_class[, restyle := cluster_labels[clust]]
  style_table <- merge(style_table, style_class[, .SD, .SDcols = c("model", "restyle")], by = "model",
                       all.x = TRUE)
  # relevel and make integer
  style_table[, restyle := factor(restyle,
                                  levels = style_levels)]


  # use and save tree from all variables
  tree_path <- here("rds", "tree.Rds")
  saveRDS(tree_v2_color, tree_path)
  
  
  return(style_table[, .SD, .SDcols = c("model", "restyle")])
}




## --------------------------------------------------------------------------------------------------
functional_classifier <- function(data){
  # rowdy
  rowdy_cols <- c("reach", "front_center", "head_tube_angle", "trail")
  Y <- data[, .SD, .SDcols = rowdy_cols] |>
    as.matrix()
  # make more positive more rowdy
  Y[, "head_tube_angle"] <- -Y[, "head_tube_angle"]
  Y <- scale(Y)
  row_sum <- apply(Y, 1, sum)
  # find max and min sum if there were a bike that was at max/min for all traits
  row_min <- min(Y[, 1]) + min(Y[, 2]) + min(Y[, 3]) + min(Y[, 4])
  row_max <- max(Y[, 1]) + max(Y[, 2]) + max(Y[, 3]) + max(Y[, 4])
  data[, rowdy_score := (row_sum - row_min)/(row_max - row_min)]
  data[, rowdy_sum := (row_sum - min(row_sum))/diff(range(row_sum))]

  # relaxed
  relaxed_cols <- c("stack_reach", "rear_center", "tire_width_max")
  Y <- data[, .SD, .SDcols = relaxed_cols] |>
    as.matrix()
  Y <- scale(Y)
  row_sum <- apply(Y, 1, sum)
  # find max and min sum if there were a bike that was at max/min for all traits
  row_min <- min(Y[, 1]) + min(Y[, 2]) + min(Y[, 3])
  row_max <- max(Y[, 1]) + max(Y[, 2]) + max(Y[, 3])
  data[, relaxed_score := (row_sum - row_min)/(row_max - row_min)]
  data[, relaxed_sum := (row_sum - min(row_sum))/diff(range(row_sum))]

  # racy
  racy_cols <- c("stack_reach", "rear_center", "head_tube_angle", "trail")
  Y <- data[, .SD, .SDcols = racy_cols] |>
    as.matrix()
  # make more positive more racy
  Y[, "stack_reach"] <- -Y[, "stack_reach"] # low stack_reach = more racy
  Y[, "rear_center"] <- -Y[, "rear_center"] # short rear_center = more racy
  Y[, "trail"] <- -Y[, "trail"] # short trail = more racy
  Y <- scale(Y)
  row_sum <- apply(Y, 1, sum)
  # find max and min sum if there were a bike that was at max/min for all traits
  row_min <- min(Y[, 1]) + min(Y[, 2]) + min(Y[, 3]) + min(Y[, 4])
  row_max <- max(Y[, 1]) + max(Y[, 2]) + max(Y[, 3]) + max(Y[, 4])
  data[, racy_score := (row_sum - row_min)/(row_max - row_min)]
  data[, racy_sum := (row_sum - min(row_sum))/diff(range(row_sum))]
  
  data[, rowdy_ternary := rowdy_sum/(rowdy_sum + relaxed_sum + racy_sum)]
  data[, relaxed_ternary := relaxed_sum/(rowdy_sum + relaxed_sum + racy_sum)]
  data[, racy_ternary := racy_sum/(rowdy_sum + relaxed_sum + racy_sum)]
  
  data[rowdy_ternary > relaxed_ternary & rowdy_ternary > racy_ternary,
       functional_style := "Rowdy"]
  data[relaxed_ternary > rowdy_ternary & relaxed_ternary > racy_ternary,
       functional_style := "Relaxed"]
  data[racy_ternary > rowdy_ternary & racy_ternary > relaxed_ternary,
       functional_style := "Racy"]
  data[, functional_style_xtra := functional_style]
  data[rowdy_ternary > 0.5,
       functional_style_xtra := "Xtra_Rowdy"]
  data[relaxed_ternary > 0.5,
       functional_style_xtra := "Xtra_Relaxed"]
  data[racy_ternary > 0.5,
       functional_style_xtra := "Xtra_Racy"]

  style_levels <- c("Racy", "Relaxed", "Rowdy")
  data[, functional_style := factor(functional_style, levels = style_levels)]

  # View(data[, .SD, .SDcols = c("model", "rowdy_ternary", "relaxed_ternary", "racy_ternary", "functional_style", "functional_style_xtra")])
  return(data[, .SD, .SDcols = c("model", "rowdy_ternary", "relaxed_ternary", "racy_ternary", "functional_style", "functional_style_xtra")])
}


## ----gravel-scores, echo = FALSE-------------------------------------------------------------------
gravel_scores <- function(data, y_cols){

  gravel_style <- data[, restyle]
  gravel_style_matrix <- data.table(
    racy = ifelse(gravel_style == "Racy", 1, 0),
    relaxed = ifelse(gravel_style == "Relaxed", 1, 0),
    rowdy = ifelse(gravel_style == "Rowdy", 1, 0)
  ) |>
    as.matrix()
    
  # are class labels correlated with PCA scores?
  Y <- data.table(
    scale(data[, .SD, .SDcols = y_cols])) |>
    as.matrix()
  R <- cor(Y)
  decomp <- eigen(R)
  E <- decomp$vectors
  scores <- Y %*% E
  # cor(gravel_style_matrix, scores[, 1:4]) # not good
  
  # style vector
  E_raw <- cor(gravel_style_matrix, Y) |>
    t()
  EtE <- diag(3)
  diag(EtE) <- 1/sqrt(diag(t(E_raw) %*% E_raw))
  E <- E_raw %*% EtE
  # t(E) %*% E # check!
  scores <- Y %*% E
  # cor(gravel_style_matrix, scores) # better

  
  scores_0 <- apply(scores, 2, function(x) x - min(x))
  scores_1 <- apply(scores_0, 1, function(x) x/sum(x)) |>
    t()
  # apply(scores_1, 1, sum) # check!

  data[, racy := scores_1[, 1] * 100]
  data[, relaxed := scores_1[, 2] * 100]
  data[, rowdy := scores_1[, 3] * 100]
  return(data)
}



## ----import-gravel, eval=TRUE----------------------------------------------------------------------
import_it <- FALSE

   classifier_cols <- c(
      "stack",
      "reach",
      "front_center",
      "rear_center",
      "head_tube_angle",
      "seat_tube_angle",
      "trail_45"
  )   
  
equalize_tire_width = TRUE
geo_bike_path <- here("rds", "geobike.Rds")
my_fit_path <- here("rds", "my_fit.Rds")
if(import_it != TRUE){
  geobike <- readRDS(geo_bike_path)
  my_fit <- readRDS(my_fit_path)
}else{
  # import from xlsx
  geobike <- import_bike_list(style = "gravel")
  geobike_import <- copy(geobike)
  # classify bikes by kmeans
  # kmeans first splits into k = 2 groups because there is only good support for 2. Then splits
  # group 2 into two groups because there is good support for this.
  # then combines into 3 clusters
  # classify bikes
  kmeans_classes <- kmeans_classifier(
    geobike[my_fit == TRUE]
  )
  geobike <- merge(geobike, kmeans_classes, by = c("model", "frame_size"))

  mclust_classes <- mclust_classifier(geobike[my_fit == TRUE,])
  geobike <- merge(geobike, mclust_classes, by = c("model", "frame_size"))
  
  geobike <- merge(geobike, kmeans_classes, by = c("model", "frame_size"))

  geobike_classes <- tree_classifier(
    geobike[my_fit == TRUE],
    y_cols = classifier_cols,
    pca = FALSE,
    method = "ward.D2"
  )
  geobike <- merge(geobike, geobike_classes, by = "model")
#  geobike <- plyr::join(geobike, geobike_classes, by = "model")

  check_it <- FALSE
  if(check_it){
    geobike[my_fit == TRUE, .(N = .N), by = .(restyle)]
  }
  
  # functional class
  geobike_classes <- functional_classifier(
    geobike[my_fit == TRUE]
  )
  geobike <- merge(geobike, geobike_classes, by = "model")
  
  geobike[, color := pal_okabe_ito_3[as.integer(mclust_style)]]
  geobike[, color_tree := pal_okabe_ito_3[as.integer(restyle)]]
  geobike[, color_function := pal_okabe_ito_3[as.integer(functional_style)]]
  
  # my_fit
  my_fit <- geobike[my_fit == TRUE]
  my_fit <- my_fit[!is.na(restyle), ]
  
  # gravel scores
  my_fit <- gravel_scores(my_fit,
                          classifier_cols)
  
  # save
  saveRDS(geobike, geo_bike_path)
  saveRDS(my_fit, my_fit_path)
  
  # make images for index page
  
}



## ----eval=FALSE------------------------------------------------------------------------------------
# convert_multi_files <- function(){
# 
#   spec_data_labels <- c(
#     "material",
#     "udh",
#     "dropout",
#     "tire_width_max_700c",
#     "tire_width_max_650b",
#     "suspension_corrected",
#     "fork_suspension",
#     "stem_suspension",
#     "rear_suspension",
#     "rear_axle",
#     "front_axle",
#     "bottom_bracket",
#     "q_factor",
#     "seatpost_diameter",
#     "front_derailleur",
#     "max_chainring_1x",
#     "max_chainring_2x",
#     "rotor_max_front",
#     "rotor_max_rear",
#     "iso_astm",
#     "routing_shifter",
#     "routing_dropper",
#     "routing_dynamo",
#     "internal_storage",
#     "mounts_inner",
#     "mounts_under",
#     "mounts_top",
#     "mounts_fork",
#     "mounts_fender",
#     "mounts_rack_front",
#     "mounts_rack_rear",
#     "frameset",
#     "base_build",
#     "custom_builds",
#     "currency"
# )
#   bike_path <- here(data_folder,"gravel orig excel file/gravel.xlsx")
#   bike_list_file = "gravel orig excel file/gravel_list.txt"
#   bike_list_path <- here(data_folder, bike_list_file)
#   bike_list <- fread(bike_list_path, sep = "\t", header = TRUE)
#   colnames(bike_list)[1] <- "model_year"
# 
#   bike_list[, brand_model := substr(model_year, 1, (nchar(model_year)-4))]
#   bike_list[, year := substr(model_year, (nchar(model_year)-3), nchar(model_year))]
#   bike_list[, first_row := substr(data_range, 2, 2) |> as.integer()]
# 
#   for(i in 1:nrow(bike_list)){
# 
#     bike_file <- paste0(bike_list[i, model_year], ".xlsx")
#     bike_model_i <- data.table(
#       spec = c("brand", "model", "model_year", "input_date", "url"),
#       val = as.character(NA)
#     )
# 
#     sheet_i <- bike_list[i, model_year]
#     temp_split <- str_split(bike_list[i, brand_model], " ", n = 2)
#     bike_model_i[1, val := temp_split[[1]][1]]
#     bike_model_i[2, val := temp_split[[1]][2]]
#     bike_model_i[3, val := bike_list[i, year]]
# 
#     range_i <- bike_list[i, data_range]
#     first_row <- substr(range_i, 2, 2) |> as.integer()
# 
#     # if sheet has no model data
#     if(first_row == 1){
#       bike_model_i[4, val := bike_list[i, year]]
#     }
#     if(first_row == 6){
#       input_date_i <- read_excel(bike_path,
#                                 sheet = sheet_i,
#                                 range = "B3", col_names = FALSE)
#       colnames(input_date_i) <- c("input_date")
#       input_date_str <- format(input_date_i$`input_date`, "%d-%b-%Y")
# 
#       url_i <- read_excel(bike_path,
#                                 sheet = sheet_i,
#                                 range = "B4", col_names = FALSE)
# 
#       bike_model_i[4, val := input_date_str]
#       bike_model_i[5, val := url_i]
#     }
# 
#     bike_specs_t <- data.table(
#       spec_list = spec_data_labels,
#       value = as.character(NA)
#     )
#     bike_specs_t[spec_list == "material", value := bike_list[i, frame]]
# 
# 
#     # geometry
#     bike_geo <- read_excel(bike_path,
#                            sheet = sheet_i,
#                            range = range_i, col_names = TRUE) |>
#       data.table()
#     colnames(bike_geo)[1:2] <- c("frame_size", "frame size")
#     measures <- bike_geo[, frame_size]
#     if(!"frame_weight" %in% measures){
#       j <- which(measures == "wheel_size")
#       if(length(j) == 2){
#         # delete first wheel_size row
#         bike_geo <- bike_geo[-j[1]]
#         j <- j[2]
#       }
#       new_row <- data.table(
#         colnames = colnames(bike_geo),
#         frame_weight = as.numeric(NA)
#       ) |>
#         transpose(make.names = 1)
#       new_row[, frame_size := "frame_weight"]
#       bike_geo <- rbind(
#         bike_geo[1:(j-1),],
#         new_row,
#         bike_geo[j:nrow(bike_geo)]
#       )
#     }
#     data_range <- data.table(
#       var = "data_range",
#       range = paste0("a8:", letters[ncol(bike_geo)], (nrow(bike_geo) + 8))
#     )
# 
# 
#     # export
#     bike_path_out <- here(data_folder, "gravel_temp_incomplete", bike_file)
# 
#     wb <- createWorkbook()
#     addWorksheet(wb, "Sheet1") # Add a sheet if creating new
#     modifyBaseFont(wb, fontSize = 14, fontName = "Futura")
#     writeData(wb, sheet = "Sheet1", # model data
#               x = bike_model_i,
#               startRow = 1, startCol = 1,
#               colNames = FALSE, rowNames = FALSE)
#     writeData(wb, sheet = "Sheet1", # measured range
#               x = data_range,
#               startRow = 7, startCol = 1,
#               colNames = FALSE, rowNames = FALSE)
#     writeData(wb, sheet = "Sheet1", # geometry
#               x = bike_geo,
#               startRow = 8, startCol = 1,
#               colNames = TRUE, rowNames = FALSE)
#     writeData(wb, sheet = "Sheet1", # specs
#               x = bike_specs_t,
#               startRow = 10 + nrow(bike_geo), startCol = 1,
#               colNames = TRUE, rowNames = FALSE)
#     setColWidths(
#       wb,
#       sheet = "Sheet1",
#       cols = 1:2,
#       widths = 19
#     )
#     saveWorkbook(wb, bike_path_out, overwrite = TRUE)
# 
# 
#   }
# 
# 
# 
# }


## ----eval=FALSE------------------------------------------------------------------------------------
# convert_single_files <- function(){
#   input_spec_data_labels <- c(
#     "material",
#     "udh",
#     "dropout",
#     "tire_width_max_700c",
#     "tire_width_max_650b",
#     "suspension_corrected",
#     "fork_suspension",
#     "stem_suspension",
#     "rear_suspension",
#     "rear_axle",
#     "front_axle",
#     "q_factor",
#     "seatpost_diameter",
#     "routing_shifter",
#     "routing_dropper",
#     "routing_dynamo",
#     "front_derailleur",
#     "max_chainring_1x",
#     "max_chainring_2x",
#     "bottom_bracket",
#     "mounts_inner",
#     "mounts_under",
#     "mounts_top",
#     "mounts_fork",
#     "mounts_fender",
#     "mounts_fender_front",
#     "mounts_rack_rear",
#     "iso_astm",
#     "rotor_max_front",
#     "rotor_max_rear",
#     "frameset",
#     "base_build",
#     "currency",
#     "internal_storage"
# )
# 
#     spec_data_labels <- c(
#     "material",
#     "udh",
#     "dropout",
#     "tire_width_max_700c",
#     "tire_width_max_650b",
#     "suspension_corrected",
#     "fork_suspension",
#     "stem_suspension",
#     "rear_suspension",
#     "rear_axle",
#     "front_axle",
#     "bottom_bracket",
#     "q_factor",
#     "seatpost_diameter",
#     "front_derailleur",
#     "max_chainring_1x",
#     "max_chainring_2x",
#     "rotor_max_front",
#     "rotor_max_rear",
#     "iso_astm",
#     "routing_shifter",
#     "routing_dropper",
#     "routing_dynamo",
#     "internal_storage",
#     "mounts_inner",
#     "mounts_under",
#     "mounts_top",
#     "mounts_fork",
#     "mounts_fender",
#     "mounts_rack_front",
#     "mounts_rack_rear",
#     "frameset",
#     "base_build",
#     "custom_builds",
#     "currency"
# )
#   bike_list_file = "gravel old/gravel_list.txt"
#   bike_list_path <- here(data_folder, bike_list_file)
#   bike_list <- fread(bike_list_path, sep = "\t", header = FALSE)
# 
#   for(i in 1:nrow(bike_list)){
# 
#     bike_file = bike_list[i]
#     bike_path <- here(data_folder, "gravel old", bike_file)
#     bike_model_i <- read_excel(bike_path, range = "A1:B5", col_names = FALSE) |>
#       data.table()
#     # read date
#     input_date_i <- read_excel(bike_path, range = "B4", col_names = FALSE)
#     colnames(input_date_i) <- c("input_date")
#     input_date_str <- format(input_date_i$`input_date`, "%d-%b-%Y")
# 
#     # specs table
#     bike_specs <- read_excel(bike_path, range = "A6:AH7", col_names = TRUE) |>
#       clean_names() |>
#       data.table()
#     colnames(bike_specs) <- input_spec_data_labels
#     bike_specs$mounts_rack_front <- NA
#     bike_specs$custom_builds <- NA
#     bike_specs <- bike_specs[1, .SD, .SDcols = spec_data_labels]
#     row.names(bike_specs) <- "value"
#     bike_specs_t <- data.table(
#       spec_list = colnames(bike_specs),
#       value = bike_specs[1, .SD, .SDcols = spec_data_labels] |> as.character()
#     )
#     bike_specs_t[value == "NA", value := NA]
# 
#     # geometry
#     bike_range_i <- read_excel(bike_path, range = "B9:B9", col_names = FALSE) |> as.character()
#     bike_geo <- read_excel(bike_path, range = bike_range_i, col_names = TRUE) |>
#       data.table()
#     colnames(bike_geo)[1:2] <- c("frame_size", "frame size")
#     measures <- bike_geo[, frame_size]
#     if(!"frame_weight" %in% measures){
#       a <- 3
#     }
# 
#     data_range <- data.table(
#       var = "data_range",
#       range = paste0("a8:", letters[ncol(bike_geo)], (nrow(bike_geo) + 8))
#     )
# 
#     # export
#     bike_path_out <- here(data_folder, "gravel_temp", bike_file)
# 
#     wb <- createWorkbook()
#     addWorksheet(wb, "Sheet1") # Add a sheet if creating new
#     modifyBaseFont(wb, fontSize = 14, fontName = "Futura")
#     writeData(wb, sheet = "Sheet1", # model data
#               x = bike_model_i,
#               startRow = 1, startCol = 1,
#               colNames = FALSE, rowNames = FALSE)
#     writeData(wb, sheet = "Sheet1", # replace with formatted date
#               x = input_date_str,
#               startRow = 4, startCol = 2,
#               colNames = FALSE, rowNames = FALSE)
#     writeData(wb, sheet = "Sheet1", # measured range
#               x = data_range,
#               startRow = 7, startCol = 1,
#               colNames = FALSE, rowNames = FALSE)
#     writeData(wb, sheet = "Sheet1", # geometry
#               x = bike_geo,
#               startRow = 8, startCol = 1,
#               colNames = TRUE, rowNames = FALSE)
#     writeData(wb, sheet = "Sheet1",
#               x = bike_specs_t,
#               startRow = 10 + nrow(bike_geo), startCol = 1,
#               colNames = TRUE, rowNames = FALSE)
#     setColWidths(
#       wb,
#       sheet = "Sheet1",
#       cols = 1:2,
#       widths = 19
#     )
#     saveWorkbook(wb, bike_path_out, overwrite = TRUE)
# 
#   }
# 
# 
# 
# }


## ----add-bike-specs--------------------------------------------------------------------------------




## ----eval=FALSE------------------------------------------------------------------------------------
# "#56B4E9" "#E69F00" "#009E73"


## ----eval=FALSE------------------------------------------------------------------------------------
# 
# frontcenter_cols <- c("reach", "head_tube_angle", "fork_offset_rake") #
# frontcenter_y <- my_fit[, .SD, .SDcols = frontcenter_cols] |>
#   scale()
# classifier_y <- my_fit[, .SD, .SDcols = classifier_cols] |>
#   scale() |>
#   data.table()
# classifier_y[, fc_proxy := reach - head_tube_angle + fork_offset_rake]
# cor(classifier_y)


## ----base-plot, echo = FALSE-----------------------------------------------------------------------
base_plot <- function(data = geobike,
                      x_col = "reach",
                      y_col = "stack",
                      legend_col = "model_size", # the column with values in the legend
                      color_col = "restyle", # the column of marker colors
                      x_label = "Reach", y_label = "Stack",
                      x_info = NULL, y_info = NULL,
                      digits = 0,
                      dot_palette = pal_okabe_ito_7,
                      dot_opacity = 0.3,
                      same_xy_scale = TRUE
                      ){
  if(is.null(x_info)){x_info <- x_label}
  if(is.null(y_info)){y_info <- y_label}
  show_trace_1_legend <- ifelse(color_col == legend_col, TRUE, FALSE)
  n_colors <- length(levels(data[, get(color_col)]))
  
  # set range of axes
  min_data_x <- min(data[, get(x_col)], na.rm = TRUE)
  min_data_y <- min(data[, get(y_col)], na.rm = TRUE)
  max_data_x <- max(data[, get(x_col)], na.rm = TRUE)
  max_data_y <- max(data[, get(y_col)], na.rm = TRUE)
  range_x <- max_data_x - min_data_x
  range_y <- max_data_y - min_data_y
  range_axis_x <- range_x * 1.1
  range_axis_y <- range_y * 1.1
  if(same_xy_scale == TRUE){
    if(range_x > range_y){
      range_axis_y <- range_axis_y * range_x/range_y
    }else{
      range_axis_x <- range_axis_x * range_y/range_x
    }}
  min_axis_x <- (min_data_x + max_data_x)/2 - 0.5*range_axis_x
  max_axis_x <- (min_data_x + max_data_x)/2 + 0.5*range_axis_x
  min_axis_y <- (min_data_y + max_data_y)/2 - 0.5*range_axis_y
  max_axis_y <- (min_data_y + max_data_y)/2 + 0.5*range_axis_y
 
  p <- plot_ly(data, evaluate=TRUE) %>%
  add_trace(type = "scatter", mode = "markers",
            x = ~get(x_col),
            y = ~get(y_col),
            color = ~get(color_col),
            colors = dot_palette[1:n_colors],
            opacity = dot_opacity,
            size = 10,
            name = ~get(color_col),
            hoverinfo = "text",
            text = ~paste(model, frame_size,
                          "<br>Cat:", restyle,
                          paste0("<br>", x_info, ":"),
                          round(get(x_col), digits),
                          paste0("<br>", y_info, ":"),
                          round(get(y_col), digits)),
            showlegend = show_trace_1_legend
  ) %>% 
    layout(xaxis = list(title = x_label,
                        tickfont = list(size = 16), titlefont = list(size = 16),
                        range = c(min_axis_x, max_axis_x)),
           yaxis = list(title = y_label,
                        tickfont = list(size = 16), titlefont = list(size = 16),
                        range = c(min_axis_y, max_axis_y)),
           title = list(text = paste(y_label, "vs.", x_label),
                        x = 0,
                        xanchor = "left"),
           legend = list(font = list(size = 10),
                         itemsizing = "constant"),
           autosize = F, width = 800, height = 600,
           NULL
    )
  
  if(legend_col != color_col){
  p <- p  %>%
    add_trace(
      type = "scatter",
      mode = "markers",
      x = ~get(x_col),
      y = ~get(y_col),
      marker = list( 
        size = 14,
        opacity = 1,
        color = ~color,
        colors = pal_okabe_ito_3
      ),
      text = ~paste("\U2B05", model, frame_size),
      textfont = list(size = 12),
      name = ~model_size,
      textposition = "right",
      visible = "legendonly",
      showlegend = TRUE
    )
  }
  
 
  return(p)
}



## ----annotate, echo = FALSE------------------------------------------------------------------------
annotate_model <- function(p,
                           data = geobike,
                           x_col = "reach",
                           y_col = "stack",
                           g_col = "restyle",
                           text_col = "model",
                           text_target = "Giant Revolt X pro 1 long", # can be a vector
                           dx = 20,
                           dy = 20
                           ){
  subdata <- data[which(data[, get(text_col)] %in% text_target), ]
  p <- p %>%
    add_trace(data = subdata,
              x = ~get(x_col),
              y = ~get(y_col),
              color = ~get(g_col),
              type = "scatter",
              mode = "markers",
              marker = list(size = 16),
              showlegend = FALSE,
              NULL) %>%
    add_annotations(
      data = subdata,
      type = "text",
      x = ~get(x_col),
      y = ~get(y_col),
      xref = "x",
      yref = "y",
      ax = dx,
      ay = dy,
      showarrow = TRUE,
      text = ~paste(model, frame_size), 
      font = list(color = "black", size = 16))
  
  return(p) 
}


## ----base-ternary-plot, echo=FALSE-----------------------------------------------------------------
base_ternary <- function(
    data,
    axis_cols = c("racy","relaxed","rowdy"),
    axis_labels = c("Racy","Relaxed","Rowdy"),
    g_col = "restyle", # factor to determine color of plot points
    legend_color_col = "color" # column with color of legend
){
  axis_info <- function(title) {
    list(
      title = title,
      titlefont = list(
        size = 20
      ),
      tickfont = list(
        size = 15
      ),
      tickcolor = 'rgba(0,0,0,0)',
      ticklen = 5
    )
  }
  m <- list(
    l = 60,
    r = 60,
    b = 60,
    t = 60,
    pad = 4
  )
    a_col = axis_cols[1]
    b_col = axis_cols[2]
    c_col = axis_cols[3]

  fig <- data %>% plot_ly()
  fig <- fig %>%
    add_trace(
      type = 'scatterternary',
      mode = 'markers',
      a = ~get(a_col),
      b = ~get(b_col),
      c = ~get(c_col),
      color = ~get(g_col),
      colors = pal_okabe_ito_3,
      showlegend = FALSE,
      opacity = 0.5,
      marker = list( 
        size = 14,
        line = list('width' = 2)
      ),
      hoverinfo = "text",
      text = ~paste(model, frame_size,
                    "<br>Cat:", restyle,
                    paste("<br> ", axis_labels[1], ":", round(get(a_col), 1)),
                    paste("<br> ", axis_labels[2], ":", round(get(b_col), 1)),
                    paste("<br> ", axis_labels[3], ":", round(get(c_col), 1))
      )
    )
  
  fig <- fig  %>%
    add_trace(
      type = 'scatterternary',
      mode = 'markers',
      a = ~get(a_col),
      b = ~get(b_col),
      c = ~get(c_col),
      marker = list( 
        size = 14,
        opacity = 1,
        color = ~get(legend_color_col),
        colors = pal_okabe_ito_3
      ),
      text = ~paste(model, frame_size),
      textfont = list(size = 12),
      name = ~model,
      textposition = "right",
      visible = "legendonly",
      showlegend = TRUE
    )
  
  
  fig <- fig %>%
    layout(
      autosize = FALSE,
      margin = m,
      ternary = list(
        sum = 100,
        aaxis = axis_info(axis_labels[1]),
        baxis = axis_info(axis_labels[2]),
        caxis = axis_info(axis_labels[3])
      )
    )
  
  return(fig)  
}


## ----scatter-fig-----------------------------------------------------------------------------------
scatter_fig <- function(data = my_fit,
                        x_col = "reach", y_col = "stack", g_col = "model_size",
                        x_label = "Reach", y_label = "Stack",
                        x_info = NULL, y_info = NULL,
                        digits = 0,
                        jitter_x = 0, jitter_y = 0,
                        annotate_model = NULL,
                        dot_palette = pal_okabe_ito_7,
                        dot_opacity = 0.3,
                        same_xy_scale = TRUE){ # if units are same on x and y then scales should be preserved
  #shared_data <- highlight_key(data, ~model)
  if(is.null(x_info)){x_info <- x_label}
  if(is.null(y_info)){y_info <- y_label}
  restyle_legend <- ifelse(g_col == "restyle",
                           TRUE,
                           FALSE)
  n_colors <- length(levels(data[, restyle]))
  
  # set range of axes
  min_data_x <- min(data[, get(x_col)], na.rm = TRUE)
  min_data_y <- min(data[, get(y_col)], na.rm = TRUE)
  max_data_x <- max(data[, get(x_col)], na.rm = TRUE)
  max_data_y <- max(data[, get(y_col)], na.rm = TRUE)
  range_x <- max_data_x - min_data_x
  range_y <- max_data_y - min_data_y
  range_axis_x <- range_x * 1.1
  range_axis_y <- range_y * 1.1
  if(same_xy_scale == TRUE){
    if(range_x > range_y){
      range_axis_y <- range_axis_y * range_x/range_y
    }else{
      range_axis_x <- range_axis_x * range_y/range_x
    }}
  min_axis_x <- (min_data_x + max_data_x)/2 - 0.5*range_axis_x
  max_axis_x <- (min_data_x + max_data_x)/2 + 0.5*range_axis_x
  min_axis_y <- (min_data_y + max_data_y)/2 - 0.5*range_axis_y
  max_axis_y <- (min_data_y + max_data_y)/2 + 0.5*range_axis_y
  

  #data <- highlight_key(data, ~model)
  fig <- plot_ly(data, type = "scatter", mode = "markers",
                 x = ~jitter(get(x_col), jitter_x),
                 y = ~jitter(get(y_col), jitter_y),
                 color = ~restyle,
                 colors = dot_palette[1:n_colors],
                 opacity = dot_opacity,
                 size = 10,
                 name = ~get(g_col),
                 hoverinfo = "text",
                 text = ~paste(model, frame_size,
                               "<br>Cat:", restyle,
                               paste0("<br>", x_info, ":"), round(get(x_col), digits),
                               paste0("<br>", y_info, ":"), round(get(y_col), digits)),
                 showlegend = restyle_legend
  ) %>% 
    layout(xaxis = list(title = x_label,
                        tickfont = list(size = 16), titlefont = list(size = 16),
                        range = c(min_axis_x, max_axis_x)),
           yaxis = list(title = y_label,
                        tickfont = list(size = 16), titlefont = list(size = 16),
                        range = c(min_axis_y, max_axis_y)),
           legend = list(font = list(size = 10),
                         itemsizing = "constant"),
           title = list(text = paste(y_label, "vs.", x_label),
                        x = 0,
                        xanchor = "left"),
           autosize = F, width = 800, height = 600
    ) #%>%
#    highlight(on = "plotly_click", off = "plotly_doubleclick")
  
  if(g_col == "model_size"){
    fig <- fig  %>%
      add_text(text = ~paste("\U2B05", model, frame_size),
               textfont = list(size = 12, color = ~restyle),
               color = ~restyle,
               opacity = 1,
               symbol = "circle",
               textposition = "right",
               visible = "legendonly",
               sort = FALSE,
               showlegend = TRUE,
      )
  }
  
  if(!is.null(annotate_model)){
    for(j in 1:length(annotate_model)){
        fig <- fig %>% add_annotations(
        x = data[model == annotate_model[j], get(x_col)],
        y = data[model == annotate_model[j], get(y_col)],
        text = paste(data[model == annotate_model[j], model],
                     data[model == annotate_model[j], year]),
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 1,
        ax = 20,
        ay = -20,
#        arrowcolor = ~restyle,
        arrowcolor = "black",
#        font = list(color = ~restyle, size = 16)
        font = list(color = "black", size = 16)
      )
    }
  }
  
  # add style legend -- cannot get color to show
  # fig <- fig %>% add_annotations(
  #   x = 0,
  #   y = 1,
  #   xref = "paper",
  #   yref = "paper",
  #   text = paste0("\U23FA", "Race"),
  #   textfont = list(size = 10, color = pal_okabe_ito_4[1]),
  #   showarrow = F
  # )
  
  
  return(fig)
}



## ----scatter-fig-new, echo=FALSE-------------------------------------------------------------------
scatter_fig_new <- function(data = geobike,
                        x_col = "reach", y_col = "stack", g_col = "model_size",
                        x_label = "Reach", y_label = "Stack",
                        x_info = NULL, y_info = NULL,
                        digits = 0,
                        dot_palette = pal_okabe_ito_7,
                        dot_opacity = 0.3,
                        same_xy_scale = TRUE){ # if units are same on x and y then scales should be preserved

  y_cols <- c(x_col, y_col, g_col, "model_size", "restyle", "color", "Size")
  subdata <- na.omit(data[, .SD, .SDcols = y_cols])

  #shared_data <- highlight_key(data, ~model)
  bike_x <- highlight_key(subdata)
  
  if(is.null(x_info)){x_info <- x_label}
  if(is.null(y_info)){y_info <- y_label}
  restyle_legend <- ifelse(g_col == "restyle",
                           TRUE,
                           FALSE)
  n_colors <- length(levels(subdata[, restyle]))
  
  # set range of axes
  min_data_x <- min(subdata[, get(x_col)], na.rm = TRUE)
  min_data_y <- min(subdata[, get(y_col)], na.rm = TRUE)
  max_data_x <- max(subdata[, get(x_col)], na.rm = TRUE)
  max_data_y <- max(subdata[, get(y_col)], na.rm = TRUE)
  range_x <- max_data_x - min_data_x
  range_y <- max_data_y - min_data_y
  range_axis_x <- range_x * 1.1
  range_axis_y <- range_y * 1.1
  if(same_xy_scale == TRUE){
    if(range_x > range_y){
      range_axis_y <- range_axis_y * range_x/range_y
    }else{
      range_axis_x <- range_axis_x * range_y/range_x
    }}
  min_axis_x <- (min_data_x + max_data_x)/2 - 0.5*range_axis_x
  max_axis_x <- (min_data_x + max_data_x)/2 + 0.5*range_axis_x
  min_axis_y <- (min_data_y + max_data_y)/2 - 0.5*range_axis_y
  max_axis_y <- (min_data_y + max_data_y)/2 + 0.5*range_axis_y
  
  fig <- bike_x %>% plot_ly()
  fig <- fig %>%
    # add dots colored by restyle
    add_trace(
      type = "scatter",
      mode = "markers",
      x = ~get(x_col),
      y = ~get(y_col),
      color = ~restyle,
      colors = dot_palette[1:n_colors],
      opacity = dot_opacity,
      size = 10,
      showlegend = FALSE,
      marker = list( 
        size = 10,
        line = list('width' = 2)
      ),
      hoverinfo = "text",
      text = ~paste(model_size,
                    "<br>Cat:", restyle,
                    paste0("<br>", x_info, ":"), round(get(x_col), digits),
                    paste0("<br>", y_info, ":"), round(get(y_col), digits))
    )
  # superimpose dots colored by column "color" but using model_size as the legend item
  fig <- fig  %>%
    add_trace(
      type = "scatter",
      mode = "markers",
      x = ~get(x_col),
      y = ~get(y_col),
      marker = list( 
        size = 14,
        opacity = 1,
 #       color = ~restyle,
        color = ~color # color is a column in the data with the hex code for the color
 #       colors = dot_palette[1:n_colors]
      ),
      text = ~paste("\U2B05", model_size),
      textfont = list(size = 12),
      name = ~get(g_col),
      textposition = "right",
      visible = "legendonly",
      showlegend = TRUE
    ) %>% 
    layout(xaxis = list(title = x_label,
                        tickfont = list(size = 12), titlefont = list(size = 12),
                        range = c(min_axis_x, max_axis_x)),
           yaxis = list(title = y_label,
                        tickfont = list(size = 12), titlefont = list(size = 12),
                        range = c(min_axis_y, max_axis_y)),
           legend = list(font = list(size = 10),
                         itemsizing = "constant"),
           # title = list(text = paste(y_label, "vs.", x_label),
           #              x = 0.5,
           #              xanchor = "center"),
           autosize = F, width = 7*96, height = 5*96,
           NULL
    )
  
  
  # set up check boxes
  boxes <- filter_checkbox("size", "Size", bike_x, ~Size, inline = FALSE)
  # boxes$attribs$style <- css(font.size = "90%") # <-change the font size

# plot check boxes and fig
  p <- bscols(
    widths = c(8, 2), fig, boxes
  )
  return(p)
}


## ----scatter-fig-global, echo=FALSE----------------------------------------------------------------
scatter_fig_global <- function(data = geobike,
                        x_col = "reach", y_col = "stack", g_col = "model_size",
                        x_label = "Reach", y_label = "Stack",
                        x_info = NULL, y_info = NULL,
                        digits = 0,
                        color_col = "restyle",
                        dot_palette = pal_okabe_ito_7,
                        dot_opacity = 0.3,
                        same_xy_scale = TRUE,
                        set_autorange = TRUE,
                        add_regression = FALSE){ # if units are same on x and y then scales should be preserved
  
  # this version links all plots so that size can be filtered globally

  # y_cols <- c(x_col, y_col, g_col, "model_size", "restyle", "color", "Size")
  #subdata <- na.omit(data$origData()[, .SD, .SDcols = y_cols])

  #shared_data <- highlight_key(data, ~model)
  # bike_x <- highlight_key(subdata)
  
  if(is.null(x_info)){x_info <- x_label}
  if(is.null(y_info)){y_info <- y_label}
  restyle_legend <- ifelse(g_col == "restyle",
                           TRUE,
                           FALSE)
  n_colors <- length(levels(data$origData()[, get(color_col)]))
  
  # set range of axes
  min_data_x <- min(data$origData()[, get(x_col)], na.rm = TRUE)
  min_data_y <- min(data$origData()[, get(y_col)], na.rm = TRUE)
  max_data_x <- max(data$origData()[, get(x_col)], na.rm = TRUE)
  max_data_y <- max(data$origData()[, get(y_col)], na.rm = TRUE)
  range_x <- max_data_x - min_data_x
  range_y <- max_data_y - min_data_y
  range_axis_x <- range_x * 1.1
  range_axis_y <- range_y * 1.1
  if(same_xy_scale == TRUE){
    if(range_x > range_y){
      range_axis_y <- range_axis_y * range_x/range_y
    }else{
      range_axis_x <- range_axis_x * range_y/range_x
    }}
  min_axis_x <- (min_data_x + max_data_x)/2 - 0.5*range_axis_x
  max_axis_x <- (min_data_x + max_data_x)/2 + 0.5*range_axis_x
  min_axis_y <- (min_data_y + max_data_y)/2 - 0.5*range_axis_y
  max_axis_y <- (min_data_y + max_data_y)/2 + 0.5*range_axis_y
  
  fig <- data %>% plot_ly(width = 6.5*96, height = 4.5*96)
  fig <- fig %>%
    # add dots colored by get(color_col)
    add_trace(
      type = "scatter",
      mode = "markers",
      x = ~get(x_col),
      y = ~get(y_col),
      color = ~get(color_col),
      colors = dot_palette[1:n_colors],
      opacity = dot_opacity,
      size = 10,
      showlegend = FALSE,
      marker = list( 
        size = 10,
        line = list('width' = 2)
      ),
      hoverinfo = "text",
      text = ~paste(model_size,
                    "<br>Cat:", get(color_col),
                    paste0("<br>", x_info, ":"), round(get(x_col), digits),
                    paste0("<br>", y_info, ":"), round(get(y_col), digits))
    )
  # superimpose dots colored by column "color" but using model_size as the legend item
   fig <- fig  %>%
    add_trace(
      type = "scatter",
      mode = "markers",
      x = ~get(x_col),
      y = ~get(y_col),
      marker = list(
        size = 14,
        opacity = 1,
        # color = ~functional_style,
        # colors = dot_palette[1:n_colors]
        color = ~color # color is a column in the data with the hex code for the color
      ),
      text = ~paste("\U2B05", model_size),
      textfont = list(size = 12),
      name = ~get(g_col),
      textposition = "right",
      visible = "legendonly",
      showlegend = TRUE
    ) |>
    layout(xaxis = list(title = x_label,
                        tickfont = list(size = 12), titlefont = list(size = 12)
                        ),
           yaxis = list(title = y_label,
                        tickfont = list(size = 12), titlefont = list(size = 12)
                        ),
           legend = list(font = list(size = 10),
                         itemsizing = "constant"),
           title = list(text = paste(y_label, "vs.", x_label),
                        x = 0.5,
                        xanchor = "center"),
           # autosize = F, width = 7*96, height = 5*96,
           NULL
    )
   if(set_autorange == TRUE){
     fig <- fig |>
       layout(xaxis = list(autorange = TRUE),
              yaxis = list(autorange = TRUE))
     
   }else{
     fig <- fig |>
       layout(xaxis = list(range = c(min_axis_x, max_axis_x)),
              yaxis = list(range = c(min_axis_y, max_axis_y)))
   }
   
   if(add_regression == TRUE){
     xy <- data.table(
       x = data$origData()[, get(x_col)],
       y = data$origData()[, get(y_col)],
       style = data$origData()[, get(color_col)]
     ) |>
       na.omit()
     
     fit <- lm(y ~ x, data = xy)
     r <- cor(xy$x, xy$y)
     
     # style means
     xy_means <- xy[, .(x = mean(x),
                        y = mean(y)),
                    by = .(style)]
#     fit <- lm(y ~ x, data = xy_means)
#     r <- cor(xy_means$x, xy_means$y)
     
     fig <- fig |>
       add_trace(
         type = "scatter",
         mode = "lines",
         x = xy$x,
         y = fitted(fit),
         alpha = 1,
         name = 'prediction',
         hoverinfo = "text",
         text = ~paste(
             "<br>Cor:", round(r, 2)),
         NULL
       )
   }
  
  
  return(fig)
}


## ----output-as-R-file------------------------------------------------------------------------------
# highlight and run to put update into R folder
write_it_as_R <- FALSE
if(write_it_as_R == TRUE){
  knitr::purl("bike_geometry_project.Rmd")
  file_name <- "bike_geometry_project.R"
  r_path <- here::here(file_name)
  file.rename(file_name, r_path)
}

