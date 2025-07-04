## ----setup, message = FALSE, warning = FALSE-------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      knitr.kable.NA = '')
# wrangling packages
library(here) # here makes a project transportable
library(janitor) # clean_names
library(readxl) # read excel, duh!
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


## ----deg_2_rad-------------------------------------------
deg_2_rad <- function(x){
  rad <- x*pi/180
  return(rad)
}
  


## ----ggdendro-extensions---------------------------------
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



## ----treed-----------------------------------------------
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


## ----bike-geometry-helpers-------------------------------
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



## ----missing data----------------------------------------
compute_wheelbase <- function(bike){
  steering_v <- compute_steering_v(bike)
  steering_h <- compute_steering_h(bike)
  offset_h <- compute_offset_h(bike)
  chainstay_h <- compute_chainstay_h(bike)
  wheelbase <- with(bike,
                    chainstay_h + reach + steering_h + offset_h)
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


## --------------------------------------------------------
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


## ----read-bike-function, echo=FALSE----------------------
# data_path <- here(data_folder, "ghost_grappler.txt")
# dt <- fread(data_path)
# bike_label = "Tumbleweed Stargazer 2022"
# bike_range = "b1:h21"

read_bike <- function(bike_label = "Breezer Radar X Pro 2022",
                      bike_range = "B1:I19",
                      data_file = "bikes.xlsx"){
  data_path <- here(data_folder, data_file)
  bike_wide <- read_excel(data_path,
                          sheet = bike_label,
                          range = bike_range) %>%
    data.table
  # re-read with coltype = numeric
  # col_type_list <- c("text", "text", rep("numeric", ncol(bike_wide)-2))
  # bike_wide <- read_excel(data_path,
  #                         sheet = bike_label,
  #                         range = bike_range,
  #                         col_types = col_type_list) %>%
  #   data.table
  
  
  bike_model <- substr(bike_label, 1, nchar(bike_label) - 5)
  model_year <- substr(bike_label,
                       nchar(bike_label) - 4,
                       nchar(bike_label))
  bike_wide <- bike_wide[, -2]
  bike <- data.table(
    model = bike_model,
    year = model_year,
    transpose(bike_wide,
              keep.names = "frame_size",
              make.names = 1)
  )

  if(!("rider_min" %in% colnames(bike))){
    bike[, rider_min := as.numeric(NA)]
    bike[, rider_max := as.numeric(NA)]
  }
  keep_names <- c("model", "year", "frame_size", "seat_tube_length", "top_tube_effective_length",
                  "head_tube_length", "seat_tube_angle", "head_tube_angle", "chainstay_length",
                  "wheelbase", "bottom_bracket_drop", "fork_offset_rake", "stack", "reach",
                  "standover", "stem_length", "handlebar_width", "crank_length", "wheel_size",
                  "tire_width_spec", "tire_width_max"
#                  , "rider_min", "rider_max"
  )
  bike <- bike[, .SD, .SDcols = keep_names]
  
  # fill in missing
    # wheelbase
  bike[, wheelbase := 
         ifelse(is.na(wheelbase),
                compute_wheelbase(bike),
                wheelbase)]

    # chainstay_length
  bike[, chainstay_length := 
         ifelse(is.na(chainstay_length),
                compute_chainstay_length(bike),
                chainstay_length)]
    # fork_offset_rake
  bike[, fork_offset_rake := 
         ifelse(is.na(fork_offset_rake),
                compute_fork_offset(bike),
                fork_offset_rake)]
  # top_tube_effective_length
  bike[, top_tube_effective_length := 
         ifelse(is.na(top_tube_effective_length),
                compute_effective_top_tube_length(bike),
                top_tube_effective_length)] 
  
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
  radius <- (ifelse(bike$wheel_size == 700 | bike$wheel_size == 29, 622, 584) + bike$tire_width_spec*2)/2
  ## trail
  bike[, trail := radius/tan(head_tube_angle*pi/180) - 
         compute_offset_h(bike)]
  # bike[, trail := (radius * cos(head_tube_angle*pi/180) - fork_offset_rake) / sin(head_tube_angle * pi / 180)]
  ## bb height
  bike[, bb_height := radius - bottom_bracket_drop]
  
# from wikipedia
# bike[, trail := ((diameter + tire_width_spec*2)/2 * cos(head_tube_angle*pi/180) -
#                     fork_offset_rake) / sin(head_tube_angle*pi/180)]
  bike[, model_size := paste(model, frame_size)]
  bike[, rear_center := sqrt(chainstay_length^2 - bottom_bracket_drop^2)] # horizontal
  bike[, front_center := wheelbase - rear_center] # horizontal
  bike[, seat_center := stack/tan(deg_2_rad(seat_tube_angle))]
  
  # ratios
  bike[, stack_reach := stack/reach]
  bike[, front_rear := front_center/rear_center]
  bike[, rear_wheelbase := rear_center/wheelbase]
  bike[, front_wheelbase := front_center/wheelbase]
  bike[, sta_hta := seat_tube_angle/head_tube_angle]

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
  
  return(bike)
}



## ----import-bikes, echo=FALSE----------------------------
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




## ----import-bikes-full, echo=FALSE-----------------------
import_bikes_full <- function(style = "gravel",
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
                        data_file = "gravel.xlsx")
    bike_i[, my_fit := ifelse(frame_size == c(bike_list[i, "my_fit"]), TRUE, FALSE)]
    geobike <- rbind(geobike, bike_i)
  }
  
  # my_fit: use 176 cm (I am 175.5)
  # add Breezer small to my_fit
  # geobike[model == "Breezer Radar X Pro" & frame_size == "48cm (S)", my_fit := TRUE]
  # add Boone 54 to my_fit
  # geobike[model == "Trek Boone 6" & frame_size == "54 cm", my_fit := TRUE]
  
  
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
  
  # clean frame_size
  setorder(geobike, "model")
  
  geobike[, frame_size := toupper(frame_size)]
  geobike[, frame_size := str_remove(frame_size, "CM")]
  geobike[, frame_size := str_remove(frame_size, "\"")]
  geobike[, frame_size := str_replace(frame_size, "SMALL", "S")]
  geobike[, frame_size := str_replace(frame_size, "MEDIUM", "M")]
  geobike[, frame_size := str_replace(frame_size, "LARGE", "L")]
  geobike[, frame_size := str_replace(frame_size, "SM", "S")]
  geobike[, frame_size := str_replace(frame_size, "MD", "M")]
  geobike[, frame_size := str_replace(frame_size, "LG", "L")]
  geobike[, frame_size := str_replace(frame_size, "MED", "M")]
  geobike[, frame_size := str_replace(frame_size, "LRG", "L")]
  geobike[, frame_size := str_replace(frame_size, "EXTRA-", "X")]
  geobike[, frame_size := str_replace(frame_size, "EXTRA ", "X")]
  geobike[, frame_size := str_replace(frame_size, "2X", "XX")]
  geobike[, frame_size := str_replace(frame_size, " - ", "/")]
  geobike[, frame_size := str_remove(frame_size, "-")]
  geobike[, frame_size := str_remove(frame_size, " ")]
  
  geobike[, frame_size := paste0(" ", frame_size)]
  geobike[, frame_size := str_replace(frame_size, " XXS", paste0("\U200B", " XXS"))]
  geobike[, frame_size := str_replace(frame_size, " XS", paste0("\U200C", " XS"))]
  geobike[, frame_size := str_replace(frame_size, " S", paste0("\U200D", " S", "\U200B"))]
  geobike[, frame_size := str_replace(frame_size, " M", paste0("\U200E", " M", "\U200B"))]
  geobike[, frame_size := str_replace(frame_size, " XM", paste0("\U200E", " XM"))]
  geobike[, frame_size := str_replace(frame_size, " L", paste0("\U200F", " L"))]
  geobike[, frame_size := str_replace(frame_size, " XL", paste0("\U200F", " XL"))]
  geobike[, frame_size := str_replace(frame_size, " XXL", paste0("\U200F", " XXL"))]
  
  # order is S/M, S, M/L, M. Solution:
  # 1) add U200B after " S" and " M"
  # 2) replace U200B/ with U200F/
  # this screws up Scott addict gravel in a way that I cannot understand so this is a very kludgy wrangle
  # model != "Scott Addict Gravel" |
  geobike[model != "Devinci Hatchet" & model != "Scott Addict Gravel",
          frame_size := str_replace(frame_size, paste0("\U200B", "/"), paste0("\U200F", "/"))]
  
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
  geobike[, Size := ifelse(my_fit == TRUE, "Focal", "Non-focal")]

  return(geobike)
}


## ----gravel-classifier, echo=FALSE-----------------------
gravel_classifier <- function(
    data,
    y_cols,
    pca = FALSE
){
  
  geobike_subset <- data[my_fit == TRUE, .SD, .SDcols = c("model", "frame_size", y_cols)]
  scale_it <- TRUE
  center_it <- TRUE

  if(pca == TRUE){
    Y <- geobike_subset[, .SD, .SDcols = y_cols] %>%
      as.matrix() %>%
      scale()
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
  
  style_table <- geobike_subset[, .SD, .SDcols = c("model", "frame_size")]
  style_levels <- c("Racy", "Relaxed", "Rowdy")
  n_fits <- length(y_cols) + 1
  for(j in 1:n_fits){
    if(j == n_fits){
      inc_cols <- y_cols
    }else{
      inc_cols <- y_cols[-j]
    }
    geobike_subset <- data[my_fit == TRUE, .SD, .SDcols = c("model", "frame_size", inc_cols)]
    tree_v2 <- get_tree(na.omit(geobike_subset),
                        inc_cols,
                        scale_it,
                        center_it,
                        hclust_method = "ward.D2")
    tree_v2_color <- dendro_data_k(tree_v2, k = 3)

    style_class <- tree_v2_color$labels %>%
      data.table()
    style_class[, model := tstrsplit(label, ",", keep = 1)]
    
    cluster_labels <- numeric(3)
    rowdy <- "Breezer Radar X Pro"
    cluster_labels[style_class[model == rowdy, clust]] <- "Rowdy"
    racy <- "OPEN U.P."
    cluster_labels[style_class[model == racy, clust]] <- "Racy"
    # relaxed <- "Mason InSearchOf"
    # cluster_labels[style_class[model == relaxed, clust]] <- "Relaxed"
    cluster_labels[which(cluster_labels == 0)] <- "Relaxed"
    
    style_class[, restyle := cluster_labels[clust]]
    style_table <- merge(style_table, style_class[, .SD, .SDcols = c("model", "restyle")], by = "model",
                         all.x = TRUE)
    # relevel and make integer
    style_table[, restyle := factor(restyle,
                                    levels = style_levels)]
    style_table[, restyle := as.numeric(as.integer(restyle))]
    
    setnames(style_table, "restyle", paste0("restyle", j))
  }
  style_cols <- paste0("restyle", 1:n_fits)
  style_table[, robust_restyle := style_levels[round(apply(style_table[, .SD, .SDcols = style_cols],
                                                  1, mean, na.rm = TRUE), 0)]]
  # use and save tree from all variables
  style_table[, restyle := style_levels[get(style_cols[n_fits])]]
  style_table[, restyle := ifelse(is.na(restyle), robust_restyle, restyle)]
  style_table[, restyle := factor(restyle, levels = style_levels)]
  tree_path <- here("rds", "tree.Rds")
  saveRDS(tree_v2_color, tree_path)
  
  # apply(style_table[, .SD, .SDcols = paste0("restyle", 1:n_fits)], 2, table)
  
  return(style_table[, .SD, .SDcols = c("model", "restyle")])
}




## ----gravel-scores, echo = FALSE-------------------------
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

  data[, racy := scores_1[, 1]]
  data[, relaxed := scores_1[, 2]]
  data[, rowdy := scores_1[, 3]]
  return(data)
}



## ----import-gravel, eval=TRUE----------------------------
import_it <- FALSE
classifier_cols <- c("stack", "reach",
                     "front_center","rear_center",
                     "head_tube_angle", "seat_tube_angle",
                     "fork_offset_rake",
#                     "bottom_bracket_drop",
#                     "trail"
#               "seat_tube_length",
#               "top_tube_angle",
               NULL)
# classifier_cols <- c("stack_reach",
#                      "front_rear",
#                      "sta_hta",
#                      "fork_offset_rake",
#                     "bottom_bracket_drop",
#                     "trail"
#               "seat_tube_length",
#               "top_tube_angle",
#               NULL)

  
equalize_tire_width = TRUE
geo_bike_path <- here("rds", "geobike.Rds")
my_fit_path <- here("rds", "my_fit.Rds")
if(import_it != TRUE){
  geobike <- readRDS(geo_bike_path)
  my_fit <- readRDS(my_fit_path)
}else{
  # import from xlsx
  geobike <- import_bikes_full(style = "gravel")
  # classify bikes
  geobike_classes <- gravel_classifier(
    geobike,
    y_cols = classifier_cols,
    pca = FALSE
  )
  geobike <- merge(geobike, geobike_classes, by = "model")
#  geobike <- plyr::join(geobike, geobike_classes, by = "model")

  check_it <- FALSE
  if(check_it){
    geobike[my_fit == TRUE, .(N = .N), by = .(restyle)]
  }
  
  geobike[, color := pal_okabe_ito_3[as.integer(restyle)]]
  
  # my_fit
  my_fit <- geobike[my_fit == TRUE]
  
  # gravel scores
  my_fit <- gravel_scores(my_fit,
                          classifier_cols)
  
  # save
  saveRDS(geobike, geo_bike_path)
  saveRDS(my_fit, my_fit_path)
  
  # make images for index page
  
}



## --------------------------------------------------------

frontcenter_cols <- c("reach", "head_tube_angle", "fork_offset_rake") #
frontcenter_y <- my_fit[, .SD, .SDcols = frontcenter_cols] |>
  scale()
classifier_y <- my_fit[, .SD, .SDcols = classifier_cols] |>
  scale() |>
  data.table()
classifier_y[, fc_proxy := reach - head_tube_angle + fork_offset_rake]
cor(classifier_y)


## ----base-plot, echo = FALSE-----------------------------
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



## ----annotate, echo = FALSE------------------------------
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


## ----base-ternary-plot, echo=FALSE-----------------------
base_ternary <- function(
    data,
    axis_cols = c("racy","relaxed","rowdy"),
    axis_labels = c("Racy","Relaxed","Rowdy"),
    g_col = "restyle"
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
                    paste("<br> ", axis_labels[1], ":", round(get(a_col)*100, 1)),
                    paste("<br> ", axis_labels[2], ":", round(get(b_col)*100, 1)),
                    paste("<br> ", axis_labels[3], ":", round(get(c_col)*100, 1))
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
        color = ~color,
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


## ----scatter-fig-----------------------------------------
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



## ----scatter-fig-new, echo=FALSE-------------------------
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


## ----scatter-fig-global, echo=FALSE----------------------
scatter_fig_global <- function(data = geobike,
                        x_col = "reach", y_col = "stack", g_col = "model_size",
                        x_label = "Reach", y_label = "Stack",
                        x_info = NULL, y_info = NULL,
                        digits = 0,
                        dot_palette = pal_okabe_ito_7,
                        dot_opacity = 0.3,
                        same_xy_scale = TRUE,
                        add_regression = FALSE){ # if units are same on x and y then scales should be preserved
  
  # this version links all plots so that size can be filtered globally

  y_cols <- c(x_col, y_col, g_col, "model_size", "restyle", "color", "Size")
  #subdata <- na.omit(data$origData()[, .SD, .SDcols = y_cols])

  #shared_data <- highlight_key(data, ~model)
  # bike_x <- highlight_key(subdata)
  
  if(is.null(x_info)){x_info <- x_label}
  if(is.null(y_info)){y_info <- y_label}
  restyle_legend <- ifelse(g_col == "restyle",
                           TRUE,
                           FALSE)
  n_colors <- length(levels(data$origData()[, restyle]))
  
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
           title = list(text = paste(y_label, "vs.", x_label),
                        x = 0.5,
                        xanchor = "center"),
           # autosize = F, width = 7*96, height = 5*96,
           NULL
    )
   
   if(add_regression == TRUE){
     xy <- data.table(
       x = data$origData()[, get(x_col)],
       y = data$origData()[, get(y_col)],
       style = data$origData()[, restyle]
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


## ----output-as-R-file------------------------------------
# highlight and run to put update into R folder
write_it_as_R <- FALSE
if(write_it_as_R == TRUE){
  knitr::purl("bike_geometry_project.Rmd")
  file_name <- "bike_geometry_project.R"
  r_path <- here::here(file_name)
  file.rename(file_name, r_path)
}

