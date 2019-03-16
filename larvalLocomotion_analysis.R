###############################################################################
#####################  Analysis of Larval Locomotion  #########################
###############################################################################

# Libraries
library("dplyr")
library("data.table")
library("ggplot2")
library("FSA")
library("rowr")

###############################################################################
#####################  Analyse one genotype at a time  ########################
###############################################################################

###############################################################################
############################### 111.1.1/+ #####################################
###############################################################################

# Load .csv output from AnimApp
# Pad non-analysed frames with NA, but before using cbind.call(), bind all
# data frames together using cbind() in order to see via the warnings()
# created how many frames couldn't be analysed - want to be below 100.
setwd("../videos/111.1.1/cropped/")
file_names <- list.files(pattern = ".csv")
data_11111 <- do.call(cbind.fill, c(lapply(file_names, fread), fill = NA))
setDT(data_11111)

# Get x-y values only
data_11111 <- data_11111[, -c("V3", "V4", "V5", "V6", "V7", "V8")]
colnames(data_11111) <- rep(c("x", "y"), 25)

# Check manually for each video whether larval path 'makes sense'
par(mfrow = c(5, 5))
for(i in seq(1, ncol(data_11111) - 1, 2)){
  plot(data_11111[, i:(i + 1)])
}

# Generate an offset table to prepare displacement analysis
data_11111_xy_offset <- rbind(data_11111[2:data_11111[, .N], ], 
                              data_11111[data_11111[, .N], ])
data_11111_displacement <- data_11111_xy_offset - data_11111



# Now, get displacement data.table and calculate movement
movement_11111 <- data_11111_displacement[, lapply(1:(ncol(.SD)/2), 
                                             function(x) sqrt((.SD[[2*x-1]])^2 + (.SD[[2*x]])^2))]

# Kill last row, because there is no movement for the last second
movement_11111 <- movement_11111[-(movement_11111[, .N]), ]

# Sum total displacement
total_displacement_11111 <- sapply(movement_11111, sum, na.rm = TRUE)

###############################################################################
############################### 132.1.1/+ #####################################
###############################################################################

# Load .csv output from AnimApp
setwd("../../132.1.1/cropped/")
file_names <- list.files(pattern = ".csv")
data_13211 <- do.call(cbind.fill, c(lapply(file_names, fread), fill = NA))
setDT(data_13211)

# Get x-y values only
data_13211 <- data_13211[, -c("V3", "V4", "V5", "V6", "V7", "V8")]
colnames(data_13211) <- rep(c("x", "y"), 26)

# Check manually for each video whether larval path 'makes sense'
par(mfrow = c(6, 5))
for(i in seq(1, ncol(data_13211) - 1, 2)){
  plot(data_13211[, i:(i + 1)])
}

###############################################################################

# Plot examples of a good and a bad tracking
well_tracked_13211 <- data_13211[, 9:10]
  
gg_well_tracked_13211 <- ggplot(well_tracked_13211, aes(x = x, y = y)) + 
  geom_point(shape = 4, size = 4) +
  theme_bw() +
  xlab("x / pixels") +
  ylab("y / pixels") +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50))
gg_well_tracked_13211

###############################################################################

# Generate an offset table to prepare displacement analysis
data_13211_xy_offset <- rbind(data_13211[2:data_13211[, .N], ], 
                              data_13211[data_13211[, .N], ])
data_13211_displacement <- data_13211_xy_offset - data_13211

# Now, get displacement data.table and calculate movement
movement_13211 <- data_13211_displacement[, lapply(1:(ncol(.SD)/2), 
                                                   function(x) sqrt((.SD[[2*x-1]])^2 + (.SD[[2*x]])^2))]

# Kill last row, because there is no movement for the last second
movement_13211 <- movement_13211[-(movement_13211[, .N]), ]

# Sum total displacement
total_displacement_13211 <- sapply(movement_13211, sum, na.rm = TRUE)


###############################################################################
################################## 7.1.1/+ ####################################
###############################################################################

# Load .csv output from AnimApp
setwd("../../7.1.1/cropped/")
file_names <- list.files(pattern = ".csv")
data_711 <- do.call(cbind.fill, c(lapply(file_names, fread), fill = NA))
setDT(data_711)

# Get x-y values only
data_711 <- data_711[, -c("V3", "V4", "V5", "V6", "V7", "V8")]
colnames(data_711) <- rep(c("x", "y"), 25)

# Check manually for each video whether larval path 'makes sense'
par(mfrow = c(5, 5))
for(i in seq(1, ncol(data_711) - 1, 2)){
  plot(data_711[, i:(i + 1)])
}

# Generate an offset table to prepare displacement analysis
data_711_xy_offset <- rbind(data_711[2:data_711[, .N], ], 
                              data_711[data_711[, .N], ])
data_711_displacement <- data_711_xy_offset - data_711

# Now, get displacement data.table and calculate movement
movement_711 <- data_711_displacement[, lapply(1:(ncol(.SD)/2), 
                                                   function(x) sqrt((.SD[[2*x-1]])^2 + (.SD[[2*x]])^2))]

# Kill last row, because there is no movement for the last second
movement_711 <- movement_711[-(movement_711[, .N]), ]

# Sum total displacement
total_displacement_711 <- sapply(movement_711, sum, na.rm = TRUE)


###############################################################################
################################# 25.1.1/+ ####################################
###############################################################################

# Load .csv output from AnimApp
setwd("../../25.1.1/cropped/")
file_names <- list.files(pattern = ".csv")
data_2511 <- do.call(cbind.fill, c(lapply(file_names, fread), fill = NA))
setDT(data_2511)

# Get x-y values only
data_2511 <- data_2511[, -c("V3", "V4", "V5", "V6", "V7", "V8")]
colnames(data_2511) <- rep(c("x", "y"), 28)

# Check manually for each video whether larval path 'makes sense'
par(mfrow = c(6, 5))
for(i in seq(1, ncol(data_2511) - 1, 2)){
  plot(data_2511[, i:(i + 1)])
}

###############################################################################

# Plot examples of a good and a bad tracking
well_tracked_2511 <- data_2511[, 35:36]

gg_well_tracked_2511 <- ggplot(well_tracked_2511, aes(x = x, y = y)) + 
  geom_point(shape = 4, size = 4) +
  theme_bw() +
  xlab("x / pixels") +
  ylab("y / pixels") +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50))
gg_well_tracked_2511

###############################################################################

# Generate an offset table to prepare displacement analysis
data_2511_xy_offset <- rbind(data_2511[2:data_2511[, .N], ], 
                              data_2511[data_2511[, .N], ])
data_2511_displacement <- data_2511_xy_offset - data_2511

# Now, get displacement data.table and calculate movement
movement_2511 <- data_2511_displacement[, lapply(1:(ncol(.SD)/2), 
                                                   function(x) sqrt((.SD[[2*x-1]])^2 + (.SD[[2*x]])^2))]

# Kill last row, because there is no movement for the last second
movement_2511 <- movement_2511[-(movement_2511[, .N]), ]

# Sum total displacement
total_displacement_2511 <- sapply(movement_2511, sum, na.rm = TRUE)


###############################################################################
################################# 137.1.3/+ ###################################
###############################################################################

# Load .csv output from AnimApp
setwd("../../137.1.3/cropped/")
file_names <- list.files(pattern = ".csv")
data_13713 <- do.call(cbind.fill, c(lapply(file_names, fread), fill = NA))
setDT(data_13713)

# Get x-y values only
data_13713 <- data_13713[, -c("V3", "V4", "V5", "V6", "V7", "V8")]
colnames(data_13713) <- rep(c("x", "y"), 31)

# Check manually for each video whether larval path 'makes sense'
par(mfrow = c(6, 6))
for(i in seq(1, ncol(data_13713) - 1, 2)){
  plot(data_13713[, i:(i + 1)])
}

# Generate an offset table to prepare displacement analysis
data_13713_xy_offset <- rbind(data_13713[2:data_13713[, .N], ], 
                             data_13713[data_13713[, .N], ])
data_13713_displacement <- data_13713_xy_offset - data_13713

# Now, get displacement data.table and calculate movement
movement_13713 <- data_13713_displacement[, lapply(1:(ncol(.SD)/2), 
                                                 function(x) sqrt((.SD[[2*x-1]])^2 + (.SD[[2*x]])^2))]

# Kill last row, because there is no movement for the last second
movement_13713 <- movement_13713[-(movement_13713[, .N]), ]

# Sum total displacement
total_displacement_13713 <- sapply(movement_13713, sum, na.rm = TRUE)


###############################################################################
###############################@## 72.1.1/+ ###################################
###############################################################################

# Load .csv output from AnimApp
setwd("../../72.1.1/cropped/")
file_names <- list.files(pattern = ".csv")
data_7211 <- do.call(cbind.fill, c(lapply(file_names, fread), fill = NA))

setDT(data_7211)

# Get x-y values only
data_7211 <- data_7211[, -c("V3", "V4", "V5", "V6", "V7", "V8")]
colnames(data_7211) <- rep(c("x", "y"), 29)

# Check manually for each video whether larval path 'makes sense'
par(mfrow = c(6, 5))
for(i in seq(1, ncol(data_7211) - 1, 2)){
  plot(data_7211[, i:(i + 1)])
}

###############################################################################

# Plot examples of bad tracking
poorly_tracked_7211 <- data_7211[, 57:58]

gg_poorly_tracked_7211 <- ggplot(poorly_tracked_7211, aes(x = x, y = y)) + 
  geom_point(shape = 4, size = 4) +
  theme_bw() +
  xlab("x / pixels") +
  ylab("y / pixels") +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50))
gg_poorly_tracked_7211

###############################################################################

# Remove poorly tracked larvae
data_7211 <- data_7211[, -c(57:58)]

# Generate an offset table to prepare displacement analysis
data_7211_xy_offset <- rbind(data_7211[2:data_7211[, .N], ], 
                             data_7211[data_7211[, .N], ])
data_7211_displacement <- data_7211_xy_offset - data_7211

# Now, get displacement data.table and calculate movement
movement_7211 <- data_7211_displacement[, lapply(1:(ncol(.SD)/2), 
                                                 function(x) sqrt((.SD[[2*x-1]])^2 + (.SD[[2*x]])^2))]

# Kill last row, because there is no movement for the last second
movement_7211 <- movement_7211[-(movement_7211[, .N]), ]

# Sum total displacement
total_displacement_7211 <- sapply(movement_7211, sum, na.rm = TRUE)

###############################################################################
################################## Plot #######################################
###############################################################################

# Make single data.table
# Adjust length of total_displacement vectors for plotting
max_length <- max(length(total_displacement_11111), length(total_displacement_13211),
                  length(total_displacement_13713), length(total_displacement_2511),
                  length(total_displacement_711), length(total_displacement_7211))

length(total_displacement_11111) <- max_length
length(total_displacement_13211) <- max_length
length(total_displacement_13713) <- max_length
length(total_displacement_2511) <- max_length
length(total_displacement_711) <- max_length
length(total_displacement_7211) <- max_length

total_displacement_combined <- cbind(total_displacement_13211, total_displacement_11111, total_displacement_711, 
                                     total_displacement_2511, total_displacement_13713, total_displacement_7211)

# Tidy matrix
total_displacement_combined <- melt(total_displacement_combined)

total_displacement_combined <- total_displacement_combined[, 2:3]
colnames(total_displacement_combined) <- c("recombinant", "distance")

# Add additional column for plot legend
total_displacement_combined$genotype <- factor(ifelse(total_displacement_combined$recombinant == "total_displacement_13211", "loxP",
                                                      ifelse(total_displacement_combined$recombinant == "total_displacement_11111", "loxP",
                                                             ifelse(total_displacement_combined$recombinant == "total_displacement_711", "loxP",
                                                                    ifelse(total_displacement_combined$recombinant == "total_displacement_2511", "GEPD",
                                                                           ifelse(total_displacement_combined$recombinant == "total_displacement_13713", "GEPD",
                                                                                  "GEPD"))))), levels = c("loxP", "GEPD"))

total_displacement_combined$HR_event <- factor(ifelse(total_displacement_combined$recombinant == "total_displacement_13211", "loxP_1",
                                                      ifelse(total_displacement_combined$recombinant == "total_displacement_11111", "loxP_2",
                                                             ifelse(total_displacement_combined$recombinant == "total_displacement_711", "loxP_3",
                                                                    ifelse(total_displacement_combined$recombinant == "total_displacement_2511", "GEPD_1",
                                                                           ifelse(total_displacement_combined$recombinant == "total_displacement_13713", "GEPD_2",
                                                                                  "GEPD_3"))))), levels = c("loxP_1", "loxP_2", "loxP_3", "GEPD_1", "GEPD_2", "GEPD_3"))

# Convert pixels to mm (conversion factor: 109 pixels / 30 mm --> divide by 3.63)
total_displacement_combined_noNA <- na.omit(total_displacement_combined)
setDT(total_displacement_combined_noNA)
total_displacement_combined_noNA_mm <- total_displacement_combined_noNA[, displacement := distance/3.63] 

# Plot with displacement in mm
gg <- ggplot(total_displacement_combined_noNA_mm, aes(x = HR_event, y = displacement)) + 
  geom_boxplot(aes(fill = genotype), outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(position = position_jitter(0.2), na.rm = TRUE, shape = 4, size = 6) +
  scale_shape_discrete(solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "mm / min") +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.text = element_text(size = 30),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(c(10, 10, 10, 10)),
        legend.title = element_blank(),
        legend.key.size = unit(2.0, 'cm'))
gg

###############################################################################
################################# Stats #######################################
###############################################################################

# Stats on individual replicates
kruskal.test(total_displacement_combined_noNA_mm$displacement ~ total_displacement_combined_noNA_mm$HR_event, 
             data = total_displacement_combined_noNA_mm)
dunnTest(total_displacement_combined_noNA_mm$displacement ~ total_displacement_combined_noNA_mm$HR_event, 
         data = total_displacement_combined_noNA_mm)

# Draw error bars
setDT(total_displacement_combined_noNA_mm)

# Remove NA's
total_displacement_combined_noNA_mm <- total_displacement_combined_noNA_mm[!is.na(total_displacement_combined_noNA_mm$distance), ]

max_distances <- total_displacement_combined_noNA_mm[, max(displacement, na.rm = TRUE), by = genotype]
max_loxp <- max_distances[1,2]
max_loxp <- as.numeric(max_loxp)
max_loxp <- max_loxp + 10

max_gepd <- max_distances[2,2]
max_gepd <- as.numeric(max_gepd)
max_gepd <- max_gepd + 10

# Draw error bars for gg
sign_loxp <- data.frame(HR_event = c("loxP_1", "loxP_2", "loxP_3"), displacement = rep(max_loxp, 3))
sign_gepd <- data.frame(HR_event = c("GEPD_1", "GEPD_2", "GEPD_3"), displacement = rep(max_gepd, 3))

gg2 <- gg + geom_line(data = sign_loxp, aes(x = HR_event, y = displacement, group = 1)) +
  annotate("text", x = "loxP_2", y = max_loxp + 5, label = "ns", size = 10)

gg3 <- gg2 + geom_line(data = sign_gepd, aes(x = HR_event, y = displacement, group = 1)) +
  annotate("text", x = "GEPD_2", y = max_gepd + 5, label = "ns", size = 10)

# Draw error bar for betwen-group stats
sign_combined <- data.frame(HR_event = c("loxP_2", "GEPD_2"), displacement = rep(max_loxp + 10, 2))
gg4 <- gg3 + geom_line(data = sign_combined, aes(x = HR_event, y = displacement, group = 1)) +
  annotate("text", x = 3.5, y = max_loxp + 15, label = "****", size = 10)
gg4

###############################################################################
############################# Rao analysis ####################################
###############################################################################

###############################################################################
################################### Paths #####################################
###############################################################################

## Below is modified from analysis.R by Rao: https://github.com/sraorao/animapp_desktop
# Need to read all the data again, as a list
setwd("../../111.1.1/cropped/")
file_names <- list.files(pattern = ".csv")
data_list_11111 <- lapply(file_names, fread)

setwd("../../132.1.1/cropped/")
file_names <- list.files(pattern = ".csv")
data_list_13211 <- lapply(file_names, fread)

setwd("../../7.1.1/cropped/")
file_names <- list.files(pattern = ".csv")
data_list_711 <- lapply(file_names, fread)

setwd("../../25.1.1/cropped/")
file_names <- list.files(pattern = ".csv")
data_list_2511 <- lapply(file_names, fread)

setwd("../../137.1.3/cropped/")
file_names <- list.files(pattern = ".csv")
data_list_13713 <- lapply(file_names, fread)

setwd("../../72.1.1/cropped/")
file_names <- list.files(pattern = ".csv")
data_list_7211 <- lapply(file_names, fread)
# Remove poorly tracked larva
data_list_7211 <- data_list_7211[1:28]

# normalise starting point to centre and convert pixels to mm
norm_11111 <- lapply(data_list_11111, function(x){
  startx = x$V1[1]
  starty = x$V2[1]
  x$V1 <- (x$V1 - startx) / 3.63
  x$V2 <- (x$V2 - starty) / 3.63
  colnames(x) <- c("x / mm", "y / mm", "frame", "V4", "V5", "V6", "V7", "V8")
  return(x)
})

norm_13211 <- lapply(data_list_13211, function(x){
  startx = x$V1[1]
  starty = x$V2[1]
  x$V1 <- (x$V1 - startx) / 3.63
  x$V2 <- (x$V2 - starty) / 3.63
  colnames(x) <- c("x / mm", "y / mm", "frame", "V4", "V5", "V6", "V7", "V8")
  return(x)
})

norm_711 <- lapply(data_list_711, function(x){
  startx = x$V1[1]
  starty = x$V2[1]
  x$V1 <- (x$V1 - startx) / 3.63
  x$V2 <- (x$V2 - starty) / 3.63
  colnames(x) <- c("x / mm", "y / mm", "frame", "V4", "V5", "V6", "V7", "V8")
  return(x)
})

norm_2511 <- lapply(data_list_2511, function(x){
  startx = x$V1[1]
  starty = x$V2[1]
  x$V1 <- (x$V1 - startx) / 3.63
  x$V2 <- (x$V2 - starty) / 3.63
  colnames(x) <- c("x / mm", "y / mm", "frame", "V4", "V5", "V6", "V7", "V8")
  return(x)
})

norm_13713 <- lapply(data_list_13713, function(x){
  startx = x$V1[1]
  starty = x$V2[1]
  x$V1 <- (x$V1 - startx) / 3.63
  x$V2 <- (x$V2 - starty) / 3.63
  colnames(x) <- c("x / mm", "y / mm", "frame", "V4", "V5", "V6", "V7", "V8")
  return(x)
})

norm_7211 <- lapply(data_list_7211, function(x){
  startx = x$V1[1]
  starty = x$V2[1]
  x$V1 <- (x$V1 - startx) / 3.63
  x$V2 <- (x$V2 - starty) / 3.63
  colnames(x) <- c("x / mm", "y / mm", "frame", "V4", "V5", "V6", "V7", "V8")
  return(x)
})

# combine data by genotype
norm_11111_combined <- do.call(rbind, lapply(1:length(norm_11111), 
                                             function(x) data.table(norm_11111[[x]])[, sample_name := paste0("norm_11111_", x)])) 

norm_13211_combined <- do.call(rbind, lapply(1:length(norm_13211), 
                                             function(x) data.table(norm_13211[[x]])[, sample_name := paste0("norm_13211_", x)])) 

norm_711_combined <- do.call(rbind, lapply(1:length(norm_711), 
                                             function(x) data.table(norm_711[[x]])[, sample_name := paste0("norm_711_", x)])) 

norm_2511_combined <- do.call(rbind, lapply(1:length(norm_2511), 
                                             function(x) data.table(norm_2511[[x]])[, sample_name := paste0("norm_2511_", x)])) 

norm_13713_combined <- do.call(rbind, lapply(1:length(norm_13713), 
                                             function(x) data.table(norm_13713[[x]])[, sample_name := paste0("norm_13713_", x)])) 

norm_7211_combined <- do.call(rbind, lapply(1:length(norm_7211), 
                                             function(x) data.table(norm_7211[[x]])[, sample_name := paste0("norm_7211_", x)])) 

combined <- list(norm_11111_combined, norm_13211_combined, norm_711_combined,
                 norm_2511_combined, norm_13713_combined, norm_7211_combined)

# normalised paths all in one plot
# only plot two genotypes (the one I sequenced), as above data shows
# that triplicates are identical in phenotype
do.call(rbind, combined) %>%
  mutate(genotype = gsub("[0-9]{1,2}_[0-9]{1,2}", "", sample_name)) %>% 
  filter(genotype == "norm_132" | genotype == "norm_25") %>%
  mutate(genotype = gsub("norm_132", "loxP", genotype)) %>%
  mutate(genotype = gsub("norm_25", "GEPD", genotype)) %>%
  ggplot(aes(x = `x / mm`, y = `y / mm`, colour = genotype)) + 
  scale_colour_manual(values = c("firebrick", "grey64")) +
  guides(colour = guide_legend(reverse = TRUE)) +
  geom_path(aes(colour = genotype, group = sample_name), alpha = 0.5, size = 1.5) + 
  theme_bw() +
  scale_x_continuous(limits = c(-60, 50)) + 
  scale_y_continuous(limits = c(-35, 45)) +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.text = element_text(size = 30),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(c(10, 10, 10, 10)),
        legend.title = element_blank(),
        legend.key.size = unit(2.0, 'cm')) -> p
p

# Plot 25.1.1 and 132.1.1 individually to count turns
larvae <- do.call(rbind, combined)
larvae_list <- split(larvae, larvae$sample_name)
# Blind yourself by sampling - set.seed() to reproduce
set.seed(1515); sample(larvae_list) -> larvae_list_random

ggplot(larvae_list_random[[14]], aes(x = `x / mm`, y = `y / mm`)) + 
  geom_path(aes(group = sample_name), alpha = 0.5, size = 1.5) + 
  theme_bw() +
  scale_x_continuous() + 
  scale_y_continuous() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.text = element_text(size = 30),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(c(10, 10, 10, 10)),
        legend.title = element_blank(),
        legend.key.size = unit(2.0, 'cm')) -> p_single
p_single

###############################################################################
############################# Larval dimensions ###############################
###############################################################################

# Expand data.tables to include larval length, width, and stops
norm_11111_combined[, larval_length := max(V6, V7) / 3.6, by = row.names(norm_11111_combined)]
norm_13211_combined[, larval_length := max(V6, V7) / 3.6, by = row.names(norm_13211_combined)]
norm_711_combined[, larval_length := max(V6, V7) / 3.6, by = row.names(norm_711_combined)]
norm_2511_combined[, larval_length := max(V6, V7) / 3.6, by = row.names(norm_2511_combined)]
norm_13713_combined[, larval_length := max(V6, V7) / 3.6, by = row.names(norm_13713_combined)]
norm_7211_combined[, larval_length := max(V6, V7) / 3.6, by = row.names(norm_7211_combined)]

norm_11111_combined[, larval_width := min(V6, V7) / 3.6, by = row.names(norm_11111_combined)]
norm_13211_combined[, larval_width := min(V6, V7) / 3.6, by = row.names(norm_13211_combined)]
norm_711_combined[, larval_width := min(V6, V7) / 3.6, by = row.names(norm_711_combined)]
norm_2511_combined[, larval_width := min(V6, V7) / 3.6, by = row.names(norm_2511_combined)]
norm_13713_combined[, larval_width := min(V6, V7) / 3.6, by = row.names(norm_13713_combined)]
norm_7211_combined[, larval_width := min(V6, V7) / 3.6, by = row.names(norm_7211_combined)]

norm_11111_combined[, larval_stop := larval_width/larval_length, by = row.names(norm_11111_combined)]
norm_13211_combined[, larval_stop := larval_width/larval_length, by = row.names(norm_13211_combined)]
norm_711_combined[, larval_stop := larval_width/larval_length, by = row.names(norm_711_combined)]
norm_2511_combined[, larval_stop := larval_width/larval_length, by = row.names(norm_2511_combined)]
norm_13713_combined[, larval_stop := larval_width/larval_length, by = row.names(norm_13713_combined)]
norm_7211_combined[, larval_stop := larval_width/larval_length, by = row.names(norm_7211_combined)]

norm_11111_combined[, time := frame / 15]
norm_13211_combined[, time := frame / 15]
norm_711_combined[, time := frame / 15]
norm_2511_combined[, time := frame / 15]
norm_13713_combined[, time := frame / 15]
norm_7211_combined[, time := frame / 15]

## Analyse larval anatomy
# Length
max_lengths_111 <- norm_11111_combined[, max(larval_length), by = sample_name]
max_lengths_132 <- norm_13211_combined[, max(larval_length), by = sample_name]
max_lengths_7 <- norm_711_combined[, max(larval_length), by = sample_name]
max_lengths_25 <- norm_2511_combined[, max(larval_length), by = sample_name]
max_lengths_137 <- norm_13713_combined[, max(larval_length), by = sample_name]
max_lengths_72 <- norm_7211_combined[, max(larval_length), by = sample_name]

colnames(max_lengths_111)[2] <- "max_length"
colnames(max_lengths_132)[2] <- "max_length"
colnames(max_lengths_7)[2] <- "max_length"
colnames(max_lengths_25)[2] <- "max_length"
colnames(max_lengths_137)[2] <- "max_length"
colnames(max_lengths_72)[2] <- "max_length"

min_lengths_111 <- norm_11111_combined[, min(larval_length), by = sample_name]
min_lengths_132 <- norm_13211_combined[, min(larval_length), by = sample_name]
min_lengths_7 <- norm_711_combined[, min(larval_length), by = sample_name]
min_lengths_25 <- norm_2511_combined[, min(larval_length), by = sample_name]
min_lengths_137 <- norm_13713_combined[, min(larval_length), by = sample_name]
min_lengths_72 <- norm_7211_combined[, min(larval_length), by = sample_name]

colnames(min_lengths_111)[2] <- "min_length"
colnames(min_lengths_132)[2] <- "min_length"
colnames(min_lengths_7)[2] <- "min_length"
colnames(min_lengths_25)[2] <- "min_length"
colnames(min_lengths_137)[2] <- "min_length"
colnames(min_lengths_72)[2] <- "min_length"

mean_lengths_111 <- norm_11111_combined[, mean(larval_length), by = sample_name]
mean_lengths_132 <- norm_13211_combined[, mean(larval_length), by = sample_name]
mean_lengths_7 <- norm_711_combined[, mean(larval_length), by = sample_name]
mean_lengths_25 <- norm_2511_combined[, mean(larval_length), by = sample_name]
mean_lengths_137 <- norm_13713_combined[, mean(larval_length), by = sample_name]
mean_lengths_72 <- norm_7211_combined[, mean(larval_length), by = sample_name]

colnames(mean_lengths_111)[2] <- "mean_length"
colnames(mean_lengths_132)[2] <- "mean_length"
colnames(mean_lengths_7)[2] <- "mean_length"
colnames(mean_lengths_25)[2] <- "mean_length"
colnames(mean_lengths_137)[2] <- "mean_length"
colnames(mean_lengths_72)[2] <- "mean_length"

max_lengths <- rbind(max_lengths_111, max_lengths_132, max_lengths_7,
                     max_lengths_25, max_lengths_137, max_lengths_72)

min_lengths <- rbind(min_lengths_111, min_lengths_132, min_lengths_7,
                     min_lengths_25, min_lengths_137, min_lengths_72)

mean_lengths <- rbind(mean_lengths_111, mean_lengths_132, mean_lengths_7,
                     mean_lengths_25, mean_lengths_137, mean_lengths_72)

max_lengths[, genotype := gsub("[0-9]{1,2}_[0-9]{1,2}", "", sample_name)]
max_lengths$genotype <- factor(max_lengths$genotype, levels = c("norm_132", "norm_111", "norm_7",
                                                          "norm_25", "norm_137", "norm_72"))

min_lengths[, genotype := gsub("[0-9]{1,2}_[0-9]{1,2}", "", sample_name)]
min_lengths$genotype <- factor(min_lengths$genotype, levels = c("norm_132", "norm_111", "norm_7",
                                                          "norm_25", "norm_137", "norm_72"))

mean_lengths[, genotype := gsub("[0-9]{1,2}_[0-9]{1,2}", "", sample_name)]
mean_lengths$genotype <- factor(mean_lengths$genotype, levels = c("norm_132", "norm_111", "norm_7",
                                                                "norm_25", "norm_137", "norm_72"))

max_lengths$Genotype <- factor(ifelse(max_lengths$genotype == "norm_132", "loxP",
                                   ifelse(max_lengths$genotype == "norm_111", "loxP",
                                          ifelse(max_lengths$genotype == "norm_7", "loxP",
                                                 ifelse(max_lengths$genotype == "norm_25", "GEPD",
                                                        ifelse(max_lengths$genotype == "norm_137", "GEPD",
                                                               "GEPD"))))), levels = c("loxP", "GEPD"))
min_lengths$Genotype <- factor(ifelse(min_lengths$genotype == "norm_132", "loxP",
                                   ifelse(min_lengths$genotype == "norm_111", "loxP",
                                          ifelse(min_lengths$genotype == "norm_7", "loxP",
                                                 ifelse(min_lengths$genotype == "norm_25", "GEPD",
                                                        ifelse(min_lengths$genotype == "norm_137", "GEPD",
                                                               "GEPD"))))), levels = c("loxP", "GEPD"))
mean_lengths$Genotype <- factor(ifelse(mean_lengths$genotype == "norm_132", "loxP",
                                      ifelse(mean_lengths$genotype == "norm_111", "loxP",
                                             ifelse(mean_lengths$genotype == "norm_7", "loxP",
                                                    ifelse(mean_lengths$genotype == "norm_25", "GEPD",
                                                           ifelse(mean_lengths$genotype == "norm_137", "GEPD",
                                                                  "GEPD"))))), levels = c("loxP", "GEPD"))


gg_max_lengths <- ggplot(max_lengths, aes(x = genotype, y = max_length)) + 
  geom_boxplot(aes(fill = Genotype), outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(position = position_jitter(0.2), na.rm = TRUE, shape = 4, size = 6) +
  scale_shape_discrete(solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Max. Length / mm") +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.text = element_text(size = 30),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(c(10, 10, 10, 10)),
        legend.title = element_blank(),
        legend.key.size = unit(2.0, 'cm'))
gg_max_lengths

gg_min_lengths <- ggplot(min_lengths, aes(x = genotype, y = min_length)) + 
  geom_boxplot(aes(fill = Genotype), outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(position = position_jitter(0.2), na.rm = TRUE, shape = 4, size = 6) +
  scale_shape_discrete(solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Max. Length / mm") +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.text = element_text(size = 30),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(c(10, 10, 10, 10)),
        legend.title = element_blank(),
        legend.key.size = unit(2.0, 'cm'))
gg_min_lengths

gg_mean_lengths <- ggplot(mean_lengths, aes(x = genotype, y = mean_length)) + 
  geom_boxplot(aes(fill = Genotype), outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(position = position_jitter(0.2), na.rm = TRUE, shape = 4, size = 6) +
  scale_shape_discrete(solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Mean Length / mm", limits = c(3, 6)) +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.text = element_text(size = 30),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(c(10, 10, 10, 10)),
        legend.title = element_blank(),
        legend.key.size = unit(2.0, 'cm'))
gg_mean_lengths

# Stats
kruskal.test(mean_lengths$mean_length ~ mean_lengths$genotype, data = mean_lengths)
dunnTest(mean_lengths$mean_length ~ mean_lengths$genotype, data = mean_lengths)

max_lengths <- mean_lengths[, max(mean_length, na.rm = TRUE), by = Genotype]
max_loxp <- max_lengths[1,2]
max_loxp <- as.numeric(max_loxp)
max_loxp <- max_loxp + 0.2

max_gepd <- max_lengths[2,2]
max_gepd <- as.numeric(max_gepd)
max_gepd <- max_gepd + 0.2

# Draw error bars for gg_mean_lengths
sign_loxp <- data.frame(genotype = c("norm_111", "norm_132", "norm_7"), mean_length = rep(max_loxp, 3))
sign_gepd <- data.frame(genotype = c("norm_25", "norm_137", "norm_72"), mean_length = rep(max_gepd, 3))

gg_mean_lengths2 <- gg_mean_lengths + geom_line(data = sign_loxp, aes(x = genotype, y = mean_length, group = 1))

gg_mean_lengths3 <- gg_mean_lengths2 + geom_line(data = sign_gepd, aes(x = genotype, y = mean_length, group = 1))

# Draw error bar for betwen-group stats
sign_combined <- data.frame(genotype = c("norm_111", "norm_137"), mean_length = rep(max_gepd + 0.3, 2))
gg_mean_lengths4 <- gg_mean_lengths3 + geom_line(data = sign_combined, aes(x = genotype, y = mean_length, group = 1)) +
  annotate("text", x = 3.5, y = max_gepd + 0.4, label = "*   ****", size = 10) +
  annotate("text", x = 2, y = max_loxp + 0.1, label = "ns", size = 10) +
  annotate("text", x = 5, y = max_gepd + 0.1, label = "**", size = 10)
gg_mean_lengths4


# Analyse width
max_widths_111 <- norm_11111_combined[, max(larval_width), by = sample_name]
max_widths_132 <- norm_13211_combined[, max(larval_width), by = sample_name]
max_widths_7 <- norm_711_combined[, max(larval_width), by = sample_name]
max_widths_25 <- norm_2511_combined[, max(larval_width), by = sample_name]
max_widths_137 <- norm_13713_combined[, max(larval_width), by = sample_name]
max_widths_72 <- norm_7211_combined[, max(larval_width), by = sample_name]

colnames(max_widths_111)[2] <- "max_width"
colnames(max_widths_132)[2] <- "max_width"
colnames(max_widths_7)[2] <- "max_width"
colnames(max_widths_25)[2] <- "max_width"
colnames(max_widths_137)[2] <- "max_width"
colnames(max_widths_72)[2] <- "max_width"

min_widths_111 <- norm_11111_combined[, min(larval_width), by = sample_name]
min_widths_132 <- norm_13211_combined[, min(larval_width), by = sample_name]
min_widths_7 <- norm_711_combined[, min(larval_width), by = sample_name]
min_widths_25 <- norm_2511_combined[, min(larval_width), by = sample_name]
min_widths_137 <- norm_13713_combined[, min(larval_width), by = sample_name]
min_widths_72 <- norm_7211_combined[, min(larval_width), by = sample_name]

colnames(min_widths_111)[2] <- "min_width"
colnames(min_widths_132)[2] <- "min_width"
colnames(min_widths_7)[2] <- "min_width"
colnames(min_widths_25)[2] <- "min_width"
colnames(min_widths_137)[2] <- "min_width"
colnames(min_widths_72)[2] <- "min_width"

mean_widths_111 <- norm_11111_combined[, mean(larval_width), by = sample_name]
mean_widths_132 <- norm_13211_combined[, mean(larval_width), by = sample_name]
mean_widths_7 <- norm_711_combined[, mean(larval_width), by = sample_name]
mean_widths_25 <- norm_2511_combined[, mean(larval_width), by = sample_name]
mean_widths_137 <- norm_13713_combined[, mean(larval_width), by = sample_name]
mean_widths_72 <- norm_7211_combined[, mean(larval_width), by = sample_name]

colnames(mean_widths_111)[2] <- "mean_width"
colnames(mean_widths_132)[2] <- "mean_width"
colnames(mean_widths_7)[2] <- "mean_width"
colnames(mean_widths_25)[2] <- "mean_width"
colnames(mean_widths_137)[2] <- "mean_width"
colnames(mean_widths_72)[2] <- "mean_width"

max_widths <- rbind(max_widths_111, max_widths_132, max_widths_7,
                     max_widths_25, max_widths_137, max_widths_72)

min_widths <- rbind(min_widths_111, min_widths_132, min_widths_7,
                     min_widths_25, min_widths_137, min_widths_72)

mean_widths <- rbind(mean_widths_111, mean_widths_132, mean_widths_7,
                      mean_widths_25, mean_widths_137, mean_widths_72)

max_widths[, genotype := gsub("[0-9]{1,2}_[0-9]{1,2}", "", sample_name)]
max_widths$genotype <- factor(max_widths$genotype, levels = c("norm_132", "norm_111", "norm_7",
                                                                "norm_25", "norm_137", "norm_72"))

min_widths[, genotype := gsub("[0-9]{1,2}_[0-9]{1,2}", "", sample_name)]
min_widths$genotype <- factor(min_widths$genotype, levels = c("norm_132", "norm_111", "norm_7",
                                                                "norm_25", "norm_137", "norm_72"))

mean_widths[, genotype := gsub("[0-9]{1,2}_[0-9]{1,2}", "", sample_name)]
mean_widths$genotype <- factor(mean_widths$genotype, levels = c("norm_132", "norm_111", "norm_7",
                                                                  "norm_25", "norm_137", "norm_72"))

max_widths$Genotype <- factor(ifelse(max_widths$genotype == "norm_132", "loxP",
                                      ifelse(max_widths$genotype == "norm_111", "loxP",
                                             ifelse(max_widths$genotype == "norm_7", "loxP",
                                                    ifelse(max_widths$genotype == "norm_25", "GEPD",
                                                           ifelse(max_widths$genotype == "norm_137", "GEPD",
                                                                  "GEPD"))))), levels = c("loxP", "GEPD"))
min_widths$Genotype <- factor(ifelse(min_widths$genotype == "norm_132", "loxP",
                                      ifelse(min_widths$genotype == "norm_111", "loxP",
                                             ifelse(min_widths$genotype == "norm_7", "loxP",
                                                    ifelse(min_widths$genotype == "norm_25", "GEPD",
                                                           ifelse(min_widths$genotype == "norm_137", "GEPD",
                                                                  "GEPD"))))), levels = c("loxP", "GEPD"))
mean_widths$Genotype <- factor(ifelse(mean_widths$genotype == "norm_132", "loxP",
                                       ifelse(mean_widths$genotype == "norm_111", "loxP",
                                              ifelse(mean_widths$genotype == "norm_7", "loxP",
                                                     ifelse(mean_widths$genotype == "norm_25", "GEPD",
                                                            ifelse(mean_widths$genotype == "norm_137", "GEPD",
                                                                   "GEPD"))))), levels = c("loxP", "GEPD"))


gg_max_widths <- ggplot(max_widths, aes(x = genotype, y = max_width)) + 
  geom_boxplot(aes(fill = Genotype), outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(position = position_jitter(0.2), na.rm = TRUE, shape = 4, size = 6) +
  scale_shape_discrete(solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Max. width / mm") +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.text = element_text(size = 30),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(c(10, 10, 10, 10)),
        legend.title = element_blank(),
        legend.key.size = unit(2.0, 'cm'))
gg_max_widths

gg_min_widths <- ggplot(min_widths, aes(x = genotype, y = min_width)) + 
  geom_boxplot(aes(fill = Genotype), outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(position = position_jitter(0.2), na.rm = TRUE, shape = 4, size = 6) +
  scale_shape_discrete(solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Max. width / mm") +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.text = element_text(size = 30),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(c(10, 10, 10, 10)),
        legend.title = element_blank(),
        legend.key.size = unit(2.0, 'cm'))
gg_min_widths

gg_mean_widths <- ggplot(mean_widths, aes(x = genotype, y = mean_width)) + 
  geom_boxplot(aes(fill = Genotype), outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(position = position_jitter(0.2), na.rm = TRUE, shape = 4, size = 6) +
  scale_shape_discrete(solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Mean Width / mm", limits = c(0.5, 2)) +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.text = element_text(size = 30),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(c(10, 10, 10, 10)),
        legend.title = element_blank(),
        legend.key.size = unit(2.0, 'cm'))
gg_mean_widths

# Stats
kruskal.test(mean_widths$mean_width ~ mean_widths$genotype, data = mean_widths)
dunnTest(mean_widths$mean_width ~ mean_widths$genotype, data = mean_widths)

max_widths <- mean_widths[, max(mean_width, na.rm = TRUE), by = Genotype]
max_loxp <- max_widths[1,2]
max_loxp <- as.numeric(max_loxp)
max_loxp <- max_loxp + 0.1

max_gepd <- max_widths[2,2]
max_gepd <- as.numeric(max_gepd)
max_gepd <- max_gepd + 0.1

# Draw error bars for gg_mean_widths
sign_loxp <- data.frame(genotype = c("norm_111", "norm_132", "norm_7"), mean_width = rep(max_loxp, 3))
sign_gepd <- data.frame(genotype = c("norm_25", "norm_137", "norm_72"), mean_width = rep(max_gepd, 3))

gg_mean_widths2 <- gg_mean_widths + geom_line(data = sign_loxp, aes(x = genotype, y = mean_width, group = 1))

gg_mean_widths3 <- gg_mean_widths2 + geom_line(data = sign_gepd, aes(x = genotype, y = mean_width, group = 1))

# Draw error bar for betwen-group stats
sign_combined <- data.frame(genotype = c("norm_111", "norm_137"), mean_width = rep(max_gepd + 0.2, 2))
gg_mean_widths4 <- gg_mean_widths3 + geom_line(data = sign_combined, aes(x = genotype, y = mean_width, group = 1)) +
  annotate("text", x = 3.5, y = max_gepd + 0.275, label = "ns", size = 10) +
  annotate("text", x = 2, y = max_loxp + 0.075, label = "ns", size = 10) +
  annotate("text", x = 5, y = max_gepd + 0.075, label = "ns", size = 10)
gg_mean_widths4


###############################################################################
############################## Larval movement ################################
###############################################################################

# Plot individual larval length
ggplot(norm_13211_combined[sample_name == "norm_13211_5", ], aes(x = time, y = larval_length)) + 
  geom_line(color = "grey64", size = 0.75) +
  scale_y_continuous(limits = c(2.5, 6), name = "Larval Length / mm") +
  scale_x_continuous(name = "Time / s") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50))

ggplot(norm_2511_combined[sample_name == "norm_2511_19", ], aes(x = time, y = larval_length)) + 
  geom_line(color = "firebrick", size = 0.75) +
  scale_y_continuous(limits = c(2.5, 6), name = "Larval Length / mm") +
  scale_x_continuous(name = "Time / s") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50))


# Plot individual larval turns
ggplot(norm_2511_combined[sample_name == "norm_2511_10", ], aes(x = time, y = larval_stop)) + 
  geom_line(color = "black") +
  scale_y_continuous(limits = c(0, 0.75), name = "Width / Length") +
  scale_x_continuous(name = "Time / s") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50))

ggplot(norm_13211_combined[sample_name == "norm_13211_2", ], aes(x = time, y = larval_stop)) + 
  geom_line(color = "black") +
  scale_y_continuous(limits = c(0, 0.75), name = "Width / Length") +
  scale_x_continuous(name = "Time / s") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50))


## The code below is from Rao's dual_plot script, available on 
# https://github.com/sraorao/animapp_desktop
# Plot length and stops of larvae
draw_dual_plot <- function(filename, save = FALSE) {
  
  fread(filename) %>%
    rowwise() %>%
    mutate(larval_length = max(V6, V7)) %>%
    mutate(larval_width = min(V6, V7)) %>%
    mutate(larval_stop = larval_width/larval_length) -> df
  setDT(df)
  df[, time := V3 / 15]
  
  df %>%
    ggplot(aes(x = time, y = larval_length)) + 
    geom_line(color = "salmon") +
    scale_y_continuous(limits = c(10, 20)) +
    theme_bw() +
    theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
          text = element_text(size = 50)) -> p1
  
  df %>%
    ggplot(aes(x = time, y = larval_stop)) + 
    geom_line() +
    scale_y_continuous(limits = c(0, 0.75)) +
    theme_bw() +
    theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
          text = element_text(size = 50)) -> p2
  
  if (save) {
    save_plot(paste0(filename, ".pdf"), p, base_height = 4, base_width = 8)
  }
  return(list(p1, p2))
}

# Plot individual traces
stretch <- draw_dual_plot("../../72.1.1/cropped/tracked_VID_20180515_154852.mp4_nosound.mp4_cropped.mp4.csv")
stretch[2]

# Define a turn by passing the threshold of width/length = 0.4 
# Randomise flies first to be blinded
larvae <- rbind(norm_11111_combined, norm_13211_combined, norm_711_combined,
                norm_2511_combined, norm_13713_combined, norm_7211_combined)
larvae_list <- split(larvae, larvae$sample_name)
# Blind yourself by sampling - set.seed() to reproduce
set.seed(1515); sample(larvae_list) -> larvae_list_random
# Plot individual larval turns
ggplot(larvae_list_random[[163]], aes(x = time, y = larval_stop)) + 
  geom_line(color = "black") +
  scale_y_continuous(limits = c(0, 0.75), name = "Width / Length") +
  scale_x_continuous(name = "Time / s") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50))

# After manual analysis of turns and unblinding, load .csv file back into R
turns <- read.csv("../../../analysis/turn_analysis.csv")
turns <- turns[, 1:3]
setDT(turns)
turns[, genotype := gsub("_[0-9]{1,2}$", "", sample_name)]
turns[, genotype := gsub("norm_", "", genotype)]
turns$genotype <- factor(turns$genotype, levels = c("13211", "11111", "711",
                                                       "2511", "13713", "7211"))
turns$Genotype <- factor(ifelse(turns$genotype == "13211", "loxP",
                                                      ifelse(turns$genotype == "11111", "loxP",
                                                             ifelse(turns$genotype == "711", "loxP",
                                                                    ifelse(turns$genotype == "2511", "GEPD",
                                                                           ifelse(turns$genotype == "13713", "GEPD",
                                                                                  "GEPD"))))), levels = c("loxP", "GEPD"))

# Plot turns per genotype
gg <- ggplot(turns, aes(x = genotype, y = threshold_crosses)) + 
  geom_boxplot(aes(fill = Genotype), outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(position = position_jitter(0.2), na.rm = TRUE, shape = 4, size = 6) +
  scale_shape_discrete(solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Turns / min") +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.text = element_text(size = 30),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin = margin(c(10, 10, 10, 10)),
        legend.title = element_blank(),
        legend.key.size = unit(2.0, 'cm'))
gg

# Stats
# Stats on individual replicates
kruskal.test(turns$threshold_crosses ~ turns$genotype, data = turns)
dunnTest(turns$threshold_crosses ~ turns$genotype, data = turns)

# Draw error bars
setDT(turns)

max_turns <- turns[, max(threshold_crosses), by = Genotype]
max_loxp <- max_turns[1,2]
max_loxp <- as.numeric(max_loxp)
max_loxp <- max_loxp + 0.5

max_gepd <- max_turns[2,2]
max_gepd <- as.numeric(max_gepd)
max_gepd <- max_gepd + 0.5

# Draw error bars for gg
sign_loxp <- data.frame(genotype = c("13211", "11111", "711"), threshold_crosses = rep(max_loxp, 3))
sign_gepd <- data.frame(genotype = c("2511", "13713", "7211"), threshold_crosses = rep(max_gepd, 3))

gg2 <- gg + geom_line(data = sign_loxp, aes(x = genotype, y = threshold_crosses, group = 1)) +
  annotate("text", x = "11111", y = max_loxp + 1, label = "ns", size = 10)

gg3 <- gg2 + geom_line(data = sign_gepd, aes(x = genotype, y = threshold_crosses, group = 1)) +
  annotate("text", x = "13713", y = max_gepd + 1, label = "ns", size = 10)

# Draw error bar for betwen-group stats
sign_combined <- data.frame(genotype = c("11111", "13713"), threshold_crosses = rep(max_loxp + 2, 2))
gg4 <- gg3 + geom_line(data = sign_combined, aes(x = genotype, y = threshold_crosses, group = 1)) +
  annotate("text", x = 3.5, y = max_loxp + 2.5, label = "        ****", size = 10)
gg4

# Plot example turns for figure
ggplot(norm_2511_combined[sample_name == "norm_2511_10", ], aes(x = time, y = larval_stop)) + 
  geom_line(color = "firebrick", size = 0.75) +
  scale_y_continuous(limits = c(0, 0.75), name = "Width / Length") +
  scale_x_continuous(name = "Time / s") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50)) +
  geom_hline(aes(yintercept = 0.4), linetype = 2)

ggplot(norm_13211_combined[sample_name == "norm_13211_22", ], aes(x = time, y = larval_stop)) + 
  geom_line(color = "grey64", size = 0.75) +
  scale_y_continuous(limits = c(0, 0.75), name = "Width / Length") +
  scale_x_continuous(name = "Time / s") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50)) +
  geom_hline(aes(yintercept = 0.4), linetype = 2)

# Plot total turns with pooled genotypes


###############################################################################
#################################  END  #######################################
###############################################################################

sessionInfo()










