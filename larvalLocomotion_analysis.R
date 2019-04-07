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
setwd("../../videos/111.1.1/cropped/")
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
################################## 72.1.1/+ ###################################
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
# Also add axial ratio to get the mean AR
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

norm_11111_combined[, AR := larval_length/larval_width, by = row.names(norm_11111_combined)]
norm_13211_combined[, AR := larval_length/larval_width, by = row.names(norm_13211_combined)]
norm_711_combined[, AR := larval_length/larval_width, by = row.names(norm_711_combined)]
norm_2511_combined[, AR := larval_length/larval_width, by = row.names(norm_2511_combined)]
norm_13713_combined[, AR := larval_length/larval_width, by = row.names(norm_13713_combined)]
norm_7211_combined[, AR := larval_length/larval_width, by = row.names(norm_7211_combined)]

# Get time in seconds (15 fps recordings)
norm_11111_combined[, time := frame / 15]
norm_13211_combined[, time := frame / 15]
norm_711_combined[, time := frame / 15]
norm_2511_combined[, time := frame / 15]
norm_13713_combined[, time := frame / 15]
norm_7211_combined[, time := frame / 15]
###############################################################################
########################### Analyse larval anatomy ############################
###############################################################################

###############################################################################
################################## Length #####################################
###############################################################################

# Check whether length is normally distributed
ggplot(norm_11111_combined, aes(x = larval_length, fill = sample_name)) +
  geom_histogram()

ggplot(norm_13211_combined, aes(x = larval_length, fill = sample_name)) +
  geom_histogram()

ggplot(norm_711_combined, aes(x = larval_length, fill = sample_name)) +
  geom_histogram()

ggplot(norm_2511_combined, aes(x = larval_length, fill = sample_name)) +
  geom_histogram()

ggplot(norm_13713_combined, aes(x = larval_length, fill = sample_name)) +
  geom_histogram()

ggplot(norm_7211_combined, aes(x = larval_length, fill = sample_name)) +
  geom_histogram()


median_lengths_111 <- norm_11111_combined[, median(larval_length), by = sample_name]
median_lengths_132 <- norm_13211_combined[, median(larval_length), by = sample_name]
median_lengths_7 <- norm_711_combined[, median(larval_length), by = sample_name]
median_lengths_25 <- norm_2511_combined[, median(larval_length), by = sample_name]
median_lengths_137 <- norm_13713_combined[, median(larval_length), by = sample_name]
median_lengths_72 <- norm_7211_combined[, median(larval_length), by = sample_name]

colnames(median_lengths_111)[2] <- "median_length"
colnames(median_lengths_132)[2] <- "median_length"
colnames(median_lengths_7)[2] <- "median_length"
colnames(median_lengths_25)[2] <- "median_length"
colnames(median_lengths_137)[2] <- "median_length"
colnames(median_lengths_72)[2] <- "median_length"

median_lengths <- rbind(median_lengths_111, median_lengths_132, median_lengths_7,
                     median_lengths_25, median_lengths_137, median_lengths_72)

median_lengths[, genotype := gsub("[0-9]{1,2}_[0-9]{1,2}", "", sample_name)]
median_lengths$genotype <- factor(median_lengths$genotype, levels = c("norm_132", "norm_111", "norm_7",
                                                                "norm_25", "norm_137", "norm_72"))

median_lengths$Genotype <- factor(ifelse(median_lengths$genotype == "norm_132", "loxP",
                                      ifelse(median_lengths$genotype == "norm_111", "loxP",
                                             ifelse(median_lengths$genotype == "norm_7", "loxP",
                                                    ifelse(median_lengths$genotype == "norm_25", "GEPD",
                                                           ifelse(median_lengths$genotype == "norm_137", "GEPD",
                                                                  "GEPD"))))), levels = c("loxP", "GEPD"))

# Get median lengths of pooled data
gg_median_lengths_pooled <- ggplot(median_lengths, aes(x = Genotype, y = median_length)) + 
  geom_boxplot(aes(fill = Genotype), outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(position = position_jitter(0.2), na.rm = TRUE, shape = 4, size = 6) +
  scale_shape_discrete(solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Median Length / mm", limits = c(0, 6)) +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 40, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.position = "none")
gg_median_lengths_pooled

# Stats
wilcox.test(median_length ~ Genotype, data = median_lengths)

# Error bars
max_length <- max(median_lengths$median_length) + 0.25
sign_max_length <- data.frame(Genotype = c(1, 2), median_length = rep(max_length, 2))
gg_median_lengths_pooled2 <- gg_median_lengths_pooled + geom_line(data = sign_max_length, aes(x = Genotype, y = median_length, group = 1)) +
  annotate("text", x = 1.5, y = max_length + 0.25, label = "****", size = 10)

gg_median_lengths_pooled2

###############################################################################
# Analyse width
###############################################################################

# Check whether width is normally distributed
ggplot(norm_11111_combined, aes(x = larval_width, fill = sample_name)) +
  geom_histogram()

ggplot(norm_13211_combined, aes(x = larval_width, fill = sample_name)) +
  geom_histogram()

ggplot(norm_711_combined, aes(x = larval_width, fill = sample_name)) +
  geom_histogram()

ggplot(norm_2511_combined, aes(x = larval_width, fill = sample_name)) +
  geom_histogram()

ggplot(norm_13713_combined, aes(x = larval_width, fill = sample_name)) +
  geom_histogram()

ggplot(norm_7211_combined, aes(x = larval_width, fill = sample_name)) +
  geom_histogram()


median_widths_111 <- norm_11111_combined[, median(larval_width), by = sample_name]
median_widths_132 <- norm_13211_combined[, median(larval_width), by = sample_name]
median_widths_7 <- norm_711_combined[, median(larval_width), by = sample_name]
median_widths_25 <- norm_2511_combined[, median(larval_width), by = sample_name]
median_widths_137 <- norm_13713_combined[, median(larval_width), by = sample_name]
median_widths_72 <- norm_7211_combined[, median(larval_width), by = sample_name]

colnames(median_widths_111)[2] <- "median_width"
colnames(median_widths_132)[2] <- "median_width"
colnames(median_widths_7)[2] <- "median_width"
colnames(median_widths_25)[2] <- "median_width"
colnames(median_widths_137)[2] <- "median_width"
colnames(median_widths_72)[2] <- "median_width"

median_widths <- rbind(median_widths_111, median_widths_132, median_widths_7,
                      median_widths_25, median_widths_137, median_widths_72)

median_widths[, genotype := gsub("[0-9]{1,2}_[0-9]{1,2}", "", sample_name)]
median_widths$genotype <- factor(median_widths$genotype, levels = c("norm_132", "norm_111", "norm_7",
                                                                  "norm_25", "norm_137", "norm_72"))

median_widths$Genotype <- factor(ifelse(median_widths$genotype == "norm_132", "loxP",
                                       ifelse(median_widths$genotype == "norm_111", "loxP",
                                              ifelse(median_widths$genotype == "norm_7", "loxP",
                                                     ifelse(median_widths$genotype == "norm_25", "GEPD",
                                                            ifelse(median_widths$genotype == "norm_137", "GEPD",
                                                                   "GEPD"))))), levels = c("loxP", "GEPD"))
# median width of pooled data
gg_median_widths_pooled <- ggplot(median_widths, aes(x = Genotype, y = median_width)) +
  geom_boxplot(aes(fill = Genotype), outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(position = position_jitter(0.2), na.rm = TRUE, shape = 4, size = 6) +
  scale_shape_discrete(solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Median Width / mm", limits = c(0, 2)) +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 40, margin = margin(t = 0, r = 30, b = 0, l = 0)),
        text = element_text(size = 50),
        legend.position = "none")
gg_median_widths_pooled

# Stats
wilcox.test(median_width ~ Genotype, data = median_widths)

# Error bars
max_width <- max(median_widths$median_width) + 0.2
sign_max_width <- data.frame(Genotype = c(1, 2), median_width = rep(max_width, 2))
gg_median_widths_pooled2 <- gg_median_widths_pooled + geom_line(data = sign_max_width, aes(x = Genotype, y = median_width, group = 1)) +
  annotate("text", x = 1.5, y = max_width + 0.2, label = "p = 0.07", size = 10)

gg_median_widths_pooled2

################################################################################
################################## Analyse AR ##################################
################################################################################

# Check whether AR is normally distributed
ggplot(norm_11111_combined, aes(x = AR, fill = sample_name)) +
  geom_histogram()

ggplot(norm_13211_combined, aes(x = AR, fill = sample_name)) +
  geom_histogram()

ggplot(norm_711_combined, aes(x = AR, fill = sample_name)) +
  geom_histogram()

ggplot(norm_2511_combined, aes(x = AR, fill = sample_name)) +
  geom_histogram()

ggplot(norm_13713_combined, aes(x = AR, fill = sample_name)) +
  geom_histogram()

ggplot(norm_7211_combined, aes(x = AR, fill = sample_name)) +
  geom_histogram()


median_AR_111 <- norm_11111_combined[, median(AR), by = sample_name]
median_AR_132 <- norm_13211_combined[, median(AR), by = sample_name]
median_AR_7 <- norm_711_combined[, median(AR), by = sample_name]
median_AR_25 <- norm_2511_combined[, median(AR), by = sample_name]
median_AR_137 <- norm_13713_combined[, median(AR), by = sample_name]
median_AR_72 <- norm_7211_combined[, median(AR), by = sample_name]

colnames(median_AR_111)[2] <- "median_AR"
colnames(median_AR_132)[2] <- "median_AR"
colnames(median_AR_7)[2] <- "median_AR"
colnames(median_AR_25)[2] <- "median_AR"
colnames(median_AR_137)[2] <- "median_AR"
colnames(median_AR_72)[2] <- "median_AR"

median_AR <- rbind(median_AR_111, median_AR_132, median_AR_7,
                     median_AR_25, median_AR_137, median_AR_72)

median_AR[, genotype := gsub("[0-9]{1,2}_[0-9]{1,2}", "", sample_name)]
median_AR$genotype <- factor(median_AR$genotype, levels = c("norm_132", "norm_111", "norm_7",
                                                                "norm_25", "norm_137", "norm_72"))

median_AR$Genotype <- factor(ifelse(median_AR$genotype == "norm_132", "loxP",
                                      ifelse(median_AR$genotype == "norm_111", "loxP",
                                             ifelse(median_AR$genotype == "norm_7", "loxP",
                                                    ifelse(median_AR$genotype == "norm_25", "GEPD",
                                                           ifelse(median_AR$genotype == "norm_137", "GEPD",
                                                                  "GEPD"))))), levels = c("loxP", "GEPD"))
# median width of pooled data
gg_median_AR_pooled <- ggplot(median_AR, aes(x = Genotype, y = median_AR)) +
  geom_boxplot(aes(fill = Genotype), outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(position = position_jitter(0.2), na.rm = TRUE, shape = 4, size = 6) +
  scale_shape_discrete(solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Axial Ratio") +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)),
        text = element_text(size = 50),
        legend.position = "none")
gg_median_AR_pooled

# Stats
wilcox.test(median_AR ~ Genotype, data = median_AR)

# Error bars
max_AR <- max(median_AR$median_AR) + 0.1
sign_max_AR <- data.frame(Genotype = c(1, 2), median_AR = rep(max_AR, 2))
gg_median_AR_pooled2 <- gg_median_AR_pooled + geom_line(data = sign_max_AR, aes(x = Genotype, y = median_AR, group = 1)) +
  annotate("text", x = 1.5, y = max_AR + 0.1, label = "**", size = 10)

gg_median_AR_pooled2

################################################################################
############################# Analyse inverse AR ###############################
################################################################################

# Check whether AR is normally distributed
ggplot(norm_11111_combined, aes(x = larval_stop, fill = sample_name)) +
  geom_histogram()

ggplot(norm_13211_combined, aes(x = larval_stop, fill = sample_name)) +
  geom_histogram()

ggplot(norm_711_combined, aes(x = larval_stop, fill = sample_name)) +
  geom_histogram()

ggplot(norm_2511_combined, aes(x = larval_stop, fill = sample_name)) +
  geom_histogram()

ggplot(norm_13713_combined, aes(x = larval_stop, fill = sample_name)) +
  geom_histogram()

ggplot(norm_7211_combined, aes(x = larval_stop, fill = sample_name)) +
  geom_histogram()


median_larval_stop_111 <- norm_11111_combined[, median(larval_stop), by = sample_name]
median_larval_stop_132 <- norm_13211_combined[, median(larval_stop), by = sample_name]
median_larval_stop_7 <- norm_711_combined[, median(larval_stop), by = sample_name]
median_larval_stop_25 <- norm_2511_combined[, median(larval_stop), by = sample_name]
median_larval_stop_137 <- norm_13713_combined[, median(larval_stop), by = sample_name]
median_larval_stop_72 <- norm_7211_combined[, median(larval_stop), by = sample_name]

colnames(median_larval_stop_111)[2] <- "median_larval_stop"
colnames(median_larval_stop_132)[2] <- "median_larval_stop"
colnames(median_larval_stop_7)[2] <- "median_larval_stop"
colnames(median_larval_stop_25)[2] <- "median_larval_stop"
colnames(median_larval_stop_137)[2] <- "median_larval_stop"
colnames(median_larval_stop_72)[2] <- "median_larval_stop"

median_larval_stop <- rbind(median_larval_stop_111, median_larval_stop_132, median_larval_stop_7,
                   median_larval_stop_25, median_larval_stop_137, median_larval_stop_72)

median_larval_stop[, genotype := gsub("[0-9]{1,2}_[0-9]{1,2}", "", sample_name)]
median_larval_stop$genotype <- factor(median_larval_stop$genotype, levels = c("norm_132", "norm_111", "norm_7",
                                                            "norm_25", "norm_137", "norm_72"))

median_larval_stop$Genotype <- factor(ifelse(median_larval_stop$genotype == "norm_132", "loxP",
                                    ifelse(median_larval_stop$genotype == "norm_111", "loxP",
                                           ifelse(median_larval_stop$genotype == "norm_7", "loxP",
                                                  ifelse(median_larval_stop$genotype == "norm_25", "GEPD",
                                                         ifelse(median_larval_stop$genotype == "norm_137", "GEPD",
                                                                "GEPD"))))), levels = c("loxP", "GEPD"))
# median width of pooled data
gg_median_larval_stop_pooled <- ggplot(median_larval_stop, aes(x = Genotype, y = median_larval_stop)) +
  geom_boxplot(aes(fill = Genotype), outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(position = position_jitter(0.2), na.rm = TRUE, shape = 4, size = 6) +
  scale_shape_discrete(solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Width / Length", limits = c(0, 0.6)) +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)),
        text = element_text(size = 50),
        legend.position = "none")
gg_median_larval_stop_pooled

# Stats
t.test(median_larval_stop ~ Genotype, data = median_larval_stop)

# Error blarval_stops
max_larval_stop <- max(median_larval_stop$median_larval_stop) + 0.01
sign_max_larval_stop <- data.frame(Genotype = c(1, 2), median_larval_stop = rep(max_larval_stop, 2))
gg_median_larval_stop_pooled2 <- gg_median_larval_stop_pooled + geom_line(data = sign_max_larval_stop, aes(x = Genotype, y = median_larval_stop, group = 1)) +
  annotate("text", x = 1.5, y = max_larval_stop + 0.01, label = "**", size = 10)

gg_median_larval_stop_pooled2


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
stretch <- draw_dual_plot("../../72.1.1/cropped/tracked_VID_20180521_191900.mp4_nosound.mp4_cropped.mp4.csv")
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
sign_loxp <- data.frame(genotype = c("13211", "11111", "711"), threshold_crosses = rep(max_loxp + 1, 3))
sign_gepd <- data.frame(genotype = c("2511", "13713", "7211"), threshold_crosses = rep(max_gepd + 1, 3))

gg2 <- gg + geom_line(data = sign_loxp, aes(x = genotype, y = threshold_crosses, group = 1)) +
  annotate("text", x = "11111", y = max_loxp + 2, label = "ns", size = 10)

gg3 <- gg2 + geom_line(data = sign_gepd, aes(x = genotype, y = threshold_crosses, group = 1)) +
  annotate("text", x = "13713", y = max_gepd + 2, label = "ns", size = 10)

# Draw error bar for betwen-group stats
sign_combined <- data.frame(genotype = c("11111", "13713"), threshold_crosses = rep(max_loxp + 3, 2))
gg4 <- gg3 + geom_line(data = sign_combined, aes(x = genotype, y = threshold_crosses, group = 1)) +
  annotate("text", x = 3.5, y = max_loxp + 3.5, label = "        ****", size = 10)
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
gg <- ggplot(turns, aes(x = Genotype, y = threshold_crosses)) + 
  geom_boxplot(aes(fill = Genotype), outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(position = position_jitter(0.2), na.rm = TRUE, shape = 4, size = 6) +
  scale_shape_discrete(solid = FALSE) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Turns / min") +
  scale_fill_manual(values = c("grey64", "firebrick")) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 50, margin = margin(t = 0, r = 30, b = 0, l = 0)), 
        text = element_text(size = 50), 
        legend.position = "none")
gg

# Stats
# Stats on individual replicates
wilcox.test(turns$threshold_crosses ~ turns$Genotype, data = turns)

# Draw error bars
setDT(turns)

max_turns <- turns[, max(threshold_crosses), by = Genotype]
max_loxp <- max_turns[1,2]
max_loxp <- as.numeric(max_loxp)
max_loxp <- max_loxp + 0.5

# Draw error bars for gg
sign_loxp <- data.frame(Genotype = c("loxP", "GEPD"), threshold_crosses = rep(max_loxp + 1, 2))

gg2 <- gg + geom_line(data = sign_loxp, aes(x = Genotype, y = threshold_crosses, group = 1)) +
  annotate("text", x = 1.5, y = max_loxp + 2, label = "****", size = 10)
gg2

###############################################################################
#################################  END  #######################################
###############################################################################

sessionInfo()










