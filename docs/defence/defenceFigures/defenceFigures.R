# Wildchrokie 
# CRD 5 August 2026

# Conceptual figure broken down for MSc defence

# housekeeping
rm(list=ls())
options(stringsAsFactors = FALSE)

# Load library 
library(future)
library(patchwork) 
library(rsvg)
library(shape)
library(pollen)

setwd("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses")

source("rcode/tools.R")
source("rcode/growthModelsMain.R")

# flags
makeplots <- FALSE

climatesum <- read.csv("output/climateSummariesYear.csv")
climatesummonth <- read.csv("output/climateSummariesByMonth.csv")
# gddyr <- read.csv("output/gddByYear.csv")
gddyr <- read.csv("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/output/gddByYear.csv")
weldhillclim <- read.csv("output/weldhillClimateCleaned.csv")

logan <- read.csv("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses/output/loganLongTermCleaned.csv")

# emp <- empir[!is.na(empir$leafout),]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Climate data #### 
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
emp$leafout <- as.integer(emp$leafout)
emp$budset <- as.integer(emp$budset)

gddyr$yeardoy <- paste(gddyr$year, gddyr$doy, sep = "_")
emp$yeardoybudburst <- paste(emp$year, emp$budburst, sep = "_")
emp$yeardoyleafout <- paste(emp$year, emp$leafout, sep = "_")
emp$yeardoybudset <- paste(emp$year, emp$budset, sep = "_")

# calculate daily gdd with caping to 0 with cold temp
gddyr$dgdd <- pmax(gddyr$meanTempC - 5, 0)
dgddagg <- aggregate(dgdd ~ doy, gddyr, FUN = mean)

# start of season average
lo <- aggregate(leafout ~ latbi, emp, FUN = mean)
bs <- aggregate(budset ~ latbi, emp, FUN = mean)
gslength <- merge(lo, bs, by = "latbi")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Conceptual figure broken down ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Set curves and stuff
# pre cc
colpre <- "#04a3bd"
colcc <- "#a00e00"

colspring <- "#247d3f"
colfall <- "#da7901"

axissize <- 2
labsize <- 2

# assign sos and eos values
ccsos <- 110
cceos <- 260
presos <- 130
preeos <- 250

# years for pre climate change and post climate change
preyr <- 1955:1975
ccyr <- 2005:2025

# Logistic curves (Panel 2 still uses these)
doy_seq <- 30:330
gdd_pre <- 2500 / (1 + exp(-0.025 * (doy_seq - 172)))
gdd_cc  <- 3000 / (1 + exp(-0.025 * (doy_seq - 140)))

# calendar days
ticks <- seq(min(doy_seq), max(doy_seq), by = 30)
dates <- format(as.Date(ticks, origin = "2023-01-01"), "%d %b")

myxlimp3 <- c(min(doy_seq), max(doy_seq))
mylwd <- 3
mysmalltxt <- 2
mylargetxt <- 3

# Panel margins
p1 <- c(0, 5, 0, 2)
p2 <- c(3, 5, 2, 2)
p3 <- c(0, 5, 0, 2)

# matrix heights
matheights <- c(1, 2.8, 2.4)

# ylim logistic
ylimlogis <- c(0, 3400)

# Real data from logan airport
prelogan <- subset(logan, year %in% preyr & doy >= presos & doy <= preeos)
cclogan <- subset(logan, year %in% ccyr & doy >= ccsos & doy <= cceos)

# GDD for logistic curves for both periods
prelogan$GDD_5 <- NA
cclogan$GDD_5 <- NA

# Start with pre climate change
years <- unique(prelogan$year)

# Loop through each year
for (y in years) {
  # Find rows for this year
  year_rows <- which(prelogan$year == y)
  
  # Calculate GDD for this year only
  prelogan$GDD_5[year_rows] <- gdd(tmax = prelogan$maxTempC[year_rows], 
                                   tmin = prelogan$minTempC[year_rows], 
                                   tbase = 5, 
                                   type = "B")
}
mean_pre_gdd <- aggregate(GDD_5 ~ doy, data = prelogan, FUN = mean, na.rm = TRUE)

# Then post climate change
years <- unique(cclogan$year)

# Loop through each year
for (y in years) {
  # Find rows for this year
  year_rows <- which(cclogan$year == y)
  
  # Calculate GDD for this year only
  cclogan$GDD_5[year_rows] <- gdd(tmax = cclogan$maxTempC[year_rows], 
                                  tmin = cclogan$minTempC[year_rows], 
                                  tbase = 5, 
                                  type = "B")
}
mean_cc_gdd  <- aggregate(GDD_5 ~ doy, data = cclogan,  FUN = mean, na.rm = TRUE)

baselineperiod <- subset(logan, year >1940 & year < 1981)
baselinemean <- mean(baselineperiod$meanTempC)

prewarm <- subset(logan, year > 1954 & year < 1976)
# prewarm$meanTempC <- prewarm$meanTempC - baselinemean
poswarm <- subset(logan, year > 2004 & year < 2026)
# poswarm$meanTempC <- poswarm$meanTempC - baselinemean

mean_pre <- aggregate(meanTempC ~ doy, data = prewarm, FUN = mean, na.rm = TRUE)
mean_pos  <- aggregate(meanTempC ~ doy, data = poswarm,  FUN = mean, na.rm = TRUE)

# Loess smoothing (span controls wiggliness: 0.3 = more wiggly, 0.5 = smoother)
loess_pre <- loess(meanTempC ~ doy, data = mean_pre, span = 0.4)
loess_pos  <- loess(meanTempC ~ doy, data = mean_pos,  span = 0.4)

smooth_pre <- predict(loess_pre, newdata = data.frame(doy = doy_seq))
smooth_cc  <- predict(loess_pos,  newdata = data.frame(doy = doy_seq))

# Set data for 5C threshold
threshold <- 5
mask <- smooth_pre <= threshold & doy_seq > 53
doy_seq2   <- doy_seq[mask]
smooth_pre2 <- smooth_pre[mask]

x_poly <- c(doy_seq2, rev(doy_seq2))
y_poly <- c(smooth_pre2, rev(rep(0, length(doy_seq2))))

# y-axis limit based on real data
ylim_temp <- c(0,max(smooth_cc))

# rasterized pictograms
img_thermom <- rsvg::rsvg("figures/pictogramsLeaves/thermometer.svg")
img_calenda <- rsvg::rsvg("figures/pictogramsLeaves/calendar.svg")
img_leafout <- rsvg::rsvg("figures/pictogramsLeaves/bepaPicLeafout.svg")
img_budset  <- rsvg::rsvg("figures/pictogramsLeaves/bepaPicBudset.svg")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# 1. Growing season length only ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
jpeg("~/github/MSthesis/docs/defence/defenceFigures/concept1.jpeg", width = 11, height = 8, units = "in", res = 400)
layout(matrix(c(1, 2, 3), nrow = 3), heights = matheights)

# Panel 1 --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
par(mar = p3)
plot.new()
plot.window(xlim = myxlimp3, ylim = c(0, 1))

# pictograms width and height
img_w <- 23
img_h <- 0.6
x_left <- ccsos
x_right <- cceos

# pictograms scaler
norm <- 2
arrow_y <- 0.3
rasterImage(img_leafout,
            x_left - 13 - img_w/norm,
            arrow_y - img_h/norm,
            x_left - 13 + img_w/norm,
            arrow_y + img_h/norm)
rasterImage(img_budset,
            x_right + 13 - img_w/norm,
            arrow_y - img_h/norm,
            x_right + 13 + img_w/norm,
            arrow_y + img_h/norm)

# Pre season arrow
arrow_y <- 0.1
cap_h <- 0.06
x_left <- presos
x_right <- preeos
segments(x0 = x_left, x1 = x_right, y0 = arrow_y, lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))
segments(x0 = x_left,  y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))
segments(x0 = x_right, y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))

# Panel 2: Temperature curves --- --- --- --- --- --- --- --- --- --- --- --- ---
par(mar = p1)
plot(doy_seq, smooth_pre, type = "n",
     xaxt = "n", ylim = ylim_temp,
     xlab = "", bty = "l",
     ylab = expression(paste("Temperature (", degree, "C)")), 
     # frame = FALSE,
     cex.axis = axissize, cex.lab = labsize)
axis(1, at = ticks, labels = dates, cex.axis = axissize)
# Shade area below threshold under pre curve
polygon(x_poly, y_poly, col = adjustcolor("grey", alpha.f = 0.6), border = NA)

lines(doy_seq, smooth_pre, lwd = 1, col = adjustcolor(colpre, alpha.f = 0.7))

# thicker lines within their respective leafout and budset timings
doypre <- presos : preeos
idx_pre <- which(doy_seq %in% doypre)
lines(doy_seq[idx_pre], smooth_pre[idx_pre], 
      lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))

# pre: start and end of thick segment
r <- 1.5  
filledcircle(r1 = r, mid = c(presos,  smooth_pre[doy_seq %in% presos]),
             col = adjustcolor(colpre, alpha.f = 1), lcol = NA)
filledcircle(r1 = r, mid = c(preeos,  smooth_pre[doy_seq %in% preeos]),
             col = adjustcolor(colpre, alpha.f = 1), lcol = NA)

# Pre-CC boundaries (lighter)
segments(x0 = presos, y0 = 0, y1 = 30, lwd = 0.3, lty = 2)
segments(x0 = preeos, y0 = 0, y1 = 30, lwd = 0.3, lty = 2)

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# 2. Temperature curve with climate change example ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
jpeg("~/github/MSthesis/docs/defence/defenceFigures/concept2.jpeg", width = 11, height = 8, units = "in", res = 400)
layout(matrix(c(1, 2, 3), nrow = 3), heights = matheights)

# Panel 1 --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
par(mar = p3)
plot.new()
plot.window(xlim = myxlimp3, ylim = c(0, 1))

# CC season arrow
arrow_y <- 0.3
cap_h <- 0.15
x_left <- ccsos
x_right <- cceos
segments(x0 = x_left, x1 = x_right, y0 = arrow_y, lwd = mylwd, col = colcc)
segments(x0 = x_left,  y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = colcc)
segments(x0 = x_right, y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = colcc)

# pictograms width and height
img_w <- 23
img_h <- 0.6

# pictograms scaler
smll <- 2
norm <- 2
rasterImage(img_leafout,
            x_left - 13 - img_w/norm,
            arrow_y - img_h/norm,
            x_left - 13 + img_w/norm,
            arrow_y + img_h/norm)

rasterImage(img_budset,
            x_right + 13 - img_w/norm,
            arrow_y - img_h/norm,
            x_right + 13 + img_w/norm,
            arrow_y + img_h/norm)
text(x = ccsos + (cceos - ccsos)/2, y = arrow_y + 0.15,
     "Longer calendar season", col = "black", cex = mylargetxt)

# Pre season arrow
arrow_y <- 0.1
cap_h <- 0.06
x_left <- presos
x_right <- preeos
segments(x0 = x_left, x1 = x_right, y0 = arrow_y, lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))
segments(x0 = x_left,  y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))
segments(x0 = x_right, y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))

arrow_y <- 0.4

# text(x = ccsos + (cceos - ccsos)/2, y = arrow_y + 0.15,
     # "Longer calendar season", col = "black", cex = mylargetxt)

# Panel 2: Temperature curves --- --- --- --- --- --- --- --- --- --- --- --- ---
par(mar = p1)
plot(doy_seq, smooth_pre, type = "n",
     xaxt = "n", ylim = ylim_temp,
     xlab = "", bty = "l",
     ylab = expression(paste("Temperature (", degree, "C)")), 
     # frame = FALSE,
     cex.axis = axissize, cex.lab = labsize)
axis(1, at = ticks, labels = dates, cex.axis = axissize)

# Shade area below threshold under pre curve
polygon(x_poly, y_poly, col = adjustcolor("grey", alpha.f = 0.6), border = NA)

lines(doy_seq, smooth_pre, lwd = 1, col = adjustcolor(colpre, alpha.f = 0.7))
lines(doy_seq, smooth_cc,  lwd = 1, col = colcc)

# thicker lines within their respective leafout and budset timings
doypre <- presos : preeos
idx_pre <- which(doy_seq %in% doypre)
lines(doy_seq[idx_pre], smooth_pre[idx_pre], 
      lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))

doycc <- ccsos:cceos
idx_cc <- which(doy_seq %in% doycc)
lines(doy_seq[idx_cc], smooth_cc[idx_cc], lwd = mylwd, col = colcc)

# pre: start and end of thick segment
r <- 1.5  
filledcircle(r1 = r, mid = c(presos,  smooth_pre[doy_seq %in% presos]),
             col = adjustcolor(colpre, alpha.f = 1), lcol = NA)
filledcircle(r1 = r, mid = c(preeos,  smooth_pre[doy_seq %in% preeos]),
             col = adjustcolor(colpre, alpha.f = 1), lcol = NA)

# cc: start and end of thick segment
filledcircle(r1 = r, mid = c(ccsos, smooth_cc[doy_seq %in% ccsos]),
             col = colcc, lcol = NA)
filledcircle(r1 = r, mid = c(cceos, smooth_cc[doy_seq %in% cceos]),
             col = colcc, lcol = NA)

# GS delimitations
segments(x0 = ccsos, y0 = 0, y1 = 30,  lwd = 1.5, lty = 2)
segments(x0 = cceos, y0 = 0, y1 = 30,  lwd = 1.5, lty = 2)

# Pre-CC boundaries (lighter)
segments(x0 = presos, y0 = 0, y1 = 30, lwd = 0.3, lty = 2)
segments(x0 = preeos, y0 = 0, y1 = 30, lwd = 0.3, lty = 2)

# Phenology trend arrows
Arrows(x0 = ccsos + 20, y0 = 5, x1 = ccsos + 2, y1 = 5,
       arr.type = "triangle", arr.width = 0.3, lwd = 2, col = colspring,
       arr.lwd = 0.5, arr.length = 0.2)
Arrows(x0 = cceos - 10, y0 = 5, x1 = cceos - 2, y1 = 5,
       arr.type = "triangle", arr.width = 0.3, lwd = 2, col = colfall, 
       arr.lwd = 0.5, arr.length = 0.2)

text(x = ccsos + 30, y = 7, "Earlier SOS", col = colspring, cex = mylargetxt)
text(x = cceos - 30, y = 7, "Later EOS",   col = colfall, cex = mylargetxt)

# Panel 3: GDD curves --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
par(mar = p2)

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> 
# 3. Add GDD accumulation curve ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
jpeg("~/github/MSthesis/docs/defence/defenceFigures/concept3.jpeg", width = 11, height = 8, units = "in", res = 400)
layout(matrix(c(1, 2, 3), nrow = 3), heights = matheights)

# Panel 1 --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
par(mar = p3)
plot.new()
plot.window(xlim = myxlimp3, ylim = c(0, 1))

# CC season arrow
arrow_y <- 0.3
cap_h <- 0.15
x_left <- ccsos
x_right <- cceos
segments(x0 = x_left, x1 = x_right, y0 = arrow_y, lwd = mylwd, col = colcc)
segments(x0 = x_left,  y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = colcc)
segments(x0 = x_right, y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = colcc)

# pictograms width and height
img_w <- 23
img_h <- 0.6

# pictograms scaler
smll <- 2
norm <- 2
rasterImage(img_leafout,
            x_left - 13 - img_w/norm,
            arrow_y - img_h/norm,
            x_left - 13 + img_w/norm,
            arrow_y + img_h/norm)

rasterImage(img_budset,
            x_right + 13 - img_w/norm,
            arrow_y - img_h/norm,
            x_right + 13 + img_w/norm,
            arrow_y + img_h/norm)
text(x = ccsos + (cceos - ccsos)/2, y = arrow_y + 0.15,
     "Longer calendar season", col = "black", cex = mylargetxt)

# Pre season arrow
arrow_y <- 0.1
cap_h <- 0.06
x_left <- presos
x_right <- preeos
segments(x0 = x_left, x1 = x_right, y0 = arrow_y, lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))
segments(x0 = x_left,  y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))
segments(x0 = x_right, y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))

arrow_y <- 0.4

# Panel 2: Temperature curves --- --- --- --- --- --- --- --- --- --- --- --- ---
par(mar = p1)
plot(doy_seq, smooth_pre, type = "n",
     xaxt = "n", ylim = ylim_temp,
     xlab = "", bty = "l",
     ylab = expression(paste("Temperature (", degree, "C)")), 
     # frame = FALSE,
     cex.axis = axissize, cex.lab = labsize)


# Shade area below threshold under pre curve
polygon(x_poly, y_poly, col = adjustcolor("grey", alpha.f = 0.6), border = NA)

lines(doy_seq, smooth_pre, lwd = 1, col = adjustcolor(colpre, alpha.f = 0.7))
lines(doy_seq, smooth_cc,  lwd = 1, col = colcc)

# thicker lines within their respective leafout and budset timings
doypre <- presos : preeos
idx_pre <- which(doy_seq %in% doypre)
lines(doy_seq[idx_pre], smooth_pre[idx_pre], 
      lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))

doycc <- ccsos:cceos
idx_cc <- which(doy_seq %in% doycc)
lines(doy_seq[idx_cc], smooth_cc[idx_cc], lwd = mylwd, col = colcc)

# pre: start and end of thick segment
r <- 1.5  
filledcircle(r1 = r, mid = c(presos,  smooth_pre[doy_seq %in% presos]),
             col = adjustcolor(colpre, alpha.f = 1), lcol = NA)
filledcircle(r1 = r, mid = c(preeos,  smooth_pre[doy_seq %in% preeos]),
             col = adjustcolor(colpre, alpha.f = 1), lcol = NA)

# cc: start and end of thick segment
filledcircle(r1 = r, mid = c(ccsos, smooth_cc[doy_seq %in% ccsos]),
             col = colcc, lcol = NA)
filledcircle(r1 = r, mid = c(cceos, smooth_cc[doy_seq %in% cceos]),
             col = colcc, lcol = NA)

# GS delimitations
segments(x0 = ccsos, y0 = 0, y1 = 30,  lwd = 1.5, lty = 2)
segments(x0 = cceos, y0 = 0, y1 = 30,  lwd = 1.5, lty = 2)

# Pre-CC boundaries (lighter)
segments(x0 = presos, y0 = 0, y1 = 30, lwd = 0.3, lty = 2)
segments(x0 = preeos, y0 = 0, y1 = 30, lwd = 0.3, lty = 2)

# Phenology trend arrows
# Phenology trend arrows
Arrows(x0 = ccsos + 20, y0 = 5, x1 = ccsos + 2, y1 = 5,
       arr.type = "triangle", arr.width = 0.3, lwd = 2, col = colspring,
       arr.lwd = 0.5, arr.length = 0.2)
Arrows(x0 = cceos - 10, y0 = 5, x1 = cceos - 2, y1 = 5,
       arr.type = "triangle", arr.width = 0.3, lwd = 2, col = colfall, 
       arr.lwd = 0.5, arr.length = 0.2)

text(x = ccsos + 30, y = 7, "Earlier SOS", col = colspring, cex = mylargetxt)
text(x = cceos - 30, y = 7, "Later EOS",   col = colfall, cex = mylargetxt)
# Panel 3: GDD curves --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
par(mar = p2)
plot(doy_seq, gdd_cc, ylim = range(mean_cc_gdd$GDD_5),
     type = "n", lwd = 1.2,
     xlab = "", ylab = "Accumulated GDD",
     xaxt = "n", bty = "l",
     # frame = FALSE,
     col = adjustcolor(colpre, alpha.f = 0.4),
     main = "", cex.axis = axissize, cex.lab = labsize)

axis(1, at = ticks, labels = dates, cex.axis = axissize)


lines(mean_pre_gdd$doy, mean_pre_gdd$GDD_5, type = "l", lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))
lines(mean_cc_gdd$doy, mean_cc_gdd$GDD_5,  type = "l", lwd = mylwd, col = adjustcolor(colcc))

# pre
filledcircle(r1 = r, mid = c(mean_pre_gdd$doy[1],  mean_pre_gdd$GDD_5[1]),
             col = adjustcolor(colpre, alpha.f = 1), lcol = NA)
filledcircle(r1 = r, mid = c(mean_pre_gdd$doy[nrow(mean_pre_gdd)], mean_pre_gdd$GDD_5[nrow(mean_pre_gdd)]),
             col = adjustcolor(colpre, alpha.f = 1), lcol = NA)

# cc
filledcircle(r1 = r, mid = c(mean_cc_gdd$doy[1], mean_cc_gdd$GDD_5[1]),
             col = adjustcolor(colcc), lcol = NA)
filledcircle(r1 = r, mid = c(mean_cc_gdd$doy[nrow(mean_cc_gdd)], mean_cc_gdd$GDD_5[nrow(mean_cc_gdd)]),
             col = adjustcolor(colcc), lcol = NA)

# Pre-CC boundaries (lighter)
segments(x0 = presos, y0 = -2, y1 = 3000, lwd = 0.3, lty = 2)
segments(x0 = preeos, y0 = -2, y1 = 3000, lwd = 0.3, lty = 2)

segments(x0 = ccsos, y0 = -100, y1 = 3000, lwd = 1.5, lty = 2)
segments(x0 = cceos, y0 = -100, y1 = 3000, lwd = 1.5, lty = 2)

# Polygon for warmer thermal season
x_arrow <- cceos + 5

shaft_w <- 2
head_w  <- 4
head_l  <- 150

y_start <- mean_pre_gdd$GDD_5[mean_pre_gdd$doy %in% preeos]
y_end   <- mean_cc_gdd$GDD_5[mean_cc_gdd$doy %in% cceos]

direction <- sign(y_end - y_start)
y_neck <- y_end - direction * head_l

polygon(
  x = c(x_arrow - shaft_w,
        x_arrow - shaft_w,
        x_arrow - head_w,
        x_arrow,
        x_arrow + head_w,
        x_arrow + shaft_w,
        x_arrow + shaft_w),
  y = c(y_start,
        y_neck,
        y_neck,
        y_end,
        y_neck,
        y_neck,
        y_start),
  col = adjustcolor(colcc, alpha.f = 1),
  border = NA
)

text(x = x_arrow + 5, y = y_start + 150, 
     "Warmer \nthermal season", col = "black", cex = mylargetxt, adj = 0)
img_w <- 23
img_h <- 3400 * 0.2
smll <- 4.3
norm <- 2

dev.off()


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> 
# 4. SOS and EOS assymetry lines ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
jpeg("~/github/MSthesis/docs/defence/defenceFigures/concept4.jpeg", width = 11, height = 8, units = "in", res = 400)
layout(matrix(c(1, 2, 3), nrow = 3), heights = matheights)

# Panel 1 --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
par(mar = p3)
plot.new()
plot.window(xlim = myxlimp3, ylim = c(0, 1))

# CC season arrow
arrow_y <- 0.3
cap_h <- 0.15
x_left <- ccsos
x_right <- cceos
segments(x0 = x_left, x1 = x_right, y0 = arrow_y, lwd = mylwd, col = colcc)
segments(x0 = x_left,  y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = colcc)
segments(x0 = x_right, y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = colcc)

# pictograms width and height
img_w <- 23
img_h <- 0.6

# pictograms scaler
smll <- 2
norm <- 2
rasterImage(img_leafout,
            x_left - 13 - img_w/norm,
            arrow_y - img_h/norm,
            x_left - 13 + img_w/norm,
            arrow_y + img_h/norm)

rasterImage(img_budset,
            x_right + 13 - img_w/norm,
            arrow_y - img_h/norm,
            x_right + 13 + img_w/norm,
            arrow_y + img_h/norm)
text(x = ccsos + (cceos - ccsos)/2, y = arrow_y + 0.15,
     "Longer calendar season", col = "black", cex = mylargetxt)

# Pre season arrow
arrow_y <- 0.1
cap_h <- 0.06
x_left <- presos
x_right <- preeos
segments(x0 = x_left, x1 = x_right, y0 = arrow_y, lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))
segments(x0 = x_left,  y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))
segments(x0 = x_right, y0 = arrow_y - cap_h, y1 = arrow_y + cap_h, lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))

arrow_y <- 0.4

# Panel 2: Temperature curves --- --- --- --- --- --- --- --- --- --- --- --- ---
par(mar = p1)
plot(doy_seq, smooth_pre, type = "n",
     xaxt = "n", ylim = ylim_temp,
     xlab = "", bty = "l",
     ylab = expression(paste("Temperature (", degree, "C)")), 
     # frame = FALSE,
     cex.axis = axissize, cex.lab = labsize)


# Shade area below threshold under pre curve
polygon(x_poly, y_poly, col = adjustcolor("grey", alpha.f = 0.6), border = NA)

lines(doy_seq, smooth_pre, lwd = 1, col = adjustcolor(colpre, alpha.f = 0.7))
lines(doy_seq, smooth_cc,  lwd = 1, col = colcc)

# thicker lines within their respective leafout and budset timings
doypre <- presos : preeos
idx_pre <- which(doy_seq %in% doypre)
lines(doy_seq[idx_pre], smooth_pre[idx_pre], 
      lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))

doycc <- ccsos:cceos
idx_cc <- which(doy_seq %in% doycc)
lines(doy_seq[idx_cc], smooth_cc[idx_cc], lwd = mylwd, col = colcc)

# Cooler first days of growth delimitations
segments(x0 = presos, x1 = presos - 30, y0 = smooth_pre[doy_seq %in% presos],  
         lwd = 1.2, lty = 3, col = colpre)

segments(x0 = ccsos, x1 = presos - 30, y0 = smooth_cc[doy_seq %in% ccsos],  
         lwd = 1.2, lty = 3, col = colcc)

# Segments that shows cooler temperature early in the season
Arrows(x0 = presos - 35, x1 = presos - 35,
       y0 = smooth_cc[doy_seq %in% ccsos], 
       y1 = smooth_pre[doy_seq %in% presos], 
       lwd = 3, lty = 1, col = colspring, arr.type = "T", code = 3)

# End-of-season delimitations
segments(x0 = preeos, x1 = preeos - 10,
         y0 = smooth_pre[doy_seq %in% preeos],  
         lwd = 1.2, lty = 3, col = colpre)

segments(x0 = cceos, x1 = preeos - 10,
         y0 = smooth_cc[doy_seq %in% cceos],  
         lwd = 1.2, lty = 3, col = colcc)

# Difference at end of season
Arrows(x0 = preeos - 15, x1 = preeos - 15,
       y0 = smooth_cc[doy_seq %in% cceos], 
       y1 = smooth_pre[doy_seq %in% preeos], 
       lwd = 2, lty = 1, col = colfall, arr.type = "T", code = 3)

# pre: start and end of thick segment
r <- 1.5  
filledcircle(r1 = r, mid = c(presos,  smooth_pre[doy_seq %in% presos]),
             col = adjustcolor(colpre, alpha.f = 1), lcol = NA)
filledcircle(r1 = r, mid = c(preeos,  smooth_pre[doy_seq %in% preeos]),
             col = adjustcolor(colpre, alpha.f = 1), lcol = NA)

# cc: start and end of thick segment
filledcircle(r1 = r, mid = c(ccsos, smooth_cc[doy_seq %in% ccsos]),
             col = colcc, lcol = NA)
filledcircle(r1 = r, mid = c(cceos, smooth_cc[doy_seq %in% cceos]),
             col = colcc, lcol = NA)

# GS delimitations
segments(x0 = ccsos, y0 = 0, y1 = 30,  lwd = 1.5, lty = 2)
segments(x0 = cceos, y0 = 0, y1 = 30,  lwd = 1.5, lty = 2)

# Pre-CC boundaries (lighter)
segments(x0 = presos, y0 = 0, y1 = 30, lwd = 0.3, lty = 2)
segments(x0 = preeos, y0 = 0, y1 = 30, lwd = 0.3, lty = 2)

# Phenology trend arrows
# Phenology trend arrows
Arrows(x0 = ccsos + 20, y0 = 5, x1 = ccsos + 2, y1 = 5,
       arr.type = "triangle", arr.width = 0.3, lwd = 2, col = colspring,
       arr.lwd = 0.5, arr.length = 0.2)
Arrows(x0 = cceos - 10, y0 = 5, x1 = cceos - 2, y1 = 5,
       arr.type = "triangle", arr.width = 0.3, lwd = 2, col = colfall, 
       arr.lwd = 0.5, arr.length = 0.2)

text(x = ccsos + 30, y = 7, "Earlier SOS", col = colspring, cex = mylargetxt)
text(x = cceos - 30, y = 7, "Later EOS",   col = colfall, cex = mylargetxt)

# Panel 3: GDD curves --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
par(mar = p2)
plot(doy_seq, gdd_cc, ylim = range(mean_cc_gdd$GDD_5),
     type = "n", lwd = 1.2,
     xlab = "", ylab = "Accumulated GDD",
     xaxt = "n", bty = "l",
     # frame = FALSE,
     col = adjustcolor(colpre, alpha.f = 0.4),
     main = "", cex.axis = axissize, cex.lab = labsize)

axis(1, at = ticks, labels = dates, cex.axis = axissize)


# mean_pre_gdd <- subset(mean_pre_gdd, doy <= max(doy_seq))
# mean_cc_gdd <- subset(mean_cc_gdd, doy <= max(doy_seq))

lines(mean_pre_gdd$doy, mean_pre_gdd$GDD_5, type = "l", lwd = mylwd, col = adjustcolor(colpre, alpha.f = 0.8))
lines(mean_cc_gdd$doy, mean_cc_gdd$GDD_5,  type = "l", lwd = mylwd, col = adjustcolor(colcc))

# pre
filledcircle(r1 = r, mid = c(mean_pre_gdd$doy[1],  mean_pre_gdd$GDD_5[1]),
             col = adjustcolor(colpre, alpha.f = 1), lcol = NA)
filledcircle(r1 = r, mid = c(mean_pre_gdd$doy[nrow(mean_pre_gdd)], mean_pre_gdd$GDD_5[nrow(mean_pre_gdd)]),
             col = adjustcolor(colpre, alpha.f = 1), lcol = NA)

# cc
filledcircle(r1 = r, mid = c(mean_cc_gdd$doy[1], mean_cc_gdd$GDD_5[1]),
             col = adjustcolor(colcc), lcol = NA)
filledcircle(r1 = r, mid = c(mean_cc_gdd$doy[nrow(mean_cc_gdd)], mean_cc_gdd$GDD_5[nrow(mean_cc_gdd)]),
             col = adjustcolor(colcc), lcol = NA)

# Pre-CC boundaries (lighter)
segments(x0 = presos, y0 = -2, y1 = 3000, lwd = 0.3, lty = 2)
segments(x0 = preeos, y0 = -2, y1 = 3000, lwd = 0.3, lty = 2)

segments(x0 = ccsos, y0 = -100, y1 = 3000, lwd = 1.5, lty = 2)
segments(x0 = cceos, y0 = -100, y1 = 3000, lwd = 1.5, lty = 2)

# Early season gdd
# Segments that shows cooler temperature early in the season
segments(x0 = presos, x1 = presos - 30, 
         y0 = mean_pre_gdd$GDD_5[mean_pre_gdd$doy %in% presos],  
         lwd = 0.8, lty = 3, col = colpre)

segments(x0 = presos, x1 = presos - 30, 
         y0 = mean_cc_gdd$GDD_5[mean_cc_gdd$doy %in% presos],  
         lwd = 0.8, lty = 3, col = colcc)

Arrows(x0 = presos - 35, x1 = presos - 35,
       y0 = min(mean_cc_gdd$GDD_5), 
       y1 = mean_cc_gdd$GDD_5[mean_cc_gdd$doy %in% presos], 
       lwd = 3, lty = 1, col = colspring, arr.type = "T", code = 3)

# Late season gdd
segments(x0 = preeos, x1 = cceos - 30,
         y0 = mean_pre_gdd$GDD_5[mean_pre_gdd$doy %in% preeos],
         lwd = 1.2, lty = 3, col = colpre)

segments(x0 = cceos, x1 = cceos - 30,
         y0 = mean_cc_gdd$GDD_5[mean_cc_gdd$doy %in% cceos],
         lwd = 1.2, lty = 3, col = colcc)

Arrows(x0 = cceos - 35, x1 = cceos - 35,
       y0 = mean_pre_gdd$GDD_5[mean_pre_gdd$doy %in% preeos], 
       y1 = mean_cc_gdd$GDD_5[mean_cc_gdd$doy %in% cceos], 
       lwd = 3, lty = 1, col = colfall, arr.type = "T", code = 3)

# Polygon for warmer thermal season
x_arrow <- cceos + 5

shaft_w <- 2
head_w  <- 4
head_l  <- 150

y_start <- mean_pre_gdd$GDD_5[mean_pre_gdd$doy %in% preeos]
y_end   <- mean_cc_gdd$GDD_5[mean_cc_gdd$doy %in% cceos]

direction <- sign(y_end - y_start)
y_neck <- y_end - direction * head_l

polygon(
  x = c(x_arrow - shaft_w,
        x_arrow - shaft_w,
        x_arrow - head_w,
        x_arrow,
        x_arrow + head_w,
        x_arrow + shaft_w,
        x_arrow + shaft_w),
  y = c(y_start,
        y_neck,
        y_neck,
        y_end,
        y_neck,
        y_neck,
        y_start),
  col = adjustcolor(colcc, alpha.f = 1),
  border = NA
)

text(x = x_arrow + 5, y = y_start + 150, 
     "Warmer \nthermal season", col = "black", cex = mylargetxt, adj = 0)
img_w <- 23
img_h <- 3400 * 0.2
smll <- 4.3
norm <- 2

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Parameter estimates ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
par(family = "Helvetica")

# === === === === === === === === === === === === === === === === 
# EMPIRICAL DATA ####
# === === === === === === === === === === === === === === === === 
climatesum <- read.csv("output/climateSummariesYear.csv")
weldhillclim <- read.csv("output/weldhillClimateCleaned.csv")

emp$latbi[which(emp$latbi %in% "Alnus incana")] <- "A. incana"
emp$latbi[which(emp$latbi %in% "Betula alleghaniensis")] <- "B. alleghaniensis"
emp$latbi[which(emp$latbi %in% "Betula papyrifera")] <- "B. papyrifera"
emp$latbi[which(emp$latbi %in% "Betula populifolia")] <- "B. populifolia"

# Load parameter summaries generated in growthModelsMain.R ####
sigma_df2  <- read.csv("output/GM_GDDparam_sigma.csv")
bspp_df2   <- read.csv("output/GM_GDDparam_bspp.csv")
treeid_df2 <- read.csv("output/GM_GDDparam_treeid.csv")
aspp_df2   <- read.csv("output/GM_GDDparam_aspp.csv")
site_df2   <- read.csv("output/GM_GDDparam_site.csv")

treeid_df2$treeid <- as.numeric(treeid_df2$treeid)  
treeid_df2$treeid_name <- emp$treeid[match(treeid_df2$treeid, emp$treeid_num)]
bspp_df2$spp_name <- emp$latbi[match(bspp_df2$spp, emp$spp_num)]
site_df2$site_name <- emp$site[match(site_df2$site, emp$site_num)]
aspp_df2$spp_name <- emp$latbi[match(aspp_df2$spp, emp$spp_num)]

# GSL
sigma_df2_gsl  <- read.csv("output/GM_GSLparam_sigma.csv")
bspp_df2_gsl   <- read.csv("output/GM_GSLparam_bspp.csv")
treeid_df2_gsl <- read.csv("output/GM_GSLparam_treeid.csv")
aspp_df2_gsl   <- read.csv("output/GM_GSLparam_aspp.csv")
site_df2_gsl   <- read.csv("output/GM_GSLparam_site.csv")

treeid_df2_gsl$treeid <- as.numeric(treeid_df2_gsl$treeid)
treeid_df2_gsl$treeid_name <- emp$treeid[match(treeid_df2_gsl$treeid, emp$treeid_num)]
bspp_df2_gsl$spp_name <- emp$latbi[match(bspp_df2_gsl$spp, emp$spp_num)]
site_df2_gsl$site_name <- emp$site[match(site_df2_gsl$site, emp$site_num)]
aspp_df2_gsl$spp_name <- emp$latbi[match(aspp_df2_gsl$spp, emp$spp_num)]

# SOS 
sigma_df2_sos  <- read.csv("output/GM_SOSparam_sigma.csv")
bspp_df2_sos   <- read.csv("output/GM_SOSparam_bspp.csv")
treeid_df2_sos <- read.csv("output/GM_SOSparam_treeid.csv")
aspp_df2_sos   <- read.csv("output/GM_SOSparam_aspp.csv")
site_df2_sos   <- read.csv("output/GM_SOSparam_site.csv")

treeid_df2_sos$treeid <- as.numeric(treeid_df2_sos$treeid)
treeid_df2_sos$treeid_name <- emp$treeid[match(treeid_df2_sos$treeid, emp$treeid_num)]
bspp_df2_sos$spp_name <- emp$latbi[match(bspp_df2_sos$spp, emp$spp_num)]
site_df2_sos$site_name <- emp$site[match(site_df2_sos$site, emp$site_num)]
aspp_df2_sos$spp_name <- emp$latbi[match(aspp_df2_sos$spp, emp$spp_num)]

# EOS
sigma_df2_eos  <- read.csv("output/GM_EOSparam_sigma.csv")
bspp_df2_eos   <- read.csv("output/GM_EOSparam_bspp.csv")
treeid_df2_eos <- read.csv("output/GM_EOSparam_treeid.csv")
aspp_df2_eos   <- read.csv("output/GM_EOSparam_aspp.csv")
site_df2_eos   <- read.csv("output/GM_EOSparam_site.csv")

treeid_df2_eos$treeid <- as.numeric(treeid_df2_eos$treeid)
treeid_df2_eos$treeid_name <- emp$treeid[match(treeid_df2_eos$treeid, emp$treeid_num)]
bspp_df2_eos$spp_name <- emp$latbi[match(bspp_df2_eos$spp, emp$spp_num)]
site_df2_eos$site_name <- emp$site[match(site_df2_eos$site, emp$site_num)]
aspp_df2_eos$spp_name <- emp$latbi[match(aspp_df2_eos$spp, emp$spp_num)]

n_spp <- 4
y_pos <- rev(1:n_spp)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### bspp GDD EMPTY##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(file = "~/github/MSthesis/docs/defence/defenceFigures/bsppEmpty.jpeg", width = 2400, height = 1800, res = 400)
par(mfrow = c(1,1), mar = c(4, 3, 2, 1))

y_pos2 <- 2:3
bdum <- bspp_df2[2:3,]
bdum <- bdum[,2:6] - 0.02
plot(bdum$mean, y_pos2,
     xlim = c(-0.2, 0.3), ylim = c(0.5, n_spp + 0.5),
     xlab = "ring width change with season predictor", ylab = "",
     yaxt = "n", pch = 16, cex = 2.3, col = "#36454F", frame.plot = TRUE, 
     panel.first = abline(v = 0, lty = 2, col = "#36454F"), 
     cex.axis = 1.2, cex.lab = 1.2, yaxt = "n",)
segments(bdum$p5,  y_pos2, bdum$p95, y_pos2, col = "#36454F", lwd = 1.5)
segments(bdum$p25, y_pos2, bdum$p75, y_pos2, col = "#36454F", lwd = 3)
arrows(x0 = 0.03, y0 = n_spp + 0.85, x1 = 0.3, y1 = n_spp + 0.85, length = 0.25, xpd = TRUE)
text(0.17, n_spp + 0.98, "More growth", pos = 2, xpd = TRUE, cex = 1.2)
arrows(x0 = -0.03, y0 = n_spp + 0.85, x1 = -0.2, y1 = n_spp + 0.85, length = 0.25, xpd = TRUE)
text(-0.03, n_spp + 0.98, "Less growth", pos = 2, xpd = TRUE, cex = 1.2)
dev.off()
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### bspp GDD ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(file = "~/github/MSthesis/docs/defence/defenceFigures/muGDD.jpeg", 
     width = 2400, height = 1800, res = 400)
par(mfrow = c(1,1), mar = c(4, 3, 2, 1))
plot(bspp_df2$mean, y_pos,
     xlim = c(-0.2, 0.3), ylim = c(0.5, n_spp + 0.5),
     xlab = "ring width change with warmer thermal seasons", ylab = "",
     yaxt = "n", pch = 16, cex = 2.3, col = wccolslatbi, frame.plot = TRUE, 
     panel.first = abline(v = 0, lty = 2, col = "black"), 
     cex.axis = 1.2, cex.lab = 1.2, yaxt = "n",)
segments(bspp_df2$p5,  y_pos, bspp_df2$p95, y_pos, col = wccolslatbi, lwd = 1.5)
segments(bspp_df2$p25, y_pos, bspp_df2$p75, y_pos, col = wccolslatbi, lwd = 3)
text(x = bspp_df2$p5[1], y = y_pos,
     labels = parse(text = paste0("italic('", bspp_df2$spp_name, "')")),
     col = wccolslatbi, adj = c(+1.1, 0.5), cex = 1.1, xpd = NA)

usr <- par("usr")
xrange <- diff(usr[1:2])
yrange <- diff(usr[3:4])
w <- xrange * 0.20
h <- yrange * 0.30
cx <- usr[1]
cy <- usr[4] - yrange * 0.05

rasterImage(
  img_thermom,
  cx - w/2,
  cy - h/2,
  cx + w/2,
  cy + h/2,
  xpd = NA
)

arrows(x0 = 0.03, y0 = n_spp + 0.85, x1 = 0.3, y1 = n_spp + 0.85, length = 0.25, xpd = TRUE)
text(0.17, n_spp + 0.98, "More growth", pos = 2, xpd = TRUE, cex = 1.2)
arrows(x0 = -0.03, y0 = n_spp + 0.85, x1 = -0.17, y1 = n_spp + 0.85, length = 0.25, xpd = TRUE)
text(-0.02, n_spp + 0.98, "Less growth", pos = 2, xpd = TRUE, cex = 1.2)
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### bspp GSL ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(file = "~/github/MSthesis/docs/defence/defenceFigures/muGSL.jpeg", 
     width = 2400, height = 1800, res = 400)
par(mfrow = c(1,1), mar = c(4, 3, 2, 1))
plot(bspp_df2_gsl$mean, y_pos,
     xlim = c(-0.2, 0.3), ylim = c(0.5, n_spp + 0.5),
     xlab = "ring width change with longer calendar seasons", ylab = "",
     yaxt = "n", pch = 16, cex = 2.3, col = wccolslatbi, frame.plot = TRUE, 
     panel.first = abline(v = 0, lty = 2, col = "black"), 
     cex.axis = 1.2, cex.lab = 1.2, yaxt = "n",)
segments(bspp_df2_gsl$p5,  y_pos, bspp_df2_gsl$p95, y_pos, 
         col = wccolslatbi, lwd = 1.5)
segments(bspp_df2_gsl$p25, y_pos, bspp_df2_gsl$p75, y_pos, 
         col = wccolslatbi, lwd = 3)
text(x = bspp_df2_gsl$p5[2], y = y_pos,
     labels = parse(text = paste0("italic('", bspp_df2_gsl$spp_name, "')")),
     col = wccolslatbi, adj = c(+1.1, 0.5), cex = 1.1, xpd = NA)

usr <- par("usr")
xrange <- diff(usr[1:2])
yrange <- diff(usr[3:4])
w <- xrange * 0.20
h <- yrange * 0.30
cx <- usr[1]
cy <- usr[4] - yrange * 0.05

rasterImage(
  img_calenda,
  cx - w/2,
  cy - h/2,
  cx + w/2,
  cy + h/2,
  xpd = NA
)

arrows(x0 = 0.03, y0 = n_spp + 0.85, x1 = 0.3, y1 = n_spp + 0.85, length = 0.25, xpd = TRUE)
text(0.17, n_spp + 0.98, "More growth", pos = 2, xpd = TRUE, cex = 1.2)
arrows(x0 = -0.03, y0 = n_spp + 0.85, x1 = -0.17, y1 = n_spp + 0.85, length = 0.25, xpd = TRUE)
text(-0.02, n_spp + 0.98, "Less growth", pos = 2, xpd = TRUE, cex = 1.2)
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### bspp SOS ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(file = "~/github/MSthesis/docs/defence/defenceFigures/muSOS.jpeg", 
     width = 2400, height = 1800, res = 400)
par(mfrow = c(1,1), mar = c(4, 3, 2, 1))
plot(bspp_df2_sos$mean, y_pos,
     xlim = c(-0.32, 0.35), ylim = c(0.5, n_spp + 0.5),
     xlab = "ring width change with a later start of season", ylab = "",
     yaxt = "n", pch = 16, cex = 2.3, col = wccolslatbi, frame.plot = TRUE, 
     panel.first = abline(v = 0, lty = 2, col = "black"), 
     cex.axis = 1.2, cex.lab = 1.2, yaxt = "n",)
segments(bspp_df2_sos$p5,  y_pos, bspp_df2_sos$p95, y_pos, 
         col = wccolslatbi, lwd = 1.5)
segments(bspp_df2_sos$p25, y_pos, bspp_df2_sos$p75, y_pos, 
         col = wccolslatbi, lwd = 3)
text(x = bspp_df2_sos$p5[1], y = y_pos,
     labels = parse(text = paste0("italic('", bspp_df2_sos$spp_name, "')")),
     col = wccolslatbi, adj = c(+1.1, 0.5), cex = 1.1, xpd = NA)

usr <- par("usr")
xrange <- diff(usr[1:2])
yrange <- diff(usr[3:4])
w <- xrange * 0.20
h <- yrange * 0.30
cx <- usr[1]
cy <- usr[4] - yrange * 0.05

rasterImage(
  img_leafout,
  cx - w/2,
  cy - h/2,
  cx + w/2,
  cy + h/2,
  xpd = NA
)

arrows(x0 = 0.03, y0 = n_spp + 0.85, x1 = 0.3, y1 = n_spp + 0.85, length = 0.25, xpd = TRUE)
text(0.2, n_spp + 0.98, "More growth", pos = 2, xpd = TRUE, cex = 1.2)
arrows(x0 = -0.03, y0 = n_spp + 0.85, x1 = -0.25, y1 = n_spp + 0.85, length = 0.25, xpd = TRUE)
text(-0.02, n_spp + 0.98, "Less growth", pos = 2, xpd = TRUE, cex = 1.2)
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### bspp EOS ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(file = "~/github/MSthesis/docs/defence/defenceFigures/muEOS.jpeg", 
     width = 2400, height = 1800, res = 400)
par(mfrow = c(1,1), mar = c(4, 3, 2, 1))
plot(bspp_df2_eos$mean, y_pos,
     xlim = c(-0.32, 0.35), ylim = c(0.5, n_spp + 0.5),
     xlab = "ring width change with a later end of season", ylab = "",
     yaxt = "n", pch = 16, cex = 2.3, col = wccolslatbi, frame.plot = TRUE, 
     panel.first = abline(v = 0, lty = 2, col = "black"), 
     cex.axis = 1.2, cex.lab = 1.2, yaxt = "n",)
segments(bspp_df2_eos$p5,  y_pos, bspp_df2_eos$p95, y_pos, 
         col = wccolslatbi, lwd = 1.5)
segments(bspp_df2_eos$p25, y_pos, bspp_df2_eos$p75, y_pos, 
         col = wccolslatbi, lwd = 3)
text(x = bspp_df2_eos$p5[1], y = y_pos,
     labels = parse(text = paste0("italic('", bspp_df2_eos$spp_name, "')")),
     col = wccolslatbi, adj = c(+1.1, 0.5), cex = 1.1, xpd = NA)

usr <- par("usr")
xrange <- diff(usr[1:2])
yrange <- diff(usr[3:4])
w <- xrange * 0.20
h <- yrange * 0.30
cx <- usr[1]
cy <- usr[4] - yrange * 0.05

rasterImage(
  img_budset,
  cx - w/2,
  cy - h/2,
  cx + w/2,
  cy + h/2,
  xpd = NA
)

arrows(x0 = 0.03, y0 = n_spp + 0.85, x1 = 0.3, y1 = n_spp + 0.85, length = 0.25, xpd = TRUE)
text(0.2, n_spp + 0.98, "More growth", pos = 2, xpd = TRUE, cex = 1.2)
arrows(x0 = -0.03, y0 = n_spp + 0.85, x1 = -0.25, y1 = n_spp + 0.85, length = 0.25, xpd = TRUE)
text(-0.02, n_spp + 0.98, "Less growth", pos = 2, xpd = TRUE, cex = 1.2)

dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Treespotters plots####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
setwd("/Users/christophe_rouleau-desrochers/github/coringtreespotters/analyses")
source("rcode/TSgrowthModelsMain.R")

# rasterized pictograms
img_thermom <- rsvg::rsvg("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/figures/pictogramsLeaves/thermometer.svg")
img_calenda <- rsvg::rsvg("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/figures/pictogramsLeaves/calendar.svg")
img_leafout <- rsvg::rsvg("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/figures/pictogramsLeaves/bepaPicLeafout.svg")
img_budset  <- rsvg::rsvg("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses/figures/pictogramsLeaves/bepaPicBudset.svg")

sigma_df2  <- read.csv("output/GM_GDDparam_sigma_noPP.csv")
bspp_df2   <- read.csv("output/GM_GDDparam_bspp_noPP.csv")
treeid_df2 <- read.csv("output/GM_GDDparam_treeid_noPP.csv")
aspp_df2   <- read.csv("output/GM_GDDparam_aspp_noPP.csv")

treeid_df2$id <- as.numeric(treeid_df2$id)  
treeid_df2$id_name <- empts$id[match(treeid_df2$id, empts$id_num)]
bspp_df2$spp_name <- empts$latbi[match(bspp_df2$spp, empts$spp_num)]
aspp_df2$spp_name <- empts$latbi[match(aspp_df2$spp, empts$spp_num)]

# GSL
sigma_df2_gsl  <- read.csv("output/GM_GSLparam_sigma.csv")
bspp_df2_gsl   <- read.csv("output/GM_GSLparam_bspp.csv")
treeid_df2_gsl <- read.csv("output/GM_GSLparam_treeid.csv")
aspp_df2_gsl   <- read.csv("output/GM_GSLparam_aspp.csv")

treeid_df2_gsl$id <- as.numeric(treeid_df2_gsl$id)
treeid_df2_gsl$id_name <- empts$id[match(treeid_df2_gsl$id, empts$id_num)]
bspp_df2_gsl$spp_name <- empts$latbi[match(bspp_df2_gsl$spp, empts$spp_num)]
aspp_df2_gsl$spp_name <- empts$latbi[match(aspp_df2_gsl$spp, empts$spp_num)]

# SOS 
sigma_df2_sos  <- read.csv("output/GM_SOSparam_sigma.csv")
bspp_df2_sos   <- read.csv("output/GM_SOSparam_bspp.csv")
treeid_df2_sos <- read.csv("output/GM_SOSparam_treeid.csv")
aspp_df2_sos   <- read.csv("output/GM_SOSparam_aspp.csv")

treeid_df2_sos$id <- as.numeric(treeid_df2_sos$id)
treeid_df2_sos$id_name <- empts$id[match(treeid_df2_sos$id, empts$id_num)]
bspp_df2_sos$spp_name <- empts$latbi[match(bspp_df2_sos$spp, empts$spp_num)]
aspp_df2_sos$spp_name <- empts$latbi[match(aspp_df2_sos$spp, empts$spp_num)]

# EOS
sigma_df2_eos  <- read.csv("output/GM_EOSparam_sigma.csv")
bspp_df2_eos   <- read.csv("output/GM_EOSparam_bspp.csv")
treeid_df2_eos <- read.csv("output/GM_EOSparam_treeid.csv")
aspp_df2_eos   <- read.csv("output/GM_EOSparam_aspp.csv")

treeid_df2_eos$id <- as.numeric(treeid_df2_eos$id)
treeid_df2_eos$id_name <- empts$id[match(treeid_df2_eos$id, empts$id_num)]
bspp_df2_eos$spp_name <- empts$latbi[match(bspp_df2_eos$spp, empts$spp_num)]
aspp_df2_eos$spp_name <- empts$latbi[match(aspp_df2_eos$spp, empts$spp_num)]

n_spp <- 11
y_pos <- rev(1:n_spp)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### bspp GDD ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(file = "~/github/MSthesis/docs/defence/defenceFigures/TSmuGDD.jpeg", 
     width = 2400, height = 1800, res = 400)
par(mfrow = c(1,1), mar = c(4, 3, 2, 1))
plot(bspp_df2$mean, y_pos,
     xlim = c(-0.3, 0.3), ylim = c(0.5, n_spp + 0.5),
     xlab = "ring width change with warmer thermal seasons", ylab = "",
     yaxt = "n", pch = 16, cex = 2.3, col = tscolslatbi, frame.plot = TRUE, 
     panel.first = abline(v = 0, lty = 2, col = "black"), 
     cex.axis = 1.2, cex.lab = 1.2, yaxt = "n",)
segments(bspp_df2$p5,  y_pos, bspp_df2$p95, y_pos, col = tscolslatbi, lwd = 1.5)
segments(bspp_df2$p25, y_pos, bspp_df2$p75, y_pos, col = tscolslatbi, lwd = 3)
text(x = bspp_df2$p5[4], y = y_pos,
     labels = parse(text = paste0("italic('", bspp_df2$spp_name, "')")),
     col = tscolslatbi, adj = c(+1.1, 0.5), cex = 1.1, xpd = NA)

usr <- par("usr")
xrange <- diff(usr[1:2])
yrange <- diff(usr[3:4])
w <- xrange * 0.20
h <- yrange * 0.30
cx <- usr[1]
cy <- usr[4] - yrange * 0.05

rasterImage(
  img_thermom,
  cx - w/2,
  cy - h/2,
  cx + w/2,
  cy + h/2,
  xpd = NA
)

arrows(x0 = 0.03, y0 = n_spp + 1.4, x1 = 0.3, y1 = n_spp + 1.4, length = 0.25, xpd = TRUE)
text(0.17, n_spp + 1.8, "More growth", pos = 2, xpd = TRUE, cex = 1.2)
arrows(x0 = -0.03, y0 = n_spp + 1.4, x1 = -0.25, y1 = n_spp + 1.4, length = 0.25, xpd = TRUE)
text(-0.02, n_spp + 1.8, "Less growth", pos = 2, xpd = TRUE, cex = 1.2)
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### bspp GSL ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(file = "~/github/MSthesis/docs/defence/defenceFigures/TSmuGSL.jpeg", 
     width = 2400, height = 1800, res = 400)
par(mfrow = c(1,1), mar = c(4, 3, 2, 1))
plot(bspp_df2_gsl$mean, y_pos,
     xlim = c(-0.3, 0.3), ylim = c(0.5, n_spp + 0.5),
     xlab = "ring width change with longer calendar seasons", ylab = "",
     yaxt = "n", pch = 16, cex = 2.3, col = tscolslatbi, frame.plot = TRUE, 
     panel.first = abline(v = 0, lty = 2, col = "black"), 
     cex.axis = 1.2, cex.lab = 1.2, yaxt = "n",)
segments(bspp_df2_gsl$p5,  y_pos, bspp_df2_gsl$p95, y_pos, 
         col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_gsl$p25, y_pos, bspp_df2_gsl$p75, y_pos, 
         col = tscolslatbi, lwd = 3)
text(x = bspp_df2_gsl$p5[4], y = y_pos,
     labels = parse(text = paste0("italic('", bspp_df2_gsl$spp_name, "')")),
     col = tscolslatbi, adj = c(+1.1, 0.5), cex = 1.1, xpd = NA)

usr <- par("usr")
xrange <- diff(usr[1:2])
yrange <- diff(usr[3:4])
w <- xrange * 0.20
h <- yrange * 0.30
cx <- usr[1]
cy <- usr[4] - yrange * 0.05

rasterImage(
  img_calenda,
  cx - w/2,
  cy - h/2,
  cx + w/2,
  cy + h/2,
  xpd = NA
)

arrows(x0 = 0.03, y0 = n_spp + 1.4, x1 = 0.3, y1 = n_spp + 1.4, length = 0.25, xpd = TRUE)
text(0.17, n_spp + 1.8, "More growth", pos = 2, xpd = TRUE, cex = 1.2)
arrows(x0 = -0.03, y0 = n_spp + 1.4, x1 = -0.25, y1 = n_spp + 1.4, length = 0.25, xpd = TRUE)
text(-0.02, n_spp + 1.8, "Less growth", pos = 2, xpd = TRUE, cex = 1.2)
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### bspp SOS ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(file = "~/github/MSthesis/docs/defence/defenceFigures/TSmuSOS.jpeg", 
     width = 2400, height = 1800, res = 400)
par(mfrow = c(1,1), mar = c(4, 3, 2, 1))
plot(bspp_df2_sos$mean, y_pos,
     xlim = c(-0.7, 0.7), ylim = c(0.5, n_spp + 0.5),
     xlab = "ring width change with a later start of season", ylab = "",
     yaxt = "n", pch = 16, cex = 2.3, col = tscolslatbi, frame.plot = TRUE, 
     panel.first = abline(v = 0, lty = 2, col = "black"), 
     cex.axis = 1.2, cex.lab = 1.2, yaxt = "n",)
segments(bspp_df2_sos$p5,  y_pos, bspp_df2_sos$p95, y_pos, 
         col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_sos$p25, y_pos, bspp_df2_sos$p75, y_pos, 
         col = tscolslatbi, lwd = 3)
text(x = bspp_df2_sos$p5[1], y = y_pos,
     labels = parse(text = paste0("italic('", bspp_df2_sos$spp_name, "')")),
     col = tscolslatbi, adj = c(+1.1, 0.5), cex = 1.1, xpd = NA)

usr <- par("usr")
xrange <- diff(usr[1:2])
yrange <- diff(usr[3:4])
w <- xrange * 0.20
h <- yrange * 0.30
cx <- usr[1]
cy <- usr[4] - yrange * 0.05

rasterImage(
  img_leafout,
  cx - w/2,
  cy - h/2,
  cx + w/2,
  cy + h/2,
  xpd = NA
)

arrows(x0 = 0.07, y0 = n_spp + 1.4, x1 = 0.7, y1 = n_spp + 1.4, length = 0.25, xpd = TRUE)
text(0.4, n_spp + 1.8, "More growth", pos = 2, xpd = TRUE, cex = 1.2)
arrows(x0 = -0.07, y0 = n_spp + 1.4, x1 = -0.6, y1 = n_spp + 1.4, length = 0.25, xpd = TRUE)
text(-0.05, n_spp + 1.8, "Less growth", pos = 2, xpd = TRUE, cex = 1.2)
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### bspp EOS ##### 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg(file = "~/github/MSthesis/docs/defence/defenceFigures/TSmuEOS.jpeg", 
     width = 2400, height = 1800, res = 400)
par(mfrow = c(1,1), mar = c(4, 3, 2, 1))
plot(bspp_df2_eos$mean, y_pos,
     xlim = c(-0.4, 0.4), ylim = c(0.5, n_spp + 0.5),
     xlab = "ring width change with a later end of season", ylab = "",
     yaxt = "n", pch = 16, cex = 2.3, col = tscolslatbi, frame.plot = TRUE, 
     panel.first = abline(v = 0, lty = 2, col = "black"), 
     cex.axis = 1.2, cex.lab = 1.2, yaxt = "n",)
segments(bspp_df2_eos$p5,  y_pos, bspp_df2_eos$p95, y_pos, 
         col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_eos$p25, y_pos, bspp_df2_eos$p75, y_pos, 
         col = tscolslatbi, lwd = 3)
text(x = bspp_df2_eos$p5[4], y = y_pos,
     labels = parse(text = paste0("italic('", bspp_df2_eos$spp_name, "')")),
     col = tscolslatbi, adj = c(+1.1, 0.5), cex = 1.1, xpd = NA)

usr <- par("usr")
xrange <- diff(usr[1:2])
yrange <- diff(usr[3:4])
w <- xrange * 0.20
h <- yrange * 0.30
cx <- usr[1]
cy <- usr[4] - yrange * 0.05

rasterImage(
  img_budset,
  cx - w/2,
  cy - h/2,
  cx + w/2,
  cy + h/2,
  xpd = NA
)

arrows(x0 = 0.04, y0 = n_spp + 1.4, x1 = 0.4, y1 = n_spp + 1.4, length = 0.25, xpd = TRUE)
text(0.22, n_spp + 1.8, "More growth", pos = 2, xpd = TRUE, cex = 1.2)
arrows(x0 = -0.04, y0 = n_spp + 1.4, x1 = -0.35, y1 = n_spp + 1.4, length = 0.25, xpd = TRUE)
text(-0.03, n_spp + 1.8, "Less growth", pos = 2, xpd = TRUE, cex = 1.2)
dev.off()
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Previous year model #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
bspp_df2_ts_curr  <- read.csv("output/GM_GDDparam_bspp_prvsYr.csv")
bspp_df2_ts_prvs  <- read.csv("output/GM_GDDparam_bsppYr_prvsYr.csv")

bspp_df2_ts_curr$spp_name <- empts$latbi[match(bspp_df2_ts_curr$spp, empts$spp_num)]
bspp_df2_ts_prvs$spp_name <- empts$latbi[match(bspp_df2_ts_prvs$spp, empts$spp_num)]

jpeg("~/github/MSthesis/docs/defence/defenceFigures/bsppCurrentVSpreviousYR.jpeg",
     width = 3600, height = 1800, res = 400)
par(mfrow = c(1,2), mar = c(4, 2, 2, 1))

# Row 2: Previous year
plot(bspp_df2_ts_prvs$mean, y_pos,
     xlim = c(-1, 0.4), ylim = c(0.5, n_spp + 0.5), 
     xlab = "ring width change with previous year thermal season", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_prvs$p5,  y_pos, bspp_df2_ts_prvs$p95, y_pos,
         col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_ts_prvs$p25, y_pos, bspp_df2_ts_prvs$p75, y_pos,
         col = tscolslatbi, lwd = 3)
mtext("Previous year", side = 3, adj = 0, line = 0.2, font = 2, cex = 2)
text(x = bspp_df2_ts_prvs$p5[9], y = y_pos,
     labels = parse(text = paste0("italic('", bspp_df2$spp_name, "')")),
     col = tscolslatbi, adj = c(+1.2, 0.5), cex = 1.1, xpd = NA)

# Current year
plot(bspp_df2_ts_curr$mean, y_pos,
     xlim = c(-1, 0.4), ylim = c(0.5, n_spp + 0.5), 
     xlab = "ring width change with current year thermal season", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi, frame.plot = TRUE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_curr$p5,  y_pos, bspp_df2_ts_curr$p95, y_pos,
         col = tscolslatbi, lwd = 1.5)
segments(bspp_df2_ts_curr$p25, y_pos, bspp_df2_ts_curr$p75, y_pos,
         col = tscolslatbi, lwd = 3)
mtext("Current year", side = 3, adj = 0, line = 0.2, font = 2, cex = 2)
text(x = bspp_df2_ts_curr$p5[9], y = y_pos,
     labels = parse(text = paste0("italic('", bspp_df2$spp_name, "')")),
     col = tscolslatbi, adj = c(+1.2, 0.5), cex = 1.1, xpd = NA)

dev.off()


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Previous year model with transparent species#####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
jpeg("~/github/MSthesis/docs/defence/defenceFigures/bsppCurrentVSpreviousYR2.jpeg",
     width = 3600, height = 1800, res = 400)
par(mfrow = c(1,2), mar = c(4, 2, 2, 1))

# Transparent all of speceis except the ones that don't cross 0
is_highlight <- bspp_df2_ts_prvs$spp_name %in% c("B. alleghaniensis", "B. nigra")
tscolslatbi2 <- ifelse(is_highlight, tscolslatbi, adjustcolor(tscolslatbi, alpha.f = 0.1))

# Row 2: Previous year
plot(bspp_df2_ts_prvs$mean, y_pos,
     xlim = c(-1, 0.4), ylim = c(0.5, n_spp + 0.5), 
     xlab = "ring width change with previous year thermal season", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi2, frame.plot = TRUE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_prvs$p5,  y_pos, bspp_df2_ts_prvs$p95, y_pos,
         col = tscolslatbi2, lwd = 1.5)
segments(bspp_df2_ts_prvs$p25, y_pos, bspp_df2_ts_prvs$p75, y_pos,
         col = tscolslatbi2, lwd = 3)
mtext("Previous year", side = 3, adj = 0, line = 0.2, font = 2, cex = 2)
text(x = bspp_df2_ts_prvs$p5[9], y = y_pos,
     labels = parse(text = paste0("italic('", bspp_df2$spp_name, "')")),
     col = tscolslatbi2, adj = c(+1.2, 0.5), cex = 1.1, xpd = NA)

# Current year
plot(bspp_df2_ts_curr$mean, y_pos,
     xlim = c(-1, 0.4), ylim = c(0.5, n_spp + 0.5), 
     xlab = "ring width change with current year thermal season", ylab = "",
     yaxt = "n", pch = 16, cex = 2, col = tscolslatbi2, frame.plot = TRUE,
     panel.first = abline(v = 0, lty = 2, col = "black"))
segments(bspp_df2_ts_curr$p5,  y_pos, bspp_df2_ts_curr$p95, y_pos,
         col = tscolslatbi2, lwd = 1.5)
segments(bspp_df2_ts_curr$p25, y_pos, bspp_df2_ts_curr$p75, y_pos,
         col = tscolslatbi2, lwd = 3)
mtext("Current year", side = 3, adj = 0, line = 0.2, font = 2, cex = 2)
text(x = bspp_df2_ts_curr$p5[9], y = y_pos,
     labels = parse(text = paste0("italic('", bspp_df2$spp_name, "')")),
     col = tscolslatbi2, adj = c(+1.2, 0.5), cex = 1.1, xpd = NA)

dev.off()


