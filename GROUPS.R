## GET PACKAGEplot
library (Momocs)


##GET AND READ FILES
getwd()
lf <- list.files ("EarlyandMidTsubo", full.names=TRUE)
lf
coo <- import_jpg(lf, auto.notcentered = TRUE, fun.notcentered = NULL, threshold = 0.9)
dat <- Out(coo)

##ADD CLASSIFIERS
dat$fac <- read.csv("jamesrdatfacallearlyandmiddletsubo.csv")

##EDIT DATA CLASS
dat$fac <- edit(dat$fac)
type.fac <- as.factor(dat$type)
dat$fac <- data.frame (ld = dat$ld, type = type.fac, site = dat$site)

##ELIPTICAL FOURIER TRANSFORM
dat.f <- efourier (dat, 11, norm = FALSE)

##PCA
dat.PCA <- PCA(dat.f)

##LDA
dat.LDA <- LDA(dat.PCA, "type")

##DISPLAY ALL IMAGES
panel(dat, fac='type', palette=col_spring, names=TRUE)
stack (dat, fac='type')


dat %>%
  Out() %>%
  coo_center() %>%
  coo_scale() %>%
  coo_slidedirection("right") %>%
  coo_untiltx() %>%
  efourier(nb.h = 11, norm = FALSE) %>%
  PCA() %>%
  
  plot_PCA(morphospace = FALSE, zoom = 1, labelpoints = TRUE, points = TRUE, ~type, box = TRUE) %>%
  layer_morphospace_PCA(
    position = c("xy")[1],
    size = 0.5,
    draw = TRUE) %>%
  layer_points(pch = 20, cex = 2) %>%
  layer_axes(col = "#999999", lwd = 1/2) %>%

CLUST (dat.PCA, palette = col_solarized, "type", dist_method="euclidean", hclust_method = "ward.D2", type = "vertical")



## GET PACKAGEplot
library (Momocs)

##GET AND READ FILES
getwd()
lf <- list.files ("EarlyandMidTsubo", full.names=TRUE)
lf
coo <- import_jpg(lf, auto.notcentered = TRUE, fun.notcentered = NULL, threshold = 0.5)
dat <- Out(coo)

##ADD CLASSIFIERS
dat$fac <- read.csv("jamesrdatfacallearlyandmiddletsubo.csv")

##EDIT DATA CLASS(IF NEEDED)
#Insert Name column as "ld"
#Insert type column as "type"
#Insert site column as "site"
dat$fac <- edit(dat$fac)
type.fac <- as.factor(dat$type)
dat$fac <- data.frame (ld = dat$ld, type = type.fac, site = dat$site)


##CHECK DATA IS CORRECT
#SHOULD BE AN "Out (outlines)" WITH RIGHT NUMBER OF OUTLINES AND CLASSIFIERS
dat
ori <- dat

##ELIPTICAL FOURIER TRANSFORM, 60 HARMONICS
dat.f <- efourier (dat, 12, norm = TRUE)

##CHECK FOURIER
#SHOULD BE AN "OutCoe" WITH SAME NUMBER OF OUTLINES AND CLASSIFIERS
dat.f

##PCA
dat.PCA <- PCA(dat.f)

##CHECK PCA
#SHOULD BE A "PCA object"
dat.PCA

##LDA (REQUIRES MORE THAN ONE GROUP IN A CLASS)
dat.LDA <- LDA(dat.PCA, "type")

##CHECK LDA
dat.LDA

##LIST TYPES CORRECTED BY LDA
dat.LDAC <- data.frame(Name = dat$ld, New_Type = dat.LDA$CV.fac, Original_Type = dat$type)
dat.LDAC



###TRY TO ONLY RUN THE ABOVE LINES THIS ONCE PER SESSION

plot (coo)



##DISPLAY ALL IMAGES
panel(dat, fac='type', palette=col_spring, names=TRUE)

stack (dat)

##DISPLAY ALL IMAGES STACKED
dat %>% 
  	coo_center %>% 
	coo_slidedirection("N") %T>% 
	print() %>% stack()

##DISPLAY ALL IMAGES STACKED CENTRED, same size
dat %>% 
  	coo_center %>% coo_scale %>%
	coo_slidedirection("N") %T>% 
	print() %>% stack()

## DISPLAY PCA NO GROUPS
plot(dat.PCA, xax = 1, yax = 2, points = FALSE,
color = "#000000", pch = 20, cex = 2, palette = pal_alpha,
center.origin = FALSE, zoom = 1, xlim = NULL, ylim = NULL,
bg = par("bg"), grid = TRUE, nb.grids = 3, morphospace = FALSE,
pos.shp = c("range", "full", "circle", "xy", "range_axes", "full_axes")[1],
amp.shp = 1, size.shp = 1, nb.shp = 12, nr.shp = 6, nc.shp = 5,
rotate.shp = 0, flipx.shp = FALSE, flipy.shp = FALSE, pts.shp = 60,
border.shp = col_alpha("#000000", 0.5), lwd.shp = 1,
col.shp = col_alpha("#000000", 0.95), stars = FALSE, ellipses = FALSE,
conf.ellipses = 0.5, ellipsesax = FALSE, conf.ellipsesax = c(0.5, 0.9),
lty.ellipsesax = 1, lwd.ellipsesax = sqrt(2), chull = FALSE,
chull.lty = 1, chull.filled = TRUE, chull.filled.alpha = 0.92,
density = FALSE, lev.density = 20, contour = FALSE, lev.contour = 3,
n.kde2d = 100, delaunay = FALSE, loadings = FALSE,
labelspoints = TRUE, col.labelspoints = par("fg"),
cex.labelspoints = .7, abbreviate.labelspoints = TRUE,
labelsgroups = TRUE, cex.labelsgroups = 0.8, rect.labelsgroups = FALSE,
abbreviate.labelsgroups = FALSE, color.legend = TRUE, axisnames = TRUE,
axisvar = TRUE, unit = FALSE, eigen = TRUE, rug = TRUE,
title = substitute(TOP), box = TRUE, old.par = TRUE) 

## DISPLAY PCA NO GROUPS YES MORPH
plot(dat.PCA, xax = 1, yax = 2, points = FALSE,
color = "#000000", pch = 20, cex = .5, palette = pal_alpha,
center.origin = FALSE, zoom = 1, xlim = NULL, ylim = NULL,
bg = par("bg"), grid = TRUE, nb.grids = 3, morphospace = TRUE,
pos.shp = c("range", "full", "circle", "xy", "range_axes", "full_axes")[1],
amp.shp = 1, size.shp = 1, nb.shp = 12, nr.shp = 6, nc.shp = 5,
rotate.shp = 0, flipx.shp = FALSE, flipy.shp = FALSE, pts.shp = 60,
border.shp = col_alpha("#000000", 0.5), lwd.shp = 1,
col.shp = col_alpha("#000000", 0.95), stars = FALSE, ellipses = FALSE,
conf.ellipses = 0.5, ellipsesax = FALSE, conf.ellipsesax = c(0.5, 0.9),
lty.ellipsesax = 1, lwd.ellipsesax = sqrt(2), chull = FALSE,
chull.lty = 1, chull.filled = TRUE, chull.filled.alpha = 0.92,
density = FALSE, lev.density = 20, contour = FALSE, lev.contour = 3,
n.kde2d = 100, delaunay = FALSE, loadings = FALSE,
labelspoints = TRUE, col.labelspoints = par("fg"),
cex.labelspoints = .7, abbreviate.labelspoints = TRUE,
labelsgroups = TRUE, cex.labelsgroups = 0.8, rect.labelsgroups = FALSE,
abbreviate.labelsgroups = FALSE, color.legend = TRUE, axisnames = TRUE,
axisvar = TRUE, unit = FALSE, eigen = TRUE, rug = TRUE,
title = substitute( ), box = TRUE, old.par = TRUE) 

## DISPLAY PCA YES GROUPS
plot(dat.PCA, ~type, xax = 1, yax = 2, points = TRUE,
color = "#000000", pch = 20, cex = 2, palette = col_spring,
center.origin = FALSE, zoom = 1, xlim = NULL, ylim = NULL,
bg = par("bg"), grid = TRUE, nb.grids = 3, morphospace = TRUE,
pos.shp = c("range", "full", "circle", "xy", "range_axes", "full_axes")[1],
amp.shp = 1, size.shp = 1, nb.shp = 12, nr.shp = 6, nc.shp = 5,
rotate.shp = 0, flipx.shp = FALSE, flipy.shp = FALSE, pts.shp = 60,
border.shp = col_alpha("#000000"), lwd.shp = 1,
col.shp = col_alpha("#000000"), stars = FALSE, ellipses = FALSE,
conf.ellipses = 0.5, ellipsesax = FALSE, conf.ellipsesax = c(0.5, 0.9),
lty.ellipsesax = 1, lwd.ellipsesax = sqrt(2), chull = FALSE,
chull.lty = 1, chull.filled = TRUE, chull.filled.alpha = 0.92,
density = FALSE, lev.density = 20, contour = FALSE, lev.contour = 3,
n.kde2d = 100, delaunay = FALSE, loadings = FALSE,
labelspoints = TRUE, col.labelspoints = par("fg"),
cex.labelspoints = 0.6, abbreviate.labelspoints = TRUE,
labelsgroups = TRUE, cex.labelsgroups = 0.8, rect.labelsgroups = FALSE,
abbreviate.labelsgroups = FALSE, color.legend = TRUE, axisnames = TRUE,
axisvar = TRUE, unit = FALSE, eigen = TRUE, rug = TRUE,
title = substitute( ), box = TRUE, old.par = TRUE) 

## DISPLAY PCA YES GROUPS NO MORPHO
plot(dat.PCA, ~type, xax = 1, yax = 2, points = TRUE,
color = "#000000", pch = 20, cex = 2, palette = col_solarized,
center.origin = FALSE, zoom = 1, xlim = NULL, ylim = NULL,
bg = par("bg"), grid = TRUE, nb.grids = 3, morphospace = FALSE,
pos.shp = c("range", "full", "circle", "xy", "range_axes", "full_axes")[1],
amp.shp = 1, size.shp = 1, nb.shp = 12, nr.shp = 6, nc.shp = 5,
rotate.shp = 0, flipx.shp = FALSE, flipy.shp = FALSE, pts.shp = 60,
border.shp = col_alpha("#000000"), lwd.shp = 1,
col.shp = col_alpha("#000000"), stars = FALSE, ellipses = FALSE,
conf.ellipses = 90, ellipsesax = FALSE, conf.ellipsesax = c(0.5, 0.9),
lty.ellipsesax = 0, lwd.ellipsesax = sqrt(0), chull = FALSE,
chull.lty = 1, chull.filled = TRUE, chull.filled.alpha = 0.92,
density = FALSE, lev.density = 50, contour = FALSE, lev.contour = 10,
n.kde2d = 100, delaunay = FALSE, loadings = FALSE,
labelspoints = FALSE, col.labelspoints = par("fg"),
cex.labelspoints = 0.6, abbreviate.labelspoints = TRUE,
labelsgroups = TRUE, cex.labelsgroups = 0.8, rect.labelsgroups = FALSE,
abbreviate.labelsgroups = FALSE, color.legend = TRUE, axisnames = TRUE,
axisvar = FALSE, unit = FALSE, eigen = FALSE, rug = TRUE,
title = substitute( ), box = TRUE, old.par = TRUE) 

## DISPLAY PCA YES GROUPS NO MORPHO
plot(dat.PCA, ~type, xax = 1, yax = 2, points = TRUE,
     color = "#000000", pch = 20, cex = 2, palette = col_solarized,
     center.origin = FALSE, zoom = 1, xlim = NULL, ylim = NULL,
     bg = par("bg"), grid = TRUE, nb.grids = 3, morphospace = TRUE,
     pos.shp = c("range", "full", "circle", "xy", "range_axes", "full_axes")[1],
     amp.shp = 1, size.shp = 1, nb.shp = 12, nr.shp = 6, nc.shp = 5,
     rotate.shp = 0, flipx.shp = FALSE, flipy.shp = FALSE, pts.shp = 60,
     border.shp = col_alpha("#000000"), lwd.shp = 1,
     col.shp = col_alpha("#000000"), stars = FALSE, ellipses = FALSE,
     conf.ellipses = 90, ellipsesax = FALSE, conf.ellipsesax = c(0.5, 0.9),
     lty.ellipsesax = 0, lwd.ellipsesax = sqrt(0), chull = FALSE,
     chull.lty = 0, chull.filled = FALSE, chull.filled.alpha = 0.92,
     density = TRUE, lev.density = 50, contour = TRUE, lev.contour = 5,
     n.kde2d = 300, delaunay = FALSE, loadings = FALSE,
     labelspoints = FALSE, col.labelspoints = par("fg"),
     cex.labelspoints = 0.6, abbreviate.labelspoints = TRUE,
     labelsgroups = TRUE, cex.labelsgroups = 0.8, rect.labelsgroups = FALSE,
     abbreviate.labelsgroups = FALSE, color.legend = TRUE, axisnames = TRUE,
     axisvar = FALSE, unit = FALSE, eigen = FALSE, rug = TRUE,
     title = substitute( ), box = TRUE, old.par = TRUE) 



##DISPLAY PHYLOGRAM
CLUST (dat.f,dist_method="euclidean",type = "fan")

##DISPLAY PHYLOGRAM
CLUST (dat.f, palette = col_solarized, dat.f$type, dist_method="euclidean", type = "vertical")


##BOXPLOT OF PCs
boxplot (dat.PCA, dat.f$type, nax = 1:3)
boxplot (dat.PCA, dat.f$type)
p <- boxplot(dat.PCA, 1)

##DISPLAY HARMONIC POWER
ef <- efourier(dat[1], 24)
rf <- efourier(dat[1], 24)
harm_pow(ef)
harm_pow(rf)
plot(cumsum(harm_pow(ef)[-1]), type='o',
     main='Cumulated harmonic power without the first harmonic',
     ylab='Cumulated harmonic power', xlab='Harmonic rank')



##DISPLAY elliptical Fourier transforms
coo_oscillo(dat[1], "efourier")
b5 <- dat %>% slice(1:5)
b5  %>% calibrate_harmonicpower_efourier(nb.h=12)
#> $gg




##TPS GRID
dat.f <- efourier(dat)
x <- MSHAPES(dat.f, 'type', nb.pts=80)$shp
fr <- x$stage1
to <- x$stage3
tps_grid(fr, to, amp=3, grid.size=10)


##TPS ISO
dat.f <- efourier(dat)
x <- MSHAPES(dat.f, 'type', nb.pts=80)$shp
fr <- x$coastal
to <- x$plains
tps_iso(fr, to, iso.nb=200, amp=3)
tps_iso(fr, to, iso.nb=200, amp=3, grid=TRUE)


##TPS ARROW
dat.f <- efourier(dat)
x <- MSHAPES(dat.f, 'type', nb.pts=80)$shp
fr <- x$stage1
to <- x$stage999
tps_arr(fr, to, arr.nb=500, palette=col_sari, amp=3)
tps_arr(fr, to, arr.nb=200, palette=col_sari, amp=3, grid=FALSE)



##MANOVA
MANOVA(dat.f, 'type', test='Pillai')









##PLOT LDA TABLE
plot_CV(dat.LDA, freq = FALSE, rm0 = TRUE, cex = 10,
round = 2, labels = TRUE)

plot_CV2(dat.LDA, legend = TRUE, palette = pal_seq_grey)

##PLOT LDA
plot(dat.LDA, type, xax = 1, yax = 2, points = TRUE,
color = "#000000", pch = 20, cex = 2, palette = col_spring,
center.origin = FALSE, zoom = 1, xlim = NULL, ylim = NULL,
bg = par("bg"), grid = TRUE, nb.grids = 3, morphospace = TRUE,
pos.shp = c("range", "full", "circle", "xy", "range_axes", "full_axes")[1],
amp.shp = 1, size.shp = 1, nb.shp = 12, nr.shp = 6, nc.shp = 5,
rotate.shp = 0, flipx.shp = FALSE, flipy.shp = FALSE, pts.shp = 60,
border.shp = col_alpha("#000000"), lwd.shp = 1,
col.shp = col_alpha("#000000"), stars = FALSE, ellipses = FALSE,
conf.ellipses = 0.5, ellipsesax = FALSE, conf.ellipsesax = c(0.5, 0.9),
lty.ellipsesax = 1, lwd.ellipsesax = sqrt(2), chull = FALSE,
chull.lty = 1, chull.filled = TRUE, chull.filled.alpha = 0.92,
density = FALSE, lev.density = 20, contour = FALSE, lev.contour = 3,
n.kde2d = 100, delaunay = FALSE, loadings = FALSE,
labelspoints = TRUE, col.labelspoints = par("fg"),
cex.labelspoints = 0.6, abbreviate.labelspoints = TRUE,
labelsgroups = TRUE, cex.labelsgroups = 0.8, rect.labelsgroups = FALSE,
abbreviate.labelsgroups = FALSE, color.legend = TRUE, axisnames = TRUE,
axisvar = TRUE, unit = FALSE, eigen = TRUE, rug = TRUE,
title = substitute(PUT_TITLE_HERE), box = TRUE, old.par = TRUE) 

##AVERAGE SHAPE
coo_plot (mshapes (dat.f))



###KMEANS
KMEANS(dat.PCA, centers = 11)

##USE KMEANS RESULTS AS NEW TYPES
dat.k <- KMEANS(dat.PCA, centers = 3)
dat.kc <- data.frame(ld = ori$ld, type = dat.k$cluster, site = ori$site)
dat$fac <- dat.kc
dat.f <- efourier (dat, 60, norm = TRUE)
dat.PCA <- PCA(dat.f)
dat.LDA <- LDA(dat.PCA, "type")
dat.LDAC <- data.frame(Name = ori$ld, New_Type = dat.LDA$CV.fac, Original_Type = ori$type)



###LIST OF NEW CLASSIFICATION BASED ON LDA
dat.LDAC <- data.frame(Name = ori$ld, New_Type = dat.LDA$CV.fac, Original_Type = ori$type)
dat.LDAC

##USE LDA-BASED CLASSES AS NEW TYPES
dat.LDAC <- data.frame(Name = ori$ld, New_Type = dat.LDA$CV.fac, Original_Type = ori$type)
dat.lc <- data.frame(ld = ori$ld, type = dat.LDAC$New_Type, site =  ori$site)
dat$fac <- dat.lc
dat.f <- efourier (dat, 60, norm = TRUE)
dat.PCA <- PCA(dat.f)
dat.LDA <- LDA(dat.PCA, "type")
dat.LDAC <- data.frame(Name = ori$ld, New_Type = dat.LDA$CV.fac, Original_Type = ori$type)



#####JEREMY'S SPACE FOR "EXPERIMENTATION", DON'T RUN THIS YOU PLEB

###STOP
