
## GET PACKAGEplot
library (Momocs)

##GET AND READ FILES
getwd()
lf <- list.files ("JamesL", full.names=TRUE)
lf
coo <- import_jpg(lf, auto.notcentered = TRUE, fun.notcentered = NULL, threshold = 0.5)
dat <- Out(coo)

##ADD CLASSIFIERS
dat$fac <- read.csv("JamesL.csv")

##EDIT DATA CLASS(IF NEEDED)
#Insert Name column as "ld"
#Insert type column as "type"
#Insert site column as "site"
dat$fac <- edit(dat$fac)
type.fac <- as.factor(dat$type)
dat$fac <- data.frame (ld = dat$ld, type = type.fac, site = dat$site)

##ELIPTICAL FOURIER TRANSFORM
dat.f <- efourier (dat, 11, norm = TRUE)

##PCA
dat.PCA <- PCA(dat.f)

##LDA (REQUIRES MORE THAN ONE GROUP IN A CLASS)
dat.LDA <- LDA(dat.PCA, "type")

##LIST TYPES CORRECTED BY LDA
dat.LDAC <- data.frame(Name = dat$ld, New_Type = dat.LDA$CV.fac, Original_Type = dat$type)
dat.LDAC

plot (coo)

##DISPLAY ALL IMAGES
panel(dat, fac='type', palette=col_spring, names=TRUE)

stack (dat)

##DISPLAY ALL IMAGES STACKED
dat %>% 
  	coo_center %>% 
	coo_slidedirection("N") %T>% 
	print() %>% stack()

##DISPLAY ALL IMAGES STACKED CENTRED
dat %>% 
  	coo_center %>% coo_scale %>%
	coo_slidedirection("N") %T>% 
	print() %>% stack()

## DISPLAY PCA
plot(dat.PCA, xax = 1, yax = 2, points = FALSE,
color = "#000000", pch = 20, cex = 2, palette = pal_alpha,
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
title = substitute(PUT_TITLE_HERE), box = TRUE, old.par = TRUE) 