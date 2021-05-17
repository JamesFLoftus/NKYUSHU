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