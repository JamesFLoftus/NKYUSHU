## GET PACKAGEplot
library (Momocs)


##GET AND READ FILES
getwd()
lf <- list.files ("JamesL", full.names=TRUE)

coo <- import_jpg(lf, auto.notcentered = TRUE, fun.notcentered = NULL, threshold = 0.5)
dat <- Out(coo)

##ADD CLASSIFIERS
dat$fac <- read.csv("JamesL.csv")

##EDIT DATA CLASS
dat$fac <- edit(dat$fac)
type.fac <- as.factor(dat$type)
dat$fac <- data.frame (ld = dat$ld, type = type.fac, site = dat$site)

##ELIPTICAL FOURIER TRANSFORM
dat.f <- efourier (dat, 11, norm = FALSE)

##PCA
dat.PCA <- PCA(dat.f)

##DISPLAY ALL IMAGES
panel(dat, fac='type', palette=col_spring, names=TRUE)

stack (dat)


dat %>%
  
  Out() %>%
  
  coo_center() %>%
  
  coo_scale() %>%
  
  coo_slidedirection("right") %>%
  
  coo_untiltx() %>%
  
  efourier(nb.h = 11, norm = FALSE) %>%
  
  PCA() %>%
  
  plot_PCA(morphospace = TRUE, zoom = 1, labelpoints = TRUE, points = TRUE, ~type, box = TRUE, axesvar = TRUE) %>%
  
  layer_morphospace_PCA(
    position = c("xy")[1],
    size = 0.5,
    draw = TRUE) %>%

  layer_density(levels_density = 20,levels_contour = 4,alpha = 1/3,n = 200,density = TRUE,contour = TRUE)