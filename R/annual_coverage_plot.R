
library(tbeptools)
library(extrafont)
library(here)

loadfonts(device = 'win', quiet = T)

fml <- "Lato"

# seagrass coverage -------------------------------------------------------

png(here('figures/seagrasscov.png'), width = 2886, height = 1632, res = 500, unit = 'px')
show_seagrasscoverage(seagrass, family = fml)
dev.off()
