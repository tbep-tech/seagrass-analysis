
library(tbeptools)
library(tidyverse)
library(ggfx)
library(grid)
library(extrafont)
library(here)

loadfonts(device = 'win', quiet = T)

fml <- "Lato"

# seagrass coverage -------------------------------------------------------

##
# data prep

# extra years for padding
exyrs <- seq(1950, 1953)

toplo <- tibble(
  Year = c(exyrs, seq(1982, 2020))
) %>%
  left_join(seagrass, by = 'Year') %>%
  mutate(
    Acres = Acres / 1000
  )

# label for last bar 
lastlab <- seagrass %>% 
  filter(Year == max(Year)) %>% 
  pull(Acres) %>% 
  round(0) %>% 
  format(big.mark = ',') %>% 
  paste(., 'acres')

# y loc for last bar label
lasty <- seagrass %>% 
  filter(Year == max(Year)) %>% 
  pull(Acres) %>% 
  `/`(1000) %>% 
  `-`(1)

##
# base ggplot

# axis labels
lbs <- toplo$Year
lbs[lbs %in% exyrs[-1]] <- ''

p <- ggplot(toplo, aes(x = factor(Year), y = Acres)) +
  with_shadow(geom_bar(fill = '#00806E', stat = 'identity', colour = 'black', width = 1.3), sigma = 2.7, x_offset = 0, y_offset = 0) +
  geom_segment(x = 0, xend = 2, y = 38, yend = 38, col = 'red', size = 2) +
  geom_segment(x = 4, xend = 42, y = 38, yend = 38, col = 'red', size = 2) +
  geom_segment(x = 42, xend = 44, y = 40, yend = 40, col = 'red', size = 2) +
  annotate("text", label = "Seagrass Coverage Goal", x = 4, y = 40.5, color = 'red', size = 5, hjust = 0, family = fml) +
  annotate('text', x = 43, y = lasty, label = lastlab, angle = 90, hjust = 1, vjust = 0.4) + 
  scale_x_discrete(breaks = lbs, labels = lbs, expand = c(0.04, 0.04)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1 * max(toplo$Acres, na.rm = T))) +
  # theme_bw() +
  theme(
    axis.line = element_line(),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.title.x = element_blank(),
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  labs(
    y = 'Seagrass Coverage (x1,000 acres)'
  )

##
# top, bottom axis line breaks

gt <- ggplotGrob(p)

is_axisb <- which(gt$layout$name == "axis-b")
is_axist <- which(gt$layout$name == "axis-t")
is_axisl <- which(gt$layout$name == "axis-l")
is_axisr <- which(gt$layout$name == "axis-r")

axisb <- gt$grobs[[is_axisb]]
xline <- axisb$children[[1]]

# location of break, break type
xline$y <- unit(rep(1, 4), "npc")
xline$x <- unit(c(0, 0.06, 1, 0.105), "npc")
xline$id <- c(1, 1, 2, 2)
xline$arrow <- arrow(angle = 90, length = unit(0.07, 'inches'))

axisb$children[[1]] <- xline
axist <- xline
axisl <- gt$grobs[[is_axisl]]

gt$grobs[[is_axisb]] <- axisb
gt$grobs[[is_axist]] <- axist
gt$grobs[[is_axisr]] <-axisl$children[[1]]

##
# save plot

png(here('figures/seagrasscov.png'), height = 3.25, width = 6, res = 300, unit = 'in')
grid.newpage(); grid.draw(gt)
dev.off()