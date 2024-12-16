
## Setup ------------------------------

# setwd("~/repos/sandbox/vha/vhtf")

library(tidyverse)
library(hdatools)
library(scales)
library(sysfonts)
library(ggtext)

knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  error = FALSE,
  message = FALSE,
  fig.asp = 0.618,
  fig.align = "left"
)

## VHA brand --------------------------

# Font: Montserrat

# Light green:  #A0D18E
# Dark turq:    #0C4D4F
# Yellow:       #ECC51E
# Grey:         #2E3030
# Light turq:   #19787B
# Light blue:   #E3F3F5

sysfonts::font_add_google("Montserrat", "Monsterrat", bold.wt = 600)

vha <- c("#0C4D4F", "#A0D18E", "#ECC51E", "#19787B", "#2E3030", "#E3F3F5")

theme_vha <- function(
    base_size = 13,
    base_family = "Montserrat",
    flip_gridlines = FALSE
) {
  
  ggplot2::theme_minimal() %+replace%
    
    ggplot2::theme(
      
      rect = ggplot2::element_rect(
        fill = "white",
        colour = "black",
        size = 0.5,
        linetype = 1L
      ),
      
      text = ggplot2::element_text(
        family = base_family,
        face = "plain",
        size = base_size,
        colour = "#383c3d",
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 0.9,
        margin = ggplot2::margin(),
        debug = FALSE
      ),
      
      line = ggplot2::element_line(colour = "#000000",
                                   size = 1,
                                   linetype = 1L,
                                   lineend = "butt"),
      
      plot.title = ggtext::element_textbox_simple(
        size = base_size * 1.25,
        color = "#383c3d",
        hjust = 0L,
        vjust = 0L,
        margin = ggplot2::margin(b = 10, unit = "pt"),
        face = "bold",
        family = "Montserrat"
      ),
      
      plot.subtitle = ggtext::element_textbox_simple(
        size = base_size,
        color = "#383c3d",
        hjust = 0L,
        vjust = 0L,
        margin = ggplot2::margin(t = -5, b = 10, unit = "pt"),
        face = "plain",
        family = "Montserrat"
      ),
      
      plot.caption = ggtext::element_textbox_simple(
        size = base_size * 0.875,
        color = "#383c3d",
        hjust = 0L,
        vjust = 0L,
        margin = ggplot2::margin(t = 10, unit = "pt"),
        lineheight = 1.1,
        face = "plain",
        family = "Montserrat"
      ),
      
      plot.title.position = "plot",
      
      plot.caption.position = "plot",
      
      panel.background = ggplot2::element_blank(),
      
      panel.grid.minor = ggplot2::element_blank(),
      
      legend.position = "none",
      
      legend.title = ggplot2::element_blank(),
      
      axis.title = ggplot2::element_blank(),
      
      axis.ticks = ggplot2::element_blank()
      
    ) +
    
    if(flip_gridlines == FALSE) {
      
      ggplot2::theme(
        
        panel.grid.major.x = ggplot2::element_blank(),
        
        panel.grid.major.y = ggplot2::element_line(
          color = "#cbcdcc",
          size = 0.05
        )
        
      )
      
    } else {
      
      ggplot2::theme(
        
        panel.grid.major.y = ggplot2::element_blank(),
        
        panel.grid.major.x = ggplot2::element_line(
          color = "#cbcdcc",
          size = 0.05
        )
        
      )
      
    }
  
}

vha_pal_discrete <- function(direction = 1, repeat_pal = FALSE) {
  
  pal <- vha
  
  function(n) {
    
    if (repeat_pal) {
      if (n > length(pal)) {
        times <- ceiling(n / length(pal))
        pal <- rep(pal, times)
      }
    }
    
    pal_n <- pal[1:n]
    
    if (direction == -1) {
      return(rev(pal_n))
    } else {
      return(pal_n)
    }
    
  }
  
}

scale_color_vha <- function(direction = 1, repeat_pal = FALSE, ...) {
  ggplot2::discrete_scale(
    "colour", "vha", palette = vha_pal_discrete(direction = direction,
                                                repeat_pal = repeat_pal),
    ...
  )
}

scale_fill_vha <- function(direction = 1, repeat_pal = FALSE, ...) {
  ggplot2::discrete_scale(
    "fill", "vha", palette = vha_pal_discrete(direction = direction,
                                                repeat_pal = repeat_pal),
    ...
  )
}


