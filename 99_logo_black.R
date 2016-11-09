library(magick)
logo <- image_read("maple_europa.png")
image_colorize(logo, opacity = 100, color = "black")
image_write(image_colorize(logo, opacity = 100, color = "black"),
            path = "maple_europa_black.png")

