library(magick)

# Create a blank image with specified dimensions and color
img <- image_blank(width = 150, height = 150, color = "lightgray")

# Add text to the image
img <- image_annotate(img, text = "No Image", size = 20, color = "black", gravity = "center")

# Save the image to the 'inst/www/img/placeholders/' directory
image_write(img, path = "inst/www/img/placeholders/default-image.png", format = "png")
