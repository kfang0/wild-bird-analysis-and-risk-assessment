library(gridExtra)
library(grid)
library(magick)
# Read PDF files
pdf_file <- c('pacific.pdf','central.pdf','mississippi.pdf','atlantic.pdf')
# Read the first page of each PDF as magick-image
pdf_images <- lapply(pdf_file,function(f) image_read_pdf(f,density = 600)[1])
# Convert magick-image to as.raster
pdf_rasters <- lapply(pdf_images,as.raster)
# Plot images and arrange them in a 2x2 grid
image_plot <- lapply(pdf_rasters,function(raster_img){
  rasterGrob(raster_img)
})
grid.arrange(grobs = image_plot,ncol = 2,nrow = 2)






