#' @importFrom glue glue

get_image_device <- function(image_name, device){
  #Function that takes an image name say "image.png" and returns a
  #device for the image say png16m
  image_device <- unlist(strsplit(image_name, "\\."))

  device <- switch(
    image_device[2],
    png  = "png16m",
    jpg = "jpeg",
    tiff = "tiff48nc",
    fax = "faxg4",
    bmp = "bmp32b",
    pcx = "pcxcmyk",
    psd = "psdrgb16",
    device
  )

  return(list("name" = image_device[1], "suffix" = image_device[2], "device" = device))

}
