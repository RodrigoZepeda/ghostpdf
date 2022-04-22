#' PDF Shrink
#'
#' @description Apply functions to attempt to reduce the size of a pdf
#' @details Needs Ghostscript installation to work.
#' @param pdf_file pdf file from which to count the number of pages
#' @param output_file output pdf file of reduced size
#' @param gs_path path to Ghostscript installation
#' @param quiet Don't output messages
#' @param resolution Image resolution (in dpi)
#' @param pdf_settings Presets the "distiller parameters" to one of four predefined settings:
#' "eBook" \code{"ebook"}, "Screen Optimized" \code{"screen"},
#' "Print Optimized" \code{"printer"}, "Prepress Optimized" \code{"prepress"} or
#' \code{"default"}. Lowest is \code{"screen"}.
#' @param detect_duplicate_images boolean that attempts to detect the same image file
#' and keep only one version of the image to reduce pdf size.
#' @param max_inline_image_size Specifies the maximum size of an inline image, in bytes.
#' For images larger than this size, pdfwrite will create an XObject instead of embedding the
#' image into the context stream. The default value is 4000. Note that redundant inline images
#' must be embedded each time they occur in the document, while multiple references can be made
#' to a single XObject image. Therefore it may be advantageous to set a small or zero value if
#' the source document is expected to contain multiple identical images, reducing the size of
#' the generated PDF.
#' @param additional_flags Additional string of bash flags to pass to ghostscript
#' @importFrom glue glue
#' @examples
#' for (i in 1:9){
#'   pdf(paste0("Example",i,".pdf"), width = 6, height = 4)
#'   plot(rnorm(10), rnorm(10), main = paste("Example",i,".pdf"))
#'   dev.off()
#' }
#'
#' pdf_merge(list.files(pattern = "Example.*\\.pdf"), output_file = "test.pdf", cleanup = TRUE)
#' #Check current file size:
#' original_size <- file.info("test.pdf")["size"]
#'
#' #Shrink file
#' pdf_shrink("test.pdf")
#'
#' #Check new file size
#' reduced_size <- file.info("test.pdf")["size"]
#'
#' #Verify size is smaller
#' original_size > reduced_size
#'
#' file.remove("test.pdf")
#' @export

pdf_shrink <- function(pdf_file, output_file = pdf_file, gs_path = NULL,
                       quiet = TRUE,
                       resolution = 300,
                       max_inline_image_size   = 1000,
                       pdf_settings            = c("screen","ebook","default","printer","prepress"),
                       detect_duplicate_images = TRUE,
                       additional_flags = c("-dCompressFonts=true",
                                            "-dCompressStreams=true")){

  #Get ghostscript
  if (is.null(gs_path)){
    gs_path <- detect_ghostscript(quiet)["gs"]
  }

  if (quiet){
    quiet_flag <- "-dQUIET"
  } else {
    quiet_flag <- ""
  }

  if (detect_duplicate_images){
    detect_duplicate_images_flag <- glue::glue("-dDetectDuplicateImages=true")
  } else {
    detect_duplicate_images_flag <- glue::glue("-dDetectDuplicateImages=false")
  }

  if (output_file == pdf_file){
    flag_overwrite <- TRUE
    output_file    <- tempfile()
  } else {
    flag_overwrite <- FALSE
  }

  system2(gs_path,
          args = c("-dNOPAUSE",
                   "-dBATCH",
                   "-sDEVICE=pdfwrite",
                   glue::glue("{quiet_flag}"),
                   glue::glue("-r{resolution}"),
                   detect_duplicate_images_flag,
                   paste(additional_flags, collapse = " "),
                   glue::glue("-dPDFSETTINGS=/{pdf_settings[1]}"),
                   glue::glue("-dMaxInlineImageSize={max_inline_image_size}"),
                   glue::glue("-sOUTPUTFILE={output_file}"),
                   pdf_file))

  #Change names
  if (flag_overwrite){
    file.copy(from = output_file, to = pdf_file, overwrite = TRUE)
  }

  return(NULL)

}
