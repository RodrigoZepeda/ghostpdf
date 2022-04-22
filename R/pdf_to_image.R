#' PDF to Image
#'
#' @description Convert s from a \code{pdf} into \code{png},\code{tiff},\code{jpeg},
#' \code{bmp}, \code{fax}, \code{pcx} or \code{psd}
#' @details  You can either extract a continuous sequence of pages specifying  \code{first_page}
#' and \code{last_page} or a discontinuous sequence by using \code{page_sequence}.
#' Needs Ghostscript installation to work.
#' @param pdf_file pdf file from which extract pages
#' @param first_page first page to extract (default = 1)
#' @param last_page  last page to extract (default = number of pages in pdf)
#' @param page_sequence page numbers to extract if \code{first_page} and \code{last_page} are
#' not specified you can create a page_sequence of numbers of the pdf pages to extract
#' @param output_file name of the output image
#' @param resolution Image resolution (dpi)
#' @param image_device Specify one of the image devices in
#' \url{https://www.ghostscript.com/doc/9.50/Devices.htm#TIFF}
#' @param preserve_annotations boolean flag to try and preserve annotations in the pdfs (default TRUE)
#' @param gs_path path to Ghostscript installation
#' @param quiet Don't output messages
#' @param additional_flags Additional string of bash flags to pass to ghostscript
#' @importFrom glue glue
#' @examples
#' \dontrun{
#' for (i in 1:20){
#'   pdf(paste0("Example",i,".pdf"), width = 6, height = 4)
#'   plot(rnorm(10), rnorm(10), main = paste("Page",i,".pdf"))
#'   dev.off()
#' }
#'
#' pdf_merge(list.files(pattern = "Example.*.pdf"), output_file = "test.pdf", cleanup = TRUE)
#'
#' #Get pages 4 to 18
#' pdf_to_image("test.pdf", 4, 18)
#' file.remove(list.files(pattern = "output.*.png"))
#'
#' #Get pages 1, 5, 7
#' pdf_to_image("test.pdf", page_sequence = c(1,5,7,12), output_file = "example_2.jpg")
#' file.remove(list.files(pattern = "example_2.*.jpg"))
#'
#' #Get pages 12 till the end
#' pdf_to_image("test.pdf", first_page = 12, output_file = "example_3.tiff", resolution = 100)
#' file.remove(list.files(pattern = "example_3.*.tiff"))
#'
#' file.remove("test.pdf")
#' }
#' @export

pdf_to_image <- function(pdf_file, first_page = 1, last_page = NULL,  output_file = "output.png",
                         page_sequence = NULL, image_device = NULL, resolution = 750,
                         preserve_annotations = TRUE, gs_path = NULL, quiet = TRUE,
                         additional_flags = ""){

  #Get ghostscript
  if (is.null(gs_path)){
    gs_path <- detect_ghostscript(quiet)$gs
  }

  if (quiet){
    quiet_flag <- "-dQUIET"
  } else {
    quiet_flag <- ""
  }

  #Get number of pdf pages
  total_number_of_pages <- pdf_num_pages(pdf_file, quiet = quiet, gs_path = gs_path)

  if (is.null(last_page) & is.null(page_sequence)){
    last_page <- total_number_of_pages
  } else if (!is.null(last_page)){
    last_page <- min(last_page, total_number_of_pages)
  } else {
    page_sequence <- ceiling(page_sequence[which(page_sequence > 0 &
                                                   page_sequence < total_number_of_pages)])
  }

  #Check image device compatibility
  device       <- get_image_device(output_file, image_device)
  if (is.null(image_device)){
    image_device <- device$device
  }

  #Check outfile
  if (!is.null(last_page) && last_page > first_page){
    ndigits <- ceiling(log10(last_page - first_page))
    if (.Platform$OS.type == "unix"){
      outfile <- glue::glue("{device['name']}_%0{ndigits}d.{device['suffix']}")
    } else {
      outfile <- glue::glue("{device['name']}_%%0{ndigits}d.{device['suffix']}")
    }
  } else {
    outfile <- glue::glue("{device['name']}.{device['suffix']}")
  }

  if (preserve_annotations){
    preserve_annotations_flag <- glue::glue("-dPreserveAnnots=true")
  } else {
    preserve_annotations_flag <- glue::glue("-dPreserveAnnots=false")
  }

  #Check if first and last pages are specified:
  if (!is.null(first_page) & !is.null(last_page)){
    system2(gs_path, args = c("-dNOPAUSE",
                              "-dBATCH",
                              "-dNOSAFER",
                              preserve_annotations_flag,
                              glue::glue("{quiet_flag}"),
                              glue::glue("-r{resolution}"),
                              glue::glue("-sDEVICE={image_device}"),
                              glue::glue("-dFirstPage={first_page}"),
                              glue::glue("-dLastPage={last_page}"),
                              glue::glue("-sOUTPUTFILE={outfile}"),
                              paste(additional_flags, collapse = " "),
                              pdf_file))
  } else {

    #Extract pages
    for (page in page_sequence){
      fname <- glue::glue("{device['name']}_{page}.{device['suffix']}")
      pdf_to_image(pdf_file = pdf_file, first_page = page, last_page = page,
                   gs_path = gs_path, quiet = quiet,
                   preserve_annotations = preserve_annotations,
                   image_device = image_device, resolution = resolution,
                   page_sequence = NULL, additional_flags = additional_flags,
                   output_file = fname)
    }

  }


}
