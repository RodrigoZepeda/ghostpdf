#' PDF Extract
#'
#' @description Extract pages from a pdf
#' @details  You can either extract a continuous sequence of pages specifying  \code{first_page}
#' and \code{last_page} or a discontinuous sequence by using \code{page_sequence}.
#' Needs Ghostscript installation to work.
#' @param pdf_file pdf file from which extract pages
#' @param first_page first page to extract
#' @param last_page  last page to extract
#' @param page_sequence page numbers to extract if \code{first_page} and \code{last_page} are
#' not specified you can create a page_sequence of numbers of the pdf pages to extract
#' @param output_file name of the output pdf that combines all pdfs
#' @param auto_rotate_pages automatically rotate pages when pasting (default = \code{"None"})
#' @param preserve_annotations boolean flag to try and preserve annotations in the pdfs (default TRUE)
#' @param gs_path path to Ghostscript installation
#' @param quiet Don't output messages
#' @param additional_flags Additional string of bash flags to pass to ghostscript
#' @importFrom glue glue
#' @examples
#' for (i in 1:9){
#'   pdf(paste0("Example",i,".pdf"), width = 6, height = 4)
#'   plot(rnorm(10), rnorm(10), main = paste("Page",i,".pdf"))
#'   dev.off()
#' }
#'
#' pdf_merge(list.files(pattern = "\\.pdf"), output_file = "test.pdf", cleanup = TRUE)
#'
#' #Get pages 4 to 7
#' pdf_extract("test.pdf", 4, 7, output_file = "example_1.pdf")
#' file.remove("example_1.pdf")
#'
#' #Get pages 1, 5, 7
#' pdf_extract("test.pdf", page_sequence = c(1,5,7), output_file = "example_2.pdf")
#' file.remove("example_2.pdf")
#'
#' #Get pages 5 till the end
#' pdf_extract("test.pdf", first_page = 5, output_file = "example_3.pdf")
#' file.remove("example_3.pdf")
#'
#' file.remove("test.pdf")
#' @export


pdf_extract <- function(pdf_file, first_page = NULL, last_page = NULL,
                        page_sequence = NULL, output_file = "output.pdf",
                        preserve_annotations = TRUE, gs_path = NULL, quiet = T,
                        auto_rotate_pages,
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

  #Check that either first page of page sequence is specified
  if (is.null(first_page) & is.null(page_sequence)){
    stop("Please specify either first_page or page_sequence")
  }

  #Cleanup to make sure we are in file
  if (is.null(last_page) & is.null(page_sequence)){
    last_page <- total_number_of_pages
  }

  if (!is.null(last_page) && last_page > total_number_of_pages){
    last_page <- total_number_of_pages
  }

  if (!is.null(page_sequence)){
    page_sequence <- floor(page_sequence)
  }

  if (!is.null(page_sequence) && max(page_sequence) > total_number_of_pages){
    page_sequence <- page_sequence[which(page_sequence < total_number_of_pages)]
  }

  if (!is.null(page_sequence) && min(page_sequence) < 1){
    page_sequence <- page_sequence[which(page_sequence > 0)]
  }

  #Check if first and last pages are specified:
  if (!is.null(first_page) & !is.null(last_page)){
    system2(gs_path, args = c("-dNOPAUSE",
                              "-dBATCH",
                              glue::glue("{quiet_flag}"),
                              "-sDEVICE=pdfwrite",
                              glue::glue("-dFirstPage={first_page}"),
                              glue::glue("-dLastPage={last_page}"),
                              glue::glue("-sOUTPUTFILE={output_file}"),
                              paste(additional_flags, collapse = " "),
                              pdf_file))
  } else {

    #PDF
    dirname   <- tempdir()
    pdf_files <- c()

    #Extract pages
    for (page in page_sequence){
      fname <- file.path(dirname, paste(page, ".pdf", sep=""))
      pdf_extract(pdf_file = pdf_file, first_page = page, last_page = page,
                  gs_path = gs_path, quiet = quiet,
                  preserve_annotations = preserve_annotations,
                  page_sequence = NULL, additional_flags = additional_flags,
                  output_file = fname)
      pdf_files <- c(pdf_files, fname)
    }

    #Merge pages
    pdf_merge(pdf_files, output_file = output_file,
                auto_rotate_pages = "None",
                preserve_annotations = preserve_annotations,
                gs_path = gs_path, quiet = quiet, additional_flags = additional_flags)

    unlink(dirname)
  }


}
