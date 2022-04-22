#' PDF Number of Pages
#'
#' @description Get the number of pages on a pdf file
#' @details Needs Ghostscript installation to work.
#' @param pdf_file pdf file from which to count the number of pages
#' @param gs_path path to Ghostscript installation
#' @param quiet Don't output messages
#' @param additional_flags Additional string of bash flags to pass to ghostscript
#' @importFrom glue glue
#' @examples
#' for (i in 0:9){
#'   pdf(paste0("Example",i,".pdf"), width = 6, height = 4)
#'   plot(rnorm(10), rnorm(10), main = paste("Example",i,".pdf"))
#'   dev.off()
#' }
#'
#' pdf_merge(list.files(pattern = "\\.pdf"), output_file = "test.pdf", cleanup = TRUE)
#' pdf_num_pages("test.pdf")
#' file.remove("test.pdf")
#' @export

pdf_num_pages <- function(pdf_file, gs_path = NULL, quiet = T, additional_flags = ""){

  #Get ghostscript
  if (is.null(gs_path)){
    gs_path <- detect_ghostscript(quiet)$gs
  }

  if (quiet){
    quiet_flag <- "-dQUIET"
  } else {
    quiet_flag <- ""
  }

  page_num <- system2(gs_path,
                      args = c("-dNOPAUSE",
                               "-DNOSAFER",
                               "-dNODISPLAY",
                               glue::glue("{quiet_flag}"),
                               paste(additional_flags, collapse = " "),
                               glue::glue("-c '({pdf_file}) (r) file runpdfbegin pdfpagecount = quit'")),
                      stdout = TRUE)
paste0()
  return(as.numeric(page_num))

}
