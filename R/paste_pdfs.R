#' PDF Combine
#'
#' @description Combine multiple pdf's in one unique file
#' @details Needs Ghostscript installation to work
#' @param pdf_files_to_paste vector with pdf files to combine in one unique pdf or
#' a character indicating which pdf's (such as *.pdf)
#' @param output_file name of the output pdf that combines all pdfs
#' @param auto_rotate_pages automatically rotate pages when pasting (default = \code{FALSE})
#' @param gs_path path to Ghostscript installation
#' @param quiet Don't output messages
#' @param cleanup Delete the files after merge? Default is \code{F}
#' @importFrom glue glue
#' @examples
#' for (i in 0:9){
#'   pdf(paste0("Example",i,".pdf"), width = 6, height = 4)
#'   plot(rnorm(10), rnorm(10), main = paste("Example",i,".pdf"))
#'   dev.off()
#' }
#'
#' pdf_combine(list.files(pattern = "\\.pdf"), output_file = "test.pdf", cleanup = TRUE)
#' file.remove("test.pdf")
#' @export

pdf_combine <- function(pdf_files_to_paste = list.files(pattern = "\\.pdf"),
                        output_file = "combined_pdfs.pdf",
                        auto_rotate_pages = c("None","All","PageByPage"),
                        gs_path = NULL, quiet = F, cleanup = F){

  #Get ghostscript
  if (is.null(gs_path)){
    gs_path <- detect_ghostscript(quiet)
  }

  system2(gs_path, args = c("-dNOPAUSE",
                            "-sDEVICE=pdfwrite",
                            glue::glue("-dAutoRotatePages=/{auto_rotate_pages[1]}"),
                            glue::glue(" -sOUTPUTFILE={output_file}"),
                            "-dBATCH",
          paste(pdf_files_to_paste, collapse = " ")))

  if (cleanup){
    file.remove(pdf_files_to_paste)
  }

}
