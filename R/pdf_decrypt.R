#' PDF Decrypt
#'
#' @description Decrypts a password protected pdf file
#' @details Needs Ghostscript installation to work.
#' @param pdf_file pdf file from which to count the number of pages
#' @param output_file output pdf file of reduced size
#' @param gs_path path to Ghostscript installation
#' @param quiet Don't output messages
#' @param password Defines that the document be encrypted with the specified owner password.
#' Owner password sets the password to edit the document.
#' @param additional_flags Additional string of bash flags to pass to ghostscript
#' @importFrom glue glue
#' @examples
#' pdf("Example.pdf", width = 6, height = 4)
#' plot(rnorm(10), rnorm(10), main = "Encrypted example")
#' dev.off()
#'
#' #Shrink file
#' pdf_encrypt("Example.pdf", password = "mypassword", output_file = "pdf_encrypted.pdf")
#' pdf_decrypt("Example.pdf", password = "mypassword", output_file = "pdf_decrypted.pdf")
#' file.remove(c("Example.pdf","pdf_encrypted.pdf","pdf_decrypted.pdf"))
#' @export


pdf_decrypt <- function(pdf_file, password, output_file = pdf_file, quiet = T, gs_path = NULL,
                        additional_flags = "") {

  #Get ghostscript
  if (is.null(gs_path)){
    gs_path <- detect_ghostscript(quiet)["gs"]
  }

  if (quiet){
    quiet_flag <- "-dQUIET"
  } else {
    quiet_flag <- ""
  }

  if (output_file == pdf_file){
    flag_overwrite <- TRUE
    output_file    <- tempfile()
  } else {
    flag_overwrite <- FALSE
  }

  system2(gs_path, args = c("-dNOPAUSE",
                            "-dBATCH",
                            glue::glue("{quiet_flag}"),
                            glue::glue("-sPDFPassword={password}"),
                            "-sDEVICE=pdfwrite",
                            glue::glue("-sOUTPUTFILE={output_file}"),
                            paste(additional_flags, collapse = " "),
                            pdf_file))

  #Change names
  if (flag_overwrite){
    file.copy(from = output_file, to = pdf_file, overwrite = TRUE)
  }

  return(NULL)

}
