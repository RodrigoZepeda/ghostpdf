#' PDF Encrypt
#'
#' @description Encrypts a pdf file
#' @details Needs Ghostscript installation to work.
#' @param pdf_file pdf file from which to count the number of pages
#' @param output_file output pdf file of reduced size
#' @param gs_path path to Ghostscript installation
#' @param quiet Don't output messages
#' @param password Set same password for owner and user. Ignored if \code{owner_password} and
#' \code{user_password} are set.
#' @param owner_password Defines that the document be encrypted with the specified owner password.
#' Owner password sets the password to edit the document.
#' @param user_password Defines the user password for opening the document but cannot edit.
#' @param permissions Defines the PDF permissions flag field. Negative values are allowed to
#' represent unsigned integers with the highest bit set. See the PDF Reference manual
#' for the meaning of the flag bits.
#' @param encryption_revision Defines the encryption method revision number - either 2 or 3.
#' @param key_length     Defines the length (in bits) of the encryption key. Must be a multiple of
#' 8 in the interval (40, 128). If the length isn't 40, \code{encryption_revision} must be 3.
#' @param additional_flags Additional string of bash flags to pass to ghostscript
#' @importFrom glue glue
#' @examples
#' pdf("Example.pdf", width = 6, height = 4)
#' plot(rnorm(10), rnorm(10), main = "Encrypted example")
#' dev.off()
#'
#' #Shrink file
#' pdf_encrypt("Example.pdf", password = "mypassword")
#' file.remove("Example.pdf")
#' @export


pdf_encrypt <- function(pdf_file, password,
                        user_password = password,
                        output_file = pdf_file, quiet = T, gs_path = NULL,
                        owner_password = password,
                        permissions = NULL,
                        encryption_revision = 3, key_length = 128, additional_flags = "") {

  #Get ghostscript
  if (is.null(gs_path)){
    gs_path <- detect_ghostscript(quiet)["gs"]
  }

  if (quiet){
    quiet_flag <- "-dQUIET"
  } else {
    quiet_flag <- ""
  }

  if (is.null(user_password)){
    user_password_flag <- ""
  } else {
    user_password_flag <- glue::glue("-sUserPassword={user_password}")
  }

  if (is.null(owner_password)){
    owner_password_flag <- ""
  } else {
    owner_password_flag <- glue::glue("-sOwnerPassword={owner_password}")
  }

  if (is.null(permissions)){
    permissions_flag <- ""
  } else {
    permissions_flag <- glue::glue("-dPermissions={permissions}")
  }

  encryption_revision_flag <- glue::glue("-dEncryptionR={encryption_revision}")
  key_length_flag          <- glue::glue("-dKeyLength={key_length}")

  if (output_file == pdf_file){
    flag_overwrite <- TRUE
    output_file    <- tempfile()
  } else {
    flag_overwrite <- FALSE
  }

  system2(gs_path, args = c("-dNOPAUSE",
                            "-dBATCH",
                            user_password_flag,
                            owner_password_flag,
                            encryption_revision_flag,
                            permissions_flag,
                            key_length_flag,
                            glue::glue("{quiet_flag}"),
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
