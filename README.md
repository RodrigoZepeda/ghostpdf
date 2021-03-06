
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ghostpdf

<!-- badges: start -->
<!-- badges: end -->

Ghostpdf wraps around [ghostscript](https://www.ghostscript.com/) to
provide easy manipulation functions for pdf files. With Ghostpdf you can
merge documents, decrypt them, encrypt them back, transform pdfs into
images or extract pages from a pdf.

> **Note** The idea is to eventually have the same functions applicable
> with PostScript files too.

## Installation

You can install the development version of ghostpdf from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RodrigoZepeda/ghostpdf")
```

You will also need to install **ghostpdf** either from their page
<https://www.ghostscript.com/releases/gsdnld.html> or if you are in a
Unix system with a package manager such as:

-   **OSX** `brew install ghostscript`
-   **Ubuntu/Debian** `sudo apt-get install ghostscript`
-   **CentOS/Fedora** `sudo yum install ghostscript`

OSX users who are not familiar with homebrew open terminal and copy:

``` bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

after the installation is completed, reopen the terminal and write:

``` bash
brew install ghostscript
```

## Examples

``` r
library(ghostpdf)

#We'll generate some pdf documents for the examples
colors <- rainbow(9)
for (i in 1:9){
  pdf(paste0("Example",i,".pdf"), width = 6, height = 4)
  plot(x = rnorm(100), y = rnorm(100), col = colors[i], main = paste0("Page ", i))
  dev.off()
}
```

### Merging pdfs

You can merge multiple pdf files into a single file:

``` r
#Merge them
pdf_merge(output_file = "All_pdfs.pdf")
#> NULL

#Or merge some of them:
pdf_merge(c("Example1.pdf", "Example6.pdf"), output_file = "Some_pdfs.pdf")
#> NULL
```

### Extracting pages from a pdf

One can extract some of the pages from a pdf:

``` r
#Extract from page 2 to 4
pdf_extract("All_pdfs.pdf", first_page = 2, last_page = 4, 
            output_file = "All_pdfs_subset.pdf")

#Or pages 1, 5 and 8
pdf_extract("All_pdfs.pdf", page_sequence = c(1,5,8),
          output_file = "Other_pdfs_subset.pdf")
```

### Shrinking a pdf

You can reduce the size of a pdf with `pdf_shrink` which enables
compression and other memory-saving tricks:

``` r
#Check current file size:
original_size <- file.info("All_pdfs_subset.pdf")["size"]

#Shrink file
pdf_shrink("All_pdfs_subset.pdf")
#> NULL

#Check new file size
reduced_size <- file.info("All_pdfs_subset.pdf")["size"]

#Verify size is smaller
#in this case there isn't much to gain because the pdf was small by itself
#in large pdfs this is useful
message(paste0("Shrank pdf from ", original_size, " to ", reduced_size))
#> Shrank pdf from 29518 to 27350
```

### PDF to image

Transform a pdf (or select) into images in any of the following formats:
`png`, `tiff`, `jpeg`, `fax`, `pcx`, `bmp`, `psd`:

``` r
#Transform a pdf into png
pdf_to_image("Some_pdfs.pdf", output_file = "cool_plots.png")

#Or sections of another pdf
pdf_to_image("All_pdfs.pdf", first_page = 4, last_page = 7, output_file = "cooler_plots.jpg")

#Sections can also be specified with a vector
pdf_to_image("All_pdfs.pdf", page_sequence = c(1,5,8,9), 
             output_file = "cooler_plots.tiff")

#You can also specify the imagedevice for example to grayscale
pdf_to_image("All_pdfs.pdf", page_sequence = c(1:4, 7),
             image_device = "pnggray",
             output_file = "gray_plots.png")

#Additional options can be passed in additional_flags e.g. transparent background
pdf_to_image("All_pdfs.pdf", page_sequence = 2,
             image_device = "pngalpha",
             output_file = "nbg.png",
             additional_flags = c("-dBackgroundColor=16#fffff",
                                  "-dDownScaleFactor=3"))
```

### Encrypt / decrypt pdf

You can encrypt a pdf file with:

``` r
pdf_encrypt("Example1.pdf", password = "mypassword")
#> NULL
```

To create a decrypted copy:

``` r
pdf_decrypt("Example1.pdf", password = "mypassword")
#> NULL
```

### More

I would like to eventually add same functionality for eps files and to
transform between images and image to pdf.

### Troubleshooting

-   **Can???t find ghostscript on windows**
    1)  I would recommend executing R as administrator. That???s the
        easier (unsafe) way.
    2)  Open `cmd` and write `where gswin64c` or `where gswin32c`. One
        of them should work. The path returned has to be used as
        `gs_path` in functions:

``` r
pdf_merge(output_file = "All_pdfs.pdf", gs_path = "C:/Program Files/gs/gs9.56.1/bin/gswin64c.exe")
```
