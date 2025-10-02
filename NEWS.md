# grwat (development version)

-   promoted to R 4.1 as minimal R version
-   replaced all internal magrittr `%>%` pipes with native `|>` pipes
-   updated tests to support ggplot 4.0 version
-   added `yrange` parameter to `gr_plot_sep()` function
-   added `gr_to_pardf()` and `gr_from_pardf()` functions to convert separation parameters to and from tabular form

# grwat 0.0.4

-   reduced the time of all examples to less than 5s.
-   added `year_min` and `year_max` parameters to `gr_summarize()` function

# grwat 0.0.3

-   removed examples and tests with non-ASCII symbols (to address CRAN issue raised by Brian Ripley via e-mail);
-   renamed `baseflow_lyne()` and `baseflow_chapman()` functions in C++ sources into `quickflow_lyne()` and `quickflow_chapman()` to address Github issue 13 raised by Kaye-HQ;
-   added support for relative paths in `gr_report()` output file;
-   fixed incorrect application of `grad2` parameter in low-water period;
-   replaced `progress` and `crayon` with `cli` package;
-   moved `knitr`, `kableExtra` and `rmarkdown` packages to suggests (as reports may not be used);
-   removed `.data$` from tidyselect expressions;
-   removed redundant references to old methods from DESCRIPTION;

# grwat 0.0.2

This release addresses the issues raised by Gregor Seyer for initial 0.0.1 version:

-   backticks around `C++17` are removed in DESCRIPTION file;
-   references are added to all methods in DESCRIPTION file using `Authors (Year) <link>` format;
-   `\value` is added to the `gr_report()` and `gr_set_locale()` functions, for all functions ensured that the structure and the meaning of the result is explained;
-   All `... = T/F` statements are replaced with full `... = TRUE/FALSE` statements;
-   `\donttest{}` directives are removed from `inst/examples/*.R` files, and added to the corresponding `man/*.Rd` files.

# grwat 0.0.1

-   the first stable version of the package ready for CRAN submission.
