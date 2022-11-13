# grwat 0.0.3

-   redundant references to old methods are removed from DESCRIPTION;

# grwat 0.0.2

This release addresses the issues raised by Gregor Seyer for initial 0.0.1 version:

-   backticks around `C++17` are removed in DESCRIPTION file;
-   references are added to all methods in DESCRIPTION file using `Authors (Year) <link>` format;
-   `\value` is added to the `gr_report()` and `gr_set_locale()` functions, for all functions ensured that the structure and the meaning of the result is explained;
-   All `... = T/F` statements are replaced with full `... = TRUE/FALSE` statements;
-   `\donttest{}` directives are removed from `inst/examples/*.R` files, and added to the corresponding `man/*.Rd` files.

# grwat 0.0.1

-   the first stable version of the package ready for CRAN submission.
