# globals.R
# Suppress R CMD check NOTE for .data pronoun used in dplyr pipelines.
# .data is a special pronoun from rlang, re-exported by dplyr.
# This declaration is the standard CRAN-accepted pattern.
utils::globalVariables(".data")
