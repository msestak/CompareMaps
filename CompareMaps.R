#!/usr/bin/Rscript

# load libraries
# install.packages("optparse", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(optparse)

# set interface
option_list <- list(
    make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
        help="Should the program print extra stuff out? [default]"),
    make_option(c("-q", "--quietly"), action="store_false", dest="verbose",
        help="Be quiet"),
    make_option(c("-m1", "--map1"),   action = "store", type="character", default=FALSE,
        help="File name of first map file"),
    make_option(c("-m2", "--map2"),   action = "store", type="character", default=FALSE,
        help="File name of second map file"),
    make_option(c("-o", "--output"),  action = "store", type="character", default=FALSE,
        help="Output of map comaparison")
)

parser <- OptionParser(usage="%prog [-map1 file -map2 file -o file]", option_list=option_list)
args <- parse_args(parser, positional_arguments = 0)
opt <- args$options

if ( opt$verbose ) {
    print(opt, stdout())
}
if ( opt$verbose ) {
    write("writing some verbose output to standard error...\n", stderr())
}

# set interface

# Read table
#table <- read.table(opt$input, header=TRUE, sep="\t")
#r <- read.table(text=readLines("./t/data/bsfull3.phmap_names")[count.fields("./t/data/bsfull3.phmap_names", sep="\t") == 4], sep="\t")

# SYNOPSIS

# ./calculate_hyper.R -i ./t/data/disease.txt -o ./t/data/disease_hyper.txt

# DESCRIPTION

# This script requires input file with this header:
# phylostrata\tphylostrata_name\tFunctional term\tquant\tsample\thit\ttotal
# after header paste your values like this:
# 1\tCellular organisms\tdisease_genes\t970\t8285\t1760\t22845
# ...

# it will then calculate log-odds, hypergeometric test, FDR and Bonferroni correction and write it to tsv file.

# AUTHOR

# <Martin Sebastijan Šestak> (<sestakm@yahoo.com>)

# LICENCE AND COPYRIGHT

# Copyright (c) <2016> <Martin Sebastijan Šestak> (<sestakm@yahoo.com>). All rights reserved.

# This module is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself. See L<perlartistic>.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 



















































