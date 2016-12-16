#!/usr/bin/Rscript

# load libraries
# install.packages("optparse", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(optparse)
library(data.table)

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

# end set interface

# get phylogeny from one of maps
phylogeny <- read.table(text=readLines(opt$map1)[count.fields(opt$map1, sep="\t") == 3], sep="\t", stringsAsFactors=F)
phylogeny <- phylogeny[-1,]    # delete first row
colnames(phylogeny) = c("ps", "psti", "psname")

# change the psname to be more readable
phylogeny$psname <- sub("[^:]+:\\s+", "", phylogeny$psname, perl=T)

# get map content from files
map1 <- read.table(text=readLines(opt$map1)[count.fields(opt$map1, sep="\t") == 4], sep="\t", stringsAsFactors=F)
map2 <- read.table(text=readLines(opt$map2)[count.fields(opt$map2, sep="\t") == 4], sep="\t", stringsAsFactors=F)

# map setup
colnames(map1) = c("prot_id", "ps", "psti", "psname")
map1$psname <- sub("[^:]+:\\s+", "", map1$psname, perl=T)
colnames(map2) = c("prot_id", "ps", "psti", "psname")
map2$psname <- sub("[^:]+:\\s+", "", map2$psname, perl=T)

# create a grid to be used later to store results of joins
my_grid <- matrix(data=NA, nrow=nrow(phylogeny), ncol=nrow(phylogeny))
rownames(my_grid) <- paste("map1_ps", 1:nrow(my_grid), sep="")
colnames(my_grid) <- paste("map2_ps", 1:ncol(my_grid), sep="")

# convert data.frames to data.tables to be usable with data.table
mapa1 = as.data.table(map1)
mapa2 = as.data.table(map2)
setkey(mapa1,prot_id)   # index mapa1 on prot_id column
setkey(mapa2,prot_id)

# do the joins
# SELECT COUNT(ful.prot_id)
# FROM bacillus.bsfull3_map AS ful
# INNER JOIN bacillus.bscdhit3_map AS cd ON cd.prot_id = ful.prot_id
# WHERE cd.phylostrata = 1 AND ful.phylostrata = 1;
for (map1 in phylogeny$ps) {
    for (map2 in phylogeny$ps) {
        #print(my_grid[map1,map2])
        my_grid[map1,map2] <- mapa1[mapa1$ps==map1][mapa2[mapa2$ps==map2], NROW(prot_id), nomatch=0, on = "prot_id"]
        #print(my_grid[map1,map2])
    }
}

# SYNOPSIS

# ./CompareMaps.R --map1 file --map2 file -o out

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



















































