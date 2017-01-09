#!/usr/bin/Rscript

# helping functions to load and install packages if required
## automatic package installation

load_package <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE, quietly = TRUE) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE, repos='http://cran.rstudio.com/')
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

#  Then try/install packages...
# load_package( c("ggplot2" , "reshape2" , "data.table" ) )

# customized heatmap function
my_heatmap <- function (x, Rowv = NULL, Colv = if (symm) "Rowv" else NULL, 
    distfun = dist, hclustfun = hclust, reorderfun = function(d, 
        w) reorder(d, w), add.expr, symm = FALSE, revC = TRUE, scale = c("row", "column", "none"), na.rm = TRUE, 
    margins = c(5, 5), ColSideColors, RowSideColors, cexRow = 0.2 + 
        1/log10(nr), cexCol = 0.2 + 1/log10(nc), labRow = NULL, 
    labCol = NULL, main = NULL, xlab = NULL, ylab = NULL, keep.dendro = FALSE, 
    verbose = getOption("verbose"), ...) 
{
    # create copy for printing values
    my_org <- x
    
    #normal
    scale <- if (symm && missing(scale)) 
        "none"
    else match.arg(scale)
    if (length(di <- dim(x)) != 2 || !is.numeric(x)) 
        stop("'x' must be a numeric matrix")
    nr <- di[1L]
    nc <- di[2L]
    if (nr <= 1 || nc <= 1) 
        stop("'x' must have at least 2 rows and 2 columns")
    if (!is.numeric(margins) || length(margins) != 2L) 
        stop("'margins' must be a numeric vector of length 2")
    doRdend <- !identical(Rowv, NA)
    doCdend <- !identical(Colv, NA)
    if (!doRdend && identical(Colv, "Rowv")) 
        doCdend <- FALSE
    if (is.null(Rowv)) 
        Rowv <- rowMeans(x, na.rm = na.rm)
    if (is.null(Colv)) 
        Colv <- colMeans(x, na.rm = na.rm)
    if (doRdend) {
        if (inherits(Rowv, "dendrogram")) 
            ddr <- Rowv
        else {
            hcr <- hclustfun(distfun(x))
            ddr <- as.dendrogram(hcr)
            if (!is.logical(Rowv) || Rowv) 
                ddr <- reorderfun(ddr, Rowv)
        }
        if (nr != length(rowInd <- order.dendrogram(ddr))) 
            stop("row dendrogram ordering gave index of wrong length")
    }
    else rowInd <- 1L:nr
    if (doCdend) {
        if (inherits(Colv, "dendrogram")) 
            ddc <- Colv
        else if (identical(Colv, "Rowv")) {
            if (nr != nc) 
                stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
            ddc <- ddr
        }
        else {
            hcc <- hclustfun(distfun(if (symm) 
                x
            else t(x)))
            ddc <- as.dendrogram(hcc)
            if (!is.logical(Colv) || Colv) 
                ddc <- reorderfun(ddc, Colv)
        }
        if (nc != length(colInd <- order.dendrogram(ddc))) 
            stop("column dendrogram ordering gave index of wrong length")
    }
    else colInd <- 1L:nc
    x <- x[rowInd, colInd]
    labRow <- if (is.null(labRow)) 
        if (is.null(rownames(x))) 
            (1L:nr)[rowInd]
        else rownames(x)
    else labRow[rowInd]
    labCol <- if (is.null(labCol)) 
        if (is.null(colnames(x))) 
            (1L:nc)[colInd]
        else colnames(x)
    else labCol[colInd]
    
    # changed to color relative values
    if (scale == "row") {
        maxx <- suppressWarnings(apply(x, 1L, max, na.rm = T))
        x <- sweep(x, 1L, maxx, "/", check.margin = FALSE)
    }
    # changed to color relative values
    else if (scale == "column") {
        maxx <- suppressWarnings(apply(x, 2L, max, na.rm = T))
        x <- sweep(x, 2L, maxx, "/", check.margin = FALSE)
    }
    lmat <- rbind(c(NA, 3), 2:1)
    lwid <- c(if (doRdend) 1 else 0.05, 4)
    lhei <- c((if (doCdend) 1 else 1) + if (!is.null(main)) 0.2 else 0, 
        4)
    if (!missing(ColSideColors)) {
        if (!is.character(ColSideColors) || length(ColSideColors) != 
            nc) 
            stop("'ColSideColors' must be a character vector of length ncol(x)")
        lmat <- rbind(lmat[1, ] + 1, c(NA, 1), lmat[2, ] + 1)
        lhei <- c(lhei[1L], 0.2, lhei[2L])
    }
    if (!missing(RowSideColors)) {
        if (!is.character(RowSideColors) || length(RowSideColors) != 
            nr) 
            stop("'RowSideColors' must be a character vector of length nrow(x)")
        lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 1), 
            1), lmat[, 2] + 1)
        lwid <- c(lwid[1L], 0.2, lwid[2L])
    }
    lmat[is.na(lmat)] <- 0
    if (verbose) {
        cat("layout: widths = ", lwid, ", heights = ", lhei, 
            "; lmat=\n")
        print(lmat)
    }
    dev.hold()
    on.exit(dev.flush())
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    layout(lmat, widths = lwid, heights = lhei, respect = TRUE)
    if (!missing(RowSideColors)) {
        par(mar = c(margins[1L], 0, 0, 0.5))
        image(rbind(if (revC) 
            nr:1L
        else 1L:nr), col = RowSideColors[rowInd], axes = FALSE)
    }
    if (!missing(ColSideColors)) {
        par(mar = c(0.5, 0, 0, margins[2L]))
        image(cbind(1L:nc), col = ColSideColors[colInd], axes = FALSE)
    }
    par(mar = c(margins[1L], 0, 0, margins[2L]))
    if (!symm || scale != "none") 
        x <- t(x)
    if (revC) {
        iy <- nr:1
        if (doRdend) 
            ddr <- rev(ddr)
        x <- x[, iy]
        # switch rows
        my_org <- my_org[iy,]
    }
    else iy <- 1L:nr
    image(1L:nc, 1L:nr, x, xlim = 0.5 + c(0, nc), ylim = 0.5 + 
        c(0, nr), axes = FALSE, xlab = "", ylab = "", ...)
    #changed 1 to 3
    axis(3, 1L:nc, labels = labCol, las = 2, line = -0.5, tick = 0, 
        cex.axis = cexCol)
    if (!is.null(xlab)) 
        mtext(xlab, side = 3, line = margins[1L] )
    axis(2, iy, labels = labRow, las = 2, line = -0.5, tick = 0, 
        cex.axis = cexRow)
    
    # printing of values
    for (xcord in 1:ncol(my_org)) {
        for (ycord in 1:nrow(my_org)) {
            if (is.na(my_org[ycord,xcord])) {
                text(xcord, ycord, '-')
            }
            else {
                text(xcord, ycord, my_org[ycord,xcord])
            }
        }
    }
            
    # try printing box around heatmap
    box(lty="solid", col="black")
    
    # try printing grid around values
    grid(nrow(my_grid), ncol(my_grid))
    
    #changed 4 to 2
    if (!is.null(ylab)) 
        mtext(ylab, side = 2, line = margins[2L] )
    if (!missing(add.expr)) 
        eval.parent(substitute(add.expr))
    par(mar = c(margins[1L], 0, 0, 0))
    if (doRdend) 
        plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
    else frame()
    par(mar = c(0, 0, if (!is.null(main)) 1 else 0, margins[2L]))
    if (doCdend) 
        plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none")
    else if (!is.null(main)) 
        frame()
    if (!is.null(main)) {
        par(xpd = NA)
        # changed cex.main from 1.5 * to 1
        title(main, cex.main = 1 * op[["cex.main"]])
    }
    invisible(list(rowInd = rowInd, colInd = colInd, Rowv = if (keep.dendro && 
        doRdend) ddr, Colv = if (keep.dendro && doCdend) ddc))
}


#------------------------------------------------------------------------------------
# start of program

# load libraries
# install.packages("optparse", dependencies=TRUE, repos='http://cran.rstudio.com/')
load_package("optparse")
load_package("data.table")
load_package("xlsx")

# set interface
option_list <- list(
    make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
        help="Should the program print extra stuff out? [default FALSE]"),
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

# get map names from files
map1_name <- sub("\\A(.+)\\.phmap_names", "\\1", basename(opt$map1), perl=T)
map2_name <- sub("\\A(.+)\\.phmap_names", "\\1", basename(opt$map2), perl=T)

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

# plot a heatmap
filename <- paste(map1_name, "_vs_", map2_name, "_comp", sep="")
file_path <- file.path(normalizePath(opt$output), filename)
pdf(file=paste(file_path, ".pdf", sep=""), paper="a4r", colormodel = "cmyk", family="ArialMT")
my_heatmap(my_grid, Colv = NA, Rowv = NA, scale="row", col=rev(heat.colors(20)),
    main=paste(map1_name, "(1)_vs_", map2_name, "(2)_comparison", sep=""), xlab=map2_name, ylab=map1_name)
dev.off()

png(file=paste(file_path, ".png", sep=""), units="cm", width=18, height=16, bg="white", res=600)
my_heatmap(my_grid, Colv = NA, Rowv = NA, scale="row", col=rev(heat.colors(20)),
    main=paste(map1_name, "(1)_vs_", map2_name, "(2)_comparison", sep=""), xlab=map2_name, ylab=map1_name)
dev.off()

# write grid to xlsx
write.table(my_grid,  file=paste(file_path, ".tsv", sep=""), sep="\t")
write.xlsx(my_grid, file=paste(file_path, ".xlsx", sep=""), sheetName=paste(map1_name, "_vs_", map2_name, sep=""), col.names=TRUE, row.names=TRUE, append=FALSE, showNA=FALSE)







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



















































