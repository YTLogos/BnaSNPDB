#' Title
#'
#' @param chr
#' @param start
#' @param end
#' @param accession
#' @param muttype
#' @param maf
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
extractallele <- function(chr = "A10", start = 14990000, end = 15010000, accession = NULL, muttype = NULL, maf = 0.05, ...) {
  if (is.null(chr)) {
    return(NULL)
  } else {
    chr.size <- chrinfo$size[chrinfo$chr == chr]
    start <- max(0, start)
    end <- min(end, chr.size)
    start <- as.numeric(start)
    end <- as.numeric(end)
    input.reg <- IRanges(start, end)
    snp.chr <- allele.list[allele.list$chr == chr, ]
    snp.reg <- IRanges(start = snp.chr$start, end = snp.chr$end)
    snp.file <- snp.chr$file[unique(queryHits(findOverlaps(snp.reg, input.reg)))]
    snp.file.list <- lapply(snp.file, function(x) {
      load(x)
      return(snp.data.inter.Matrix)
    })

    snp.file.list <- unlist(snp.file.list, recursive = FALSE)
    snp.data <- do.call(rbind, snp.file.list)
    rownames(snp.data) <- str_sub(rownames(snp.data), start = 5)
    get_snp <- snp.data[as.numeric(rownames(snp.data)) >= start & as.numeric(rownames(snp.data)) <= end, , drop = F]

    eff.RData <- paste0("./data/SNP_data/", chr, ".snpeff.RData")
    load(eff.RData)
    allele.info <- snpeff
    rownames(snpeff) <- str_sub(snpeff[, 1], start = 5)
    snpeff <- snpeff[, -c(1, 2, 3)]
    if (!is.null(muttype) && length(muttype) >= 1 && length(muttype) != 12) {
      snpeff.info <- snpeff[snpeff$eff %in% muttype, , drop = F]
      get_snp <- get_snp[rownames(get_snp) %in% rownames(snpeff.info), , drop = F]
    }

    get_snp <- t(get_snp)
    get_snp <- NAM::snpQC(gen = get_snp, MAF = maf, remove = T, impute = F)
    get_snp <- t(get_snp)
    get_snp <- get_snp[order(as.numeric(rownames(get_snp))), ]
    snp.pos <- as.numeric(rownames(get_snp))
    snp.allele <- snpeff[rownames(snpeff) %in% rownames(get_snp), , drop = F]

    snp.allele <- snp.allele[order(as.numeric(rownames(snp.allele))), ]

    snp.allele[, 3] <- paste(snp.allele[, 1], snp.allele[, 2], sep = "/")
    snp.allele[, 1] <- paste(snp.allele[, 1], snp.allele[, 1], sep = "/")
    snp.allele[, 2] <- paste(snp.allele[, 2], snp.allele[, 2], sep = "/")
    colnames(snp.allele)[3] <- "het"
    snp.code <- as.vector(t(snp.allele[rownames(get_snp), ]))

    dat.res.n <- seq_len(nrow(get_snp)) - 1
    dat.res.n <- rep(dat.res.n, each = ncol(get_snp))
    dat.res.n <- matrix(dat.res.n, ncol = ncol(get_snp), byrow = T)
    dat.res <- get_snp + 1 + dat.res.n * 3
    dat.res.mat <- matrix(snp.code[as.matrix(dat.res)], ncol = ncol(get_snp))
    rownames(dat.res.mat) <- rownames(dat.res)
    colnames(dat.res.mat) <- colnames(dat.res)

    accession <- sapply(accession, function(x) {
      if (x %in% c("Winter", "Semi-winter", "Spring", "Core")) {
        var <- readLines(paste0("./data/Other_data/", x, ".var.txt"))
        return(var)
      } else {
        return(x)
      }
    })
    accession <- unique(unlist(accession))
    if (!is.null(accession) && length(accession) >= 2) {
      dat.res.mat <- dat.res.mat[, colnames(dat.res.mat) %in% accession, drop = FALSE]
    }
    rownames(dat.res.mat) <- str_c(chr, rownames(dat.res.mat), sep = "_")
    allele.info <- allele.info[allele.info$id %in% rownames(dat.res.mat), , drop = F]
    allele.info <- cbind(allele.info, dat.res.mat)
    return(list(dat.res.mat, allele.info))
  }
}
