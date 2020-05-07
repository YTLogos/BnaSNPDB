#' Title
#'
#' @param chr
#' @param start
#' @param end
#' @param accession
#' @param muttype
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
extractsnp <- function(chr = "A10", start = 14990000, end = 15010000, accession = NULL, muttype = NULL, maf = 0.05, ...) {
  if (is.null(chr)) {
    return(NULL)
  } else {
    chr.size <- chrinfo$size[chrinfo$chr == chr]
    start <- max(0, start)
    end <- min(end, chr.size)
    start <- as.numeric(start)
    end <- as.numeric(end)
    input.reg <- IRanges(start, end)
    snp.chr <- snp.list[snp.list$chr == chr, ]
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
    # snp.pos <- as.numeric(rownames(get_snp))
    rownames(get_snp) <- str_c(chr, rownames(get_snp), sep = "_")

    if (!is.null(accession)) {
      accession <- sapply(accession, function(x) {
        if (x %in% c("Winter", "Semi-winter", "Spring", "Core")) {
          var <- readLines(paste0("./data/Other_data/", x, ".var.txt"))
          return(var)
        } else {
          return(x)
          # message("Warnning!!! Please enter the right accession ...")
        }
      })
    }

    accession <- unique(unlist(accession))
    if (!is.null(accession) && length(accession >= 2)) {
      get_snp <- get_snp[, colnames(get_snp) %in% accession, drop = F]
    }

    eff.RData <- paste0("./data/SNP_data/", chr, ".snpeff.RData")
    load(eff.RData)
    if (!is.null(muttype) && length(muttype) >= 1 && length(muttype) != 12) {
      snpeff.info <- snpeff[snpeff$eff %in% muttype, , drop = F]
      get_snp <- get_snp[rownames(get_snp) %in% snpeff.info$id, , drop = F]
    }
  }
  get_snp <- t(get_snp)
  get_snp <- NAM::snpQC(gen = get_snp, MAF = maf, remove = T, impute = F)
  get_snp <- t(get_snp)
  snp.pos <- as.numeric(str_sub(rownames(get_snp), start = 5))
  snp.allele <- snpeff[snpeff$id %in% rownames(get_snp), , drop = F]
  allele <- as.matrix(get_snp)
  snp.allele <- cbind(snp.allele, allele)
  return(list(get_snp, snp.pos, snp.allele))
}
