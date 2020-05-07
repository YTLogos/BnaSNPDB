diversity <- function(chr = "A10", start = 14990000, end = 15100000, groups = c("Winter", "Spring"), numerator = "Winter", denominator = "Spring", step = 10, muttype = NULL, maf = 0.05, show.gene = FALSE, ...) {
  data <- extractallele(chr = chr, start = start, end = end, muttype = muttype, maf = maf)[[1]]
  data.L <- gsub("/.+", "", data)
  data.R <- gsub(".+/", "", data)
  colnames(data.L) <- paste0(colnames(data.L), ".L")
  colnames(data.R) <- paste0(colnames(data.R), ".R")
  dat.mat <- t(cbind(data.L, data.R))
  dat.mat[is.na(dat.mat)] <- "-"
  dat.bin <- as.DNAbin(dat.mat)
  div.group <- lapply(unique(c(groups, numerator, denominator)), function(x) {
    x.accession <- readLines(paste0("./data/Other_data/", x, ".var.txt"))
    x.accession.L <- paste0(x.accession, ".L")
    x.accession.R <- paste0(x.accession, ".R")
    dat <- dat.bin[rownames(dat.bin) %in% c(x.accession.L, x.accession.R), ]

    nuc.div <- lapply(seq(1, ncol(dat.bin), by = step), function(i) {
      dat.i <- dat[, i:min(i + step - 1, ncol(dat.bin))]

      div <- nuc.div(dat.i, pairwise.deletion = TRUE)

      return(div)
    })
    nuc.div.df <- do.call(rbind, nuc.div)
    nuc.div.df <- data.frame(nuc.div.df, stringsAsFactors = FALSE)
  })

  div.group.df <- do.call(cbind, div.group)
  names(div.group.df) <- unique(c(groups, numerator, denominator))
  dat.pos <- as.numeric(str_sub(colnames(dat.mat), start = 5))
  nuc.pos <- dat.pos[seq(1, ncol(dat.mat), by = step)][1:nrow(div.group.df)]
  div.group.df$pos <- nuc.pos

  diVTxt <<- div.group.df
  diVTxt <<- diVTxt[, c(ncol(diVTxt), 1:(ncol(diVTxt) - 1))]
  names(diVTxt)[1] <<- "position"

  div.group.df.1 <- div.group.df[, c("pos", groups)]
  div.group.df.2 <- div.group.df[, c("pos", numerator, denominator)]

  div.group.df.1.long <- gather(div.group.df.1, group, diversity, -pos)
  div.group.df.2.long <- gather(div.group.df.2, group, diversity, -pos)
  nuc.gene.info <- geneinfo
  nuc.gene.info <- nuc.gene.info[nuc.gene.info$chr == chr &
    nuc.gene.info$start >= as.numeric(start) &
    nuc.gene.info$end <= as.numeric(end), ]

  p1 <- ggplot(div.group.df.1.long) +
    geom_line(aes(x = pos, y = diversity, color = group)) +
    xlab("") +
    ylab("Nucleotide diversity") +
    theme_classic() +
    ylim(-0.15, NA) +
    theme(legend.title = element_blank()) +
    theme(legend.position = "top")
  if (nrow(nuc.gene.info) >= 1 & show.gene) {
    p1 <- p1 + geom_rect(aes(xmin = start, xmax = end, ymin = -0.03, ymax = -0.05), fill = "grey40", data = nuc.gene.info) + geom_text(aes(x = (start + end) / 2, y = -0.12, label = id), angle = 45, size = 3, data = nuc.gene.info)
  }

  p1 <- p1 + theme(
    axis.ticks.x = element_blank(), axis.text.x = element_blank(),
    axis.line.x = element_blank(), legend.text = element_text(size = 14, face = "bold"), axis.text.y = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 16, face = "bold")
  )

  div.group.df.2$value <- div.group.df.2[, numerator] / div.group.df.2[, denominator]

  p2 <- ggplot(div.group.df.2) +
    geom_line(aes(x = pos, y = value)) +
    xlab("genomic position (bp)") +
    ylab(paste0(numerator, "/", denominator)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12, face = "bold"), axis.title = element_text(size = 16, face = "bold"), axis.text.y = element_text(size = 12, face = "bold"))
  p <- p1 / p2
  return(p)
  # gp1 <- ggplotGrob(p1)
  # gp2 <- ggplotGrob(p2)
  # maxWidth <- grid::unit.pmax(gp1$widths[2:5], gp2$widths[2:5])
  # gp1$widths[2:5] <- as.list(maxWidth)
  # gp2$widths[2:5] <- as.list(maxWidth)
  #
  # return(list(gp1, gp2))
}
