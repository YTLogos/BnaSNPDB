#' Title
#'
#' @param chr
#' @param start
#' @param end
#' @param snp.h
#' @param gene
#' @param ld.y
#' @param ld.w
#' @param flip
#' @param accession
#' @param mutType
#' @param snpSites
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ldheatmap <- function(chr = "C07", start = 31077219, end = 31089031, snp.h = c(1),
                      gene = FALSE, ld.y = 0.85, ld.w = 0.91, flip = FALSE, accession = NULL,
                      mutType = NULL, snpSites = NULL, ...) {
  chr.size <- chrinfo$size[chrinfo$chr == chr]
  start <- max(0, start)
  end <- min(end, chr.size)
  start <- as.numeric(start)
  end <- as.numeric(end)
  snp.data <- extractsnp(chr = chr, start = start, end = end, accession = accession, muttype = mutType)
  snp.pos <- snp.data[[2]]
  snp.mat <- t(as.matrix(snp.data[[1]]))
  snp.mat <- as(snp.mat, "SnpMatrix")

  if (!flip) {
    LDheatmap(snp.mat, snp.pos, flip = F, title = NULL, ...)
  } else if (flip) {
    if (!gene) {
      LDheatmap(snp.mat, snp.pos, flip = T, title = NULL, ...)
    } else {
      ld <- LDheatmap(snp.mat, snp.pos, flip = T, title = NULL, ...)
      p1 <- genestru.viz(chr = chr, start = start, end = end)
      plot.new()
      ld2 <- LDheatmap.addGrob(ld, rectGrob(gp = gpar(col = "white")), height = .3)
      pushViewport(viewport(x = 0.483, y = ld.y, width = ld.w, height = .3))
      grid.draw(ggplotGrob(p1))
    }
  }

  # if(gene){
  #   ld <- LDheatmap(snp.mat, snp.pos, flip = T, title = NULL,...)
  #   p1 <- genestru.viz(chr=chr,start=start, end=end)
  #   plot.new()
  #   ld2 <- LDheatmap.addGrob(ld, rectGrob(gp=gpar(col="white")), height = .3)
  #   pushViewport(viewport(x=0.483, y=ld.y, width = ld.w, height = .3))
  #   grid.draw(ggplotGrob(p1))
  # }else{
  #   if(flip){
  #     LDheatmap(snp.mat, snp.pos, flip = T, title = NULL,...)
  #   }else{
  #     LDheatmap(snp.mat, snp.pos, flip = F, title = NULL, SNP.name = snp.mat[,1][snp.h])
  #   }
  # }
}
