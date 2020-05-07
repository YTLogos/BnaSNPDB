#' Title
#'
#' @param chr
#' @param start
#' @param end
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
genestru.viz <- function(chr = "C07", start = 31077219, end = 31089031, ...) {
  chr.size <- chrinfo$size[chrinfo$chr == chr]
  start <- max(0, start)
  end <- min(end, chr.size)
  start <- as.numeric(start)
  end <- as.numeric(end)
  gff.chr <- gffinfo %>% dplyr::filter(chr == {{ chr }}, type == "mRNA")
  gene <- gff.chr[gff.chr$start >= start & gff.chr$end <= end, ]
  gene <- gffinfo[gffinfo$id %in% gene$id & gffinfo$type != "mRNA", ][, -c(3, 7)]
  if (length(unique(gene$id)) <= 0) {
    stop("There is no gene selected!")
  }
  if (length(unique(gene$id)) <= 3) {
    p <- ggplot(gene, aes(xmin = start, xmax = end, y = id, fill = type, label = type)) +
      geom_gene_arrow(arrowhead_height = unit(3, "mm"), arrowhead_width = unit(1, "mm")) +
      geom_gene_label(align = "left") +
      facet_wrap(~id, scales = "free", ncol = 1) +
      scale_fill_brewer(palette = "Set3") +
      theme_genes() +
      ylab(NULL) +
      theme(legend.title = element_blank())
  } else {
    message("More than 3 genes were selected! Just show 3 genes structure!")
    gene_id <- as.character(sample(unique(gene$id), 3))
    gene <- gene[gene$id %in% gene_id, , drop = F]
    p <- ggplot(gene, aes(xmin = start, xmax = end, y = id, fill = type, label = type)) +
      geom_gene_arrow(arrowhead_height = unit(3, "mm"), arrowhead_width = unit(1, "mm")) +
      geom_gene_label(align = "left") +
      facet_wrap(~id, scales = "free", ncol = 1) +
      scale_fill_brewer(palette = "Set3") +
      theme_genes() +
      ylab(NULL) +
      theme(legend.title = element_blank())
  }
  return(p)
}
