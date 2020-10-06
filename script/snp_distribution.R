snp_dis_theme <- theme(plot.background = element_blank()) +
  theme(panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.line = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, face = "bold")) +
  theme(legend.position = "right") +
  theme(axis.ticks = element_blank()) +
  theme(legend.text = element_text(size = 10)) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

#' Title
#'
#' @param snp_data 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
snp_distribution <- function(snp_data, ...) {
  types <- as.character(unique(snp_data$type))
  group <- length(types)
  if (group == 1) {
    height <- snp_data %>%
      dplyr::filter(type == types[1]) %>%
      select(accession) %>%
      unique() %>%
      nrow()
    p <- ggplot(snp_data, aes(x = snppos, y = accession, fill = allele)) +
      geom_tile(stat = "identity", width = 0.96) +
      ylab(paste0(types[1], "\n", "(n=", height[1], ")")) +
      scale_fill_manual(
        values = c("#EBE9CB", "#CACAE7", "#E93857"),
        name = "Allele \ncode", breaks = c("0", "1", "2")
      ) +
      snp_dis_theme
  }
  if (group == 2) {
    snp_list <- list()
    p_list <- list()
    height <- vector(mode = "numeric", length = length(types))
    for (i in 1:length(types)) {
      height[i] <- snp_data %>%
        dplyr::filter(type == types[i]) %>%
        select(accession) %>%
        unique() %>%
        nrow()
    }
    heights <- round(height / 100, 2)
    for (i in 1:length(types)) {
      snp_list[[i]] <- snp_data %>% dplyr::filter(type == types[i])
      p_list[[i]] <- ggplot(snp_list[[i]], aes(x = snppos, y = accession, fill = allele)) +
        geom_tile(stat = "identity", width = 0.96) +
        scale_fill_manual(
          values = c("#EBE9CB", "#CACAE7", "#E93857"),
          name = "Allele \ncode", breaks = c("0", "1", "2")
        ) +
        ylab(paste0(types[i], "\n", "(n=", height[i], ")")) +
        snp_dis_theme
    }
    p <- ggpubr::ggarrange(p_list[[1]], p_list[[2]],
      ncol = 1, nrow = 2,
      heights = heights, common.legend = TRUE, legend = "right", align = "hv"
    )
  }
  if (group == 3) {
    snp_list <- list()
    p_list <- list()
    height <- vector(mode = "numeric", length = length(types))
    for (i in 1:length(types)) {
      height[i] <- snp_data %>%
        dplyr::filter(type == types[i]) %>%
        select(accession) %>%
        unique() %>%
        nrow()
    }
    
    h_adjust <- c(height[1],height[3],height[2])
    heights <- round(h_adjust / 100, 2)
    for (i in 1:length(types)) {
      snp_list[[i]] <- snp_data %>% dplyr::filter(type == types[i])
      p_list[[i]] <- ggplot(snp_list[[i]], aes(x = snppos, y = accession, fill = factor(allele))) +
        geom_tile(stat = "identity", width = 0.96) +
        ylab(paste0(types[i], "\n", "(n=", height[i], ")")) +
        scale_fill_manual(
          values = c("#EBE9CB", "#CACAE7", "#E93857"),
          name = "Allele \ncode", breaks = c("0", "1", "2")
        ) +
        snp_dis_theme
    }
    p <- ggpubr::ggarrange(p_list[[1]], p_list[[3]], p_list[[2]],
      ncol = 1, nrow = 3,
      heights = heights, common.legend = TRUE, legend = "right", align = "hv"
    )
  }
  if (group == 4) {
    snp_list <- list()
    p_list <- list()
    height <- vector(mode = "numeric", length = length(types))
    for (i in 1:length(types)) {
      height[i] <- snp_data %>%
        dplyr::filter(type == types[i]) %>%
        select(accession) %>%
        unique() %>%
        nrow()
    }
    heights <- round(height / 100, 2)
    for (i in 1:length(types)) {
      snp_list[[i]] <- snp_data %>% dplyr::filter(type == types[i])
      p_list[[i]] <- ggplot(snp_list[[i]], aes(x = snppos, y = accession, fill = factor(allele))) +
        geom_tile(stat = "identity", width = 0.96) +
        ylab(paste0(types[i], "\n", "(n=", height[i], ")")) +
        scale_fill_manual(
          values = c("#EBE9CB", "#CACAE7", "#E93857"),
          name = "Allele \ncode", breaks = c("0", "1", "2")
        ) +
        snp_dis_theme
    }
    p <- ggpubr::ggarrange(p_list[[1]], p_list[[2]], p_list[[3]], p_list[[4]],
      ncol = 1, nrow = 4,
      heights = heights, common.legend = TRUE, legend = "right", align = "hv"
    )
  }
  if (group == 5) {
    snp_list <- list()
    p_list <- list()
    height <- vector(mode = "numeric", length = length(types))
    for (i in 1:length(types)) {
      height[i] <- snp_adta %>%
        dplyr::filter(type == types[i]) %>%
        select(accession) %>%
        unique() %>%
        nrow()
    }
    heights <- round(height / 100, 2)
    for (i in 1:length(types)) {
      snp_list[[i]] <- snp_data %>% dplyr::filter(type == types[i])
      p_list[[i]] <- ggplot(snp_list[[i]], aes(x = snppos, y = accession, fill = factor(allele))) +
        geom_bar(stat = "identity", width = 0.96) +
        ylab(paste0(types[i], "\n", "(n=", height[i], ")")) +
        scale_fill_manual(
          values = c("#EBE9CB", "#CACAE7", "#E93857"),
          name = "Allele \ncode", breaks = c("0", "1", "2")
        ) +
        snp_dis_theme
    }
    p <- ggpubr::ggarrange(p_list[[1]], p_list[[2]], p_list[[3]], p_list[[4]], p_list[[5]],
      ncol = 1, nrow = 5,
      heights = heights, common.legend = TRUE, legend = "right", align = "hv"
    )
  }
  return(p)
}