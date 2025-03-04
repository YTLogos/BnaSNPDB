snp_dis_theme <- theme(
  plot.background = element_blank(),
  panel.background = element_blank(),
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_text(angle = 0, vjust = 0.5, face = "bold"),
  legend.position = "right",
  axis.ticks = element_blank(),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 12)
)

snp_distribution <- function(snp_data, ...) {
  # 获取唯一的type类别并确定数量
  types <- as.character(unique(snp_data$type))
  group_num <- length(types)
  
  # 计算每个类别的样本数量
  heights <- sapply(types, function(t) {
    snp_data %>%
      dplyr::filter(type == t) %>%
      dplyr::select(accession) %>%
      dplyr::distinct() %>%
      nrow()
  })
  
  # 生成每个类别的绘图对象
  plot_list <- lapply(seq_along(types), function(i) {
    t <- types[i]
    data_subset <- dplyr::filter(snp_data, type == t)
    
    ggplot2::ggplot(data_subset, aes(x = snppos, y = accession, fill = factor(allele))) +
      ggplot2::geom_tile(stat = "identity", width = 0.96) +
      ggplot2::ylab(paste0(t, "\n(n=", heights[i], ")")) +
      ggplot2::scale_fill_manual(
        values = c("#EBE9CB", "#CACAE7", "#E93857"),
        name = "Allele \ncode",
        breaks = c("0", "1", "2")
      ) +
      snp_dis_theme
  })
  
  # 计算相对高度（按原始比例的1%调整）
  heights_rel <- round(heights / 100, 2)
  
  # 组合所有图形
  ggpubr::ggarrange(
    plotlist = plot_list,
    ncol = 1,
    nrow = group_num,
    heights = heights_rel,
    common.legend = TRUE,
    legend = "right",
    align = "hv"
  )
}