ll <- LDheatmap(snp.mat, snp.pos, flip = T, title = NULL, color = rgb.palette(100))
p1 <- ggplot(gene,aes(xmin = start, xmax = end, y = molecule, fill = gene, label = gene)) +
  geom_gene_arrow(arrowhead_height = unit(3, "mm"), arrowhead_width = unit(1, "mm")) +
  geom_gene_label(align = "left")+
  facet_wrap(~ molecule, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  theme_genes()+
  ylab(NULL)+
  theme(legend.title = element_blank())
plot.new()
ld<-LDheatmap.addGrob(ll, rectGrob(gp = gpar(col = "white")),height = .34)
pushViewport(viewport(x = 0.483, y= 0.8, width = 0.75,height = .4))
grid.draw(ggplotGrob(p1))

