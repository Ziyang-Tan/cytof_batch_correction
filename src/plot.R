library(ggplot2)

umapPlot = function(umap, method, colored_by = c("batch", "level1", "level2")){
  if(colored_by == "batch"){
    df = data.frame(UMAP1 = umap$layout[,1],
                    UMAP2 = umap$layout[,2],
                    Batch = factor(Label$batch))
    gp = ggplot(df, aes(UMAP1, UMAP2, color = Batch)) +
      geom_point(size = 1, alpha = 0.5, stroke = 0) +
      scale_color_manual(values = c('#ff71ce', '#01cdfe')) +
      ggtitle(method) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.title = element_text(size = 15)) +
      guides(color = guide_legend(override.aes = list(alpha = 1, size = 5)))
  }
  if(colored_by == "level1"){
    df = data.frame(UMAP1 = umap$layout[,1],
                    UMAP2 = umap$layout[,2],
                    Level1 = Label$level1)
    gp = ggplot(df, aes(UMAP1, UMAP2, color = Level1)) +
      geom_point(size = 1, alpha = 0.5, stroke = 0) +
      ggtitle(method) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.title = element_text(size = 15)) +
      guides(color = guide_legend(override.aes = list(alpha = 1, size = 5)))
  }
  if(colored_by == "level2"){
    df = data.frame(UMAP1 = umap$layout[,1],
                    UMAP2 = umap$layout[,2],
                    Level2 = Label$level2)
    gp = ggplot(df, aes(UMAP1, UMAP2, color = Level2)) +
      geom_point(size = 1, alpha = 0.5, stroke = 0) +
      ggtitle(method) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            legend.title = element_text(size = 15)) +
      guides(color = guide_legend(override.aes = list(alpha = 1, size = 5)))
  }
  return(gp)
}

layoutPlot = function(layout, labelData, title, colorBy){
  gp =  ggplot(cbind(data.frame(layout1 = layout[,1], 
                          layout2 = layout[,2]), 
               labelData),
               aes(layout1, layout2)) +
        aes_string(color = colorBy) +
        ggtitle(title) +
        geom_point(size = 1, alpha = 1, stroke = 0.3)
  return(gp)
}





