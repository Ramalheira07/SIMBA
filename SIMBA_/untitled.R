p = ggplot() + 
  geom_line(data = daily_data, aes(x = Date, y = Toxinas,color = "blueTste")) +
  geom_line(data = df_trend1, aes(x = Date, y = ProdutorasdeDSP,color = "redteste")) +
  xlab('Dates') +
  ylab('percent.change') + scale_color_manual(name="Legenda", values=c("#35978f"),guide = guide_legend(override.aes=aes(fill=1))) + 
        theme(legend.title = element_text(size=14)) + theme(legend.text = element_text(size = 14))

  df_trend1 <- daily_data2[daily_data2$'Zona de Produção' == "L8", ]