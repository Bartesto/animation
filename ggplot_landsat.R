##Script found that plots landsat in ggplot
##can't work out how to stretch it
##Takes a long time to run and produce one plot

#https://shekeine.github.io/visualization/2014/09/27/sfcc_rgb_in_R/




mb_df <- raster::as.data.frame(b, xy=T)
mb_df <- data.frame(x=mb_df$x, y=mb_df$y, 
                    r=mb_df$l8ut11174m_150615_USG_utm51pre.3, 
                    n=mb_df$l8ut11174m_150615_USG_utm51pre.4, 
                    g=mb_df$l8ut11174m_150615_USG_utm51pre.2, 
                    b=mb_df$l8ut11174m_150615_USG_utm51pre.1)

mb_df[!complete.cases(mb_df),]#check for zeros

ggplot(data=mb_df, aes(x=x, y=y, fill=rgb(r,g,b, maxColorValue = 255))) +
        coord_equal() + theme_bw() + geom_tile() + scale_fill_identity() +
        scale_x_continuous(breaks=range(mb_df$x)*c(1.01, 0.99), 
                           labels=range(mb_df$x), expand = c(0,0)) +
        scale_y_continuous(breaks=range(mb_df$y)*c(0.99, 1.01), 
                           labels=range(mb_df$y), expand = c(0,0)) +
        theme(panel.grid=element_blank(), 
              plot.title = element_text(size = 10))+
        labs(list(title="Landsat 8 TM derived RGB composite of Ngong Area, Kenya (UTM zone 37s)", 
                  x="Lon", y="Lat"))


ggplot(data=mb_df, aes(x=x, y=y, fill=rgb(r,g,b, maxColorValue = 255))) +
        coord_equal() + theme_bw() + geom_tile() + scale_fill_identity() +
        scale_x_continuous(breaks=range(mb_df$x)*c(1.01, 0.99), 
                           labels=range(mb_df$x), expand = c(0,0)) +
        scale_y_continuous(breaks=range(mb_df$y)*c(0.99, 1.01), 
                           labels=range(mb_df$y), expand = c(0,0)) +
        theme(panel.grid=element_blank(), 
              plot.title = element_text(size = 10))+
        labs(list(title="Landsat 8 TM derived RGB composite of Ngong Area, Kenya (UTM zone 37s)", 
                  x="Lon", y="Lat"))