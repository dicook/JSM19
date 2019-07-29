# Ugly code to make a pretty tour animation html file
library(tourr)
library(RColorBrewer)
library(spinifex)
library(htmltools)
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
iris_std <- iris %>% 
  mutate_if(is.numeric, scale2)
iris_std_mean <- iris_std %>% group_by(Species) %>%
  summarise_if(is.numeric, mean) # Compute means
tpath <- save_history(iris_std[,1:4], max = 10, 
                      start=matrix(c(1,0,0,1,0,0,0,0), ncol=2, byrow=TRUE))
tpath[,,1] <- matrix(c(1,0,0,1,0,0,0,0), ncol=2, byrow=TRUE)
tour_path <- interpolate(tpath, 0.1)
d <- dim(tour_path)
mydat <- NULL; myaxes <- NULL; mymeans <- NULL
for (i in 1:d[3]) {
  fp <- as.matrix(iris_std[,1:4]) %*% matrix(tour_path[,,i], ncol=2)
  fp <- tourr::center(fp)
  colnames(fp) <- c("d1", "d2")
  mydat <- rbind(mydat, cbind(fp, rep(i+10, 2*nrow(fp))))
  fa <- cbind(matrix(0, 4, 2), matrix(tour_path[,,i], ncol=2))
  colnames(fa) <- c("origin1", "origin2", "d1", "d2") 
  myaxes <- rbind(myaxes, cbind(fa, rep(i+10, 2*nrow(fa))))
  mm <- as.matrix(iris_std_mean[,2:5]) %*% matrix(tour_path[,,i], ncol=2)
  mm <- tourr::center(mm)
  colnames(mm) <- c("d1", "d2")
  mymeans <- rbind(mymeans, cbind(mm, rep(i+10, 2*nrow(mm))))
  
}
colnames(mydat)[3] <- "indx"
colnames(myaxes)[5] <- "indx"
colnames(mymeans)[3] <- "indx"
df <- as_tibble(mydat) %>% 
  mutate(species = rep(iris_std$Species, d[3]))
dfaxes <- as_tibble(myaxes) %>%
  mutate(labels=rep(colnames(iris_std[,1:4]), d[3]))
dfaxes_mat <- dfaxes %>%
  mutate(xloc = rep(max(df$d1)+1.3, d[3]*4), yloc=rep(seq(0.5, -0.5, -0.3), d[3]), 
         coef=paste(round(dfaxes$d1, 2), ", ", round(dfaxes$d2, 2)))
dfmeans <- as_tibble(mymeans) %>% 
  mutate(species = rep(iris_std_mean$Species, d[3]))
p <- ggplot() +
  geom_segment(data=dfaxes, aes(x=d1-5, xend=origin1-5, 
                                y=d2, yend=origin2, 
                                frame = indx), colour="grey70") +
  geom_text(data=dfaxes, aes(x=d1-5, y=d2, label=labels, 
                             frame = indx), colour="grey70", size=3.5) +
  geom_point(data = df, aes(x = d1, y = d2, colour=species, 
                            frame = indx), size=1.5, alpha=0.7) +
  geom_text(data=dfaxes_mat, aes(x=xloc, y=yloc, 
                                 label=coef, frame = indx), 
            hjust="inward", colour="grey70") + 
  geom_point(data=dfmeans, aes(x = d1, y = d2, colour=species, 
                               frame = indx), shape=3, size=3) + 
  scale_colour_brewer(palette = "Dark2") +
  theme_void() +
  coord_fixed() +
  theme(legend.position="none")
pg <- ggplotly(p, width=1000, height=550) %>%
  animation_opts(200, redraw = FALSE, 
                 easing = "linear", transition=0)
save_html(pg, file="iris_tour.html")
