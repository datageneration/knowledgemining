# R Programming (EDA)
# Adapted from Stackoverflow examples
install.packages("plotly")
library(plotly)
data(iris)
attach(iris)
# Generate plot on three quantitative variables
iris_plot <- plot_ly(iris, 
                     x = Sepal.Length, 
                     y = Sepal.Width, 
                     z = Petal.Length, 
                     type = "scatter3d", 
                     mode = "markers",
                     size = 0.01)
iris_plot
# Regression object

petal_lm <- lm(Petal.Length ~ 0 + Sepal.Length + Sepal.Width, 
               data = iris)
install.packages("reshape2")
library(reshape2)

#load data

petal_lm <- lm(Petal.Length ~ 0 + Sepal.Length + Sepal.Width,data = iris)

# Setting resolution parameter
graph_reso <- 0.05

#Setup Axis
axis_x <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), by = graph_reso)
axis_y <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), by = graph_reso)

# Regression surface
# Rearranging data for plotting
petal_lm_surface <- expand.grid(Sepal.Length = axis_x,Sepal.Width = axis_y,KEEP.OUT.ATTRS = F)
petal_lm_surface$Petal.Length <- predict.lm(petal_lm, newdata = petal_lm_surface)
petal_lm_surface <- acast(petal_lm_surface, Sepal.Width ~ Sepal.Length, value.var = "Petal.Length") 
hcolors=c("orange","blue","green")[iris$Species]
iris_plot <- plot_ly(iris, 
                     x = ~Sepal.Length, 
                     y = ~Sepal.Width, 
                     z = ~Petal.Length,
                     text = Species, 
                     type = "scatter3d", 
                     mode = "markers",
                     marker = list(color = hcolors),
                     size=0.01)
# Add surface
iris_plot <- add_trace(p = iris_plot,
                       z = petal_lm_surface,
                       x = axis_x,
                       y = axis_y,
                       type = "surface",mode = "markers",
                       marker = list(color = hcolors))
iris_plot
