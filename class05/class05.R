####################################
# Author: Meg Robinson
# Class 5
# Section 5: Scatter Plots
####################################


# load the ggplot2 package after installation
library(ggplot2)

# Use the built-in 'cars' dataset for visualization 

# Look at whats in the dataset
head(cars)

# Use ggplots 3 layers: data, aes, geoms
# To visualize 'cars' as a scatterplot
# Use + to add layers
plot = ggplot(cars) +
  aes(x=speed, y=dist) + geom_point() 
plot

# Add geom_smooth() to show the relationship between variables
plot = plot + geom_smooth(method='lm', se=FALSE)
plot

plot = plot + labs(title='Speed vs Distance of Cars', 
                   x="Speed (mph)",
                   y="Distance to stop (ft)")  +
  theme_bw()
plot


# Read in the results of DE analysis
url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)

# How many genes are in the dataset?
nrow(genes)

# How many columns are in the dataset?
ncol(genes)

# What are the column names?
colnames(genes)

# How many upregulated genes are there?
table(genes$State)

# What percent of up regulated genes?
round(table(genes$State)/nrow(genes)*100,2)

# Make a scatter plot of the genes df
ggplot(genes) + 
  aes(x=Condition1, y=Condition2) +
  geom_point()

# Map the the column  State to color
gene_plot = ggplot(genes) + 
  aes(x=Condition1, y=Condition2, col=State) +
  geom_point()
gene_plot



# add titles etc
gene_plot = gene_plot + theme_bw() + scale_colour_manual(values=c("blue","gray","red")) +
  labs(title="Gene Expresion Changes Upon Drug Treatment",
       x="Control (no drug) ",
       y="Drug Treatment")
gene_plot

