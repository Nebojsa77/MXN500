#Making a Venn Diagram
library(VennDiagram)
draw.pairwise.venn(area1 = 0.5, area2 = 0.6, cross.area = 0.2,
                   category = c("Electric Playground", "Family"),
                   lty = rep("blank", 2),
                   fill = c("light blue", "pink"),
                   alpha = rep(0.5, 2),
                   cat.pos = c(0,0), cat.dist = rep(0.025, 2), scaled = F)

#making a wierd ass diagramm
library(DiagrammeR)
nodes <- create_node_df(n = 10, type = "number", 
         label = c("", "<=18", "19-40", ">=41", "<=5", ">5", "<=5", ">5", "<=5", ">5"))
         edges <- create_edge_df(from = c(1, 1, 1, 2, 2, 3, 3, 4, 4 ),
                                 to = c(2, 3, 4, 5, 6, 7, 8, 9, 10),
                                 label = c("Y", "M", "O", "0.6", "0.4", "0.7", "0.3", "0.8", "0.2"),
                                 rel = "leading to")
graph <- create_graph(nodes_df = nodes, edges_df = edges,
                      attr_theme= NULL)
# View the graph
render_graph(graph)

rand_df <- data.frame( x = runif(n  = 1000, min = 1, max = 6))

rdf <- mutate(rand_df, x = round(x, digits = 0))

ggplot(data = rand_df, aes(x=x)) +geom_histogram(fill = "Dark Blue", color = "Black") +
  theme_classic()

ggplot(data = rdf, aes(x=x)) +geom_histogram(fill = "Dark Blue", color = "Black") +
  theme_classic()


#create a function that randomly generates a number 1-6

die <- 








