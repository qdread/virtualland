library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg) 

grViz("digraph{
      compound = true;
      fontname = Helvetica;

      graph[rankdir = LR];

      node[shape = rectangle, style = filled, fontname = Helvetica]
      
      subgraph cluster_0 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = Gold
    
         label = 'Food consumed'
         node[shape = rectangle, fillcolor = LemonChiffon, margin = 0.25]
         A[label = 'USDA LAFA']
      }
      
      subgraph cluster_1 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = Gold
    
         label = 'Domestic agricultural production'
         node[shape = rectangle, fillcolor = LemonChiffon, margin = 0.25]
         B[label = 'BEA I-O table']
         node[shape = rectangle, fillcolor = Coral, margin = 0.25]
         C[label = 'USEEIO model']
      }
      
      subgraph cluster_2 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = Gold
    
         label = 'Foreign agricultural imports'
         node[shape = rectangle, fillcolor = LemonChiffon, margin = 0.25]
         D[label = 'FAOSTAT trade data']
      }
      
      subgraph cluster_3 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = Gold
    
         label = 'Domestic land footprint'
         node[shape = rectangle, fillcolor = LemonChiffon, margin = 0.25]
         E[label = 'USDA Census of Ag.']
         node[shape = rectangle, fillcolor = Coral, margin = 0.25]
         F[label = 'land satellite table']
      }
      
      subgraph cluster_4 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = Gold
    
         label = 'Foreign land footprint'
         node[shape = rectangle, fillcolor = LemonChiffon, margin = 0.25]
         G[label = 'FAOSTAT yield and FBS data']
      }
      
      subgraph cluster_678 {

      rank=same;
      style=invis;
      
      subgraph cluster_7 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = Gold
    
         label = 'Diet shift scenarios'
         node[shape = rectangle, fillcolor = LemonChiffon, margin = 0.25]
         K[label = 'USDA Dietary Guidelines']
         node[shape = rectangle, fillcolor = LemonChiffon, margin = 0.25]
         L[label = 'EAT-Lancet diet']
      }
      
        subgraph cluster_6 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = Gold
    
         label = 'Biodiversity threat footprint'
         node[shape = rectangle, fillcolor = LemonChiffon, margin = 0.25]
         I[label = 'IUCN, WWF data']
         node[shape = rectangle, fillcolor = Coral, margin = 0.25]
         J[label = 'Chaudhary model']
      }
      
      subgraph cluster_8 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = Gold
    
         label = 'Waste reduction scenarios'
         node[shape = rectangle, fillcolor = LemonChiffon, margin = 0.25]
         M[label = 'USDA LAFA']
      }
      
      }
      
      node[shape = rectangle
           style = 'rounded,filled'
           fillcolor = Gold
           margin = 0.4]
      H[label = 'Total baseline\nland footprint']
      N[label = 'Counterfactual\nfootprints']
  
      edge[color = black, arrowhead = vee, arrowsize = 1.25]
      A -> B [ltail=cluster_0, lhead=cluster_1];
      A -> D [ltail=cluster_0, lhead=cluster_2];
      B -> E [ltail=cluster_1, lhead=cluster_3];
      D -> G [ltail=cluster_2, lhead=cluster_4];
      E -> H [ltail=cluster_3];
      G -> H [ltail=cluster_4];
      H -> I [lhead=cluster_6];
      K -> N [ltail=cluster_7];
      I -> N [ltail=cluster_6];
      M -> N [ltail=cluster_8];

      }")

### Version 2 with the scenarios entering in at the beginning.

palette.colors(n = 9, palette = 'Okabe-Ito')
vir_cols <- scales::viridis_pal(alpha = 0.75)(3)

method_graphviz <- grViz("digraph{
      compound = true;
      fontname = 'Helvetica-bold';

      graph[rankdir = LR];

      node[shape = rectangle, style = filled, fontname = Helvetica]
      
      subgraph cluster_7 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = '#0072B280'
         fontsize = 18

         label = 'Diet shift\nscenarios'
         node[shape = rectangle, fillcolor = '#009E7380', margin = 0.25]
         K[label = 'USDA Dietary Guidelines']
         L[label = 'EAT-Lancet diet']
      }
      
      subgraph cluster_8 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = '#0072B280'
         fontsize = 18
    
         label = '  Waste reduction  \nscenarios'
         node[shape = rectangle, fillcolor = '#009E7380', margin = 0.25]
         M[label = 'USDA LAFA']
      }
      
      subgraph cluster_0 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = '#0072B280'
         fontsize = 18
    
         label = ' Food consumed '
         node[shape = rectangle, fillcolor = '#009E7380', margin = 0.25]
         A[label = 'USDA LAFA']
      }
      
      subgraph cluster_1 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = '#0072B280'
         fontsize = 18
         
         label = '  Domestic agricultural  \nproduction'
         node[shape = rectangle, fillcolor = '#009E7380', margin = 0.25]
         B[label = 'BEA I-O table']
         node[shape = rectangle, fillcolor = '#D55E0080', margin = 0.25]
         C[label = 'USEEIO model']
      }
      
      subgraph cluster_2 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = '#0072B280'
         fontsize = 18
    
         label = '  Foreign agricultural  \nimports'
         node[shape = rectangle, fillcolor = '#009E7380', margin = 0.25]
         D[label = 'FAOSTAT\ntrade data']
      }
      
      subgraph cluster_3 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = '#0072B280'
         fontsize = 18
    
         label = 'Domestic land\nfootprint'
         node[shape = rectangle, fillcolor = '#009E7380', margin = 0.25]
         E[label = 'USDA Census of Ag.']
         node[shape = rectangle, fillcolor = '#D55E0080', margin = 0.25]
         F[label = 'land satellite table']
      }
      
      subgraph cluster_4 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = '#0072B280'
         fontsize = 18
    
         label = 'Foreign land\nfootprint'
         node[shape = rectangle, fillcolor = '#009E7380', margin = 0.25]
         G[label = 'FAOSTAT\nyield and FBS data']
      }
      
      subgraph cluster_6 {
         graph[shape = rectangle]
         style = rounded
         bgcolor = '#0072B280'
         fontsize = 18
    
         label = 'Biodiversity threat\nfootprint'
         node[shape = rectangle, fillcolor = '#009E7380', margin = 0.25]
         I[label = 'IUCN, WWF data']
         node[shape = rectangle, fillcolor = '#D55E0080', margin = 0.25]
         J[label = 'Chaudhary & Brooks\nmodel']
      }
      
      node[shape = rectangle
           style = 'rounded,filled'
           fillcolor = '#0072B280'
           fontsize = 18
           fontname = 'Helvetica-bold'
           margin = 0.4]
      H[label = 'Total land\nfootprint']

      edge[color = black, arrowhead = vee, arrowsize = 1.25]
      K -> A [ltail=cluster_7, lhead=cluster_0];
      M -> A [ltail=cluster_8, lhead=cluster_0];
      A -> B [ltail=cluster_0, lhead=cluster_1];
      A -> D [ltail=cluster_0, lhead=cluster_2];
      B -> E [ltail=cluster_1, lhead=cluster_3];
      D -> G [ltail=cluster_2, lhead=cluster_4];
      E -> H [ltail=cluster_3];
      G -> H [ltail=cluster_4];
      H -> I [lhead=cluster_6];

      }")

write(export_svg(method_graphviz), file = 'figs/method_graph.svg')
