library(DiagrammeR)

grViz("
  digraph {
   AA [label=  'Monitoring program']
    A [label = 'Field Data', shape = rectangle]
    B [label = 'Data Type?', shape = diamond]
    BB[label = 'SMART Database', shape = rectangle]
    C [label = 'DEIMS-SDR', shape = diamond]
    D [label = 'GD-PAME', shape = rectangle]
    E [label = 'GBIF', shape = rectangle]
    F [label = 'Internal Research', shape = rectangle]
    G [label = 'National Reports', shape = rectangle]
    H [label = 'Miradi', shape = rectangle]
    K [label = 'Share result', shape = rectangle]
    I [label = 'Conservation Actions', shape = rectangle]
    
    
    
    AA -> A [label = 'Setting goals']
    A -> B
    B -> BB [label = 'Threat/Occurrence', width = 0.01, height = 0.01]
    B -> C [label = 'Species/Plots', shape = rectangle]
    B -> D [label = 'PA Metrics', shape = rectangle]
    C -> F [label = 'Restricted']
    C -> E [label = 'Open Data']
    BB -> F
    D -> G
    E -> H
    E -> K
    F -> K
    F -> H
    G -> H
    H -> I
    K -> I
    I -> AA
  }
")
