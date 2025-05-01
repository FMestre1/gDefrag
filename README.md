# gDefrag

**gDefrag** is an R package designed to assist in mitigating landscape fragmentation caused by linear infrastructures such as roads, railways, and pipelines. By representing landscapes as graphs, gDefrag enables users to identify and prioritise critical connections to enhance habitat connectivity.

## Overview

Landscape fragmentation poses significant challenges to biodiversity conservation. gDefrag offers a graph-based approach to model and analyse habitat connectivity, allowing for the identification of key areas where mitigation efforts can be most effective.

The package is based on the work presented in:

> Mestre, F., Ascensão, F., & Barbosa, A.M. (2019). gDefrag: A graph-based tool to help defragmenting landscapes divided by linear infrastructures. *Ecological Modelling*, 392, 1–5.\
> <https://doi.org/10.1016/j.ecolmodel.2018.10.012>

## Features

-   **Graph-Based Modeling**: Simplifies landscapes into nodes (habitat patches) and edges (connections), facilitating connectivity analysis.
-   **Prioritization Methods**: Implements multiple criteria to rank potential corridors:
    -   **Value**: Prioritizes connections between high-value habitat patches.
    -   **Betweenness**: Identifies edges that serve as critical links within the network.
    -   **Integral Index of Connectivity (IIC)**: Assesses the impact of each edge on overall connectivity.
    -   **Area-Weighted Metric (AWM)**: Considers both habitat quality and size.
-   **Shapefile Integration**: Supports input and output of spatial data in shapefile format.

## Installation

To install the latest version from GitHub:

```         
# Install devtools if not already installed 
install.packages("devtools")

# Install gDefrag from GitHub
devtools::install_github("FMestre1/gDefrag")
```
