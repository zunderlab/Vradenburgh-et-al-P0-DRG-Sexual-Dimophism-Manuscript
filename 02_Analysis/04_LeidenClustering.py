# -*- coding: utf-8 -*-
"""
Created on Fri Apr  5 12:01:04 2019
@author: Corey Williams
"""
## Import packages
import pandas as pd
import numpy as np
import leidenalg
import igraph as ig
import csv
import os

## Input variables
INPUT_FOLDER = "/Users/" #if not on Rivanna, change to target director
INDEXES_FILENAME = "/UMAP_knn_indexes.csv"
DISTANCES_FILENAME = "/UMAP_knn_distances.csv"
CLUSTERS_FILENAME = "/clusters.csv"

## Read input files
knn_indexes = pd.read_csv(INPUT_FOLDER+INDEXES_FILENAME,
                          header=None).values[:,1:]
knn_distances = pd.read_csv(INPUT_FOLDER+DISTANCES_FILENAME,
                            header=None).values[:,1:]

## Do Leiden clustering
#Prep graph
vcount = np.size(knn_indexes,0)
sources = np.repeat(range(np.size(knn_indexes,0)),np.size(knn_indexes,1))
targets = np.ravel(knn_indexes) - 1
edgelist = list(zip([int(x) for x in sources], [int(x) for x in targets]))
weights = np.ravel(1/knn_distances)
weights = np.array(weights) #Need to convert into a form appropriate for igraph
leiden_graph = ig.Graph(vcount, edgelist, edge_attrs={'weight': weights})

#Do Leiden clustering
leiden_out = leidenalg.find_partition(leiden_graph,
                                      leidenalg.ModularityVertexPartition,
                                      n_iterations=-1,seed=1)
    
## Output csv file
with open((INPUT_FOLDER + CLUSTERS_FILENAME),'w',newline='') as csvfile:
    cluster_writer = csv.writer(csvfile, delimiter = ',')
    cluster_writer.writerow(leiden_out.membership)
