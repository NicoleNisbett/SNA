import networkx as nx
from networkx.algorithms import bipartite
import pandas as pd

g = nx.Graph()
user = []
topic = []
with open("COP25Merged.csv", "r") as f:
    f.readline()
    for l in f:
        l = l.rstrip().split(",")
        u = str(l[1])
        t = str(l[2])
        g.add_edge(u, t)
        user.append(u)
        topic.append(t)

Ngraph_user = bipartite.weighted_projected_graph(g, user)

Ngraph_user.remove_edges_from([(n1, n2) for n1, n2, w in Ngraph_user.edges(data="weight") if w < 3])

nx.write_edgelist(Ngraph_user, "COP25ProjectionsN.csv",  delimiter = ",", data=['weight'])

file = pd.read_csv("COP25ProjectionsN.csv")
headerList = ['Source', 'Target', 'Weight']
file.to_csv("COP25ProjectionsN.csv", header=headerList, index=False)


