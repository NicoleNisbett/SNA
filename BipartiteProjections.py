#Creating bipartite projections. Need to run in ARC because too memory intensive. Only works for COP20 (uses 80G)
import networkx as nx
from networkx.algorithms import bipartite

version_list = ["COP20", "COP21", "COP22", "COP23", "COP24", "COP25", "COP26"]

for i in version_list:
    g = nx.Graph()
    user = []
    topic = []
    with open(i + "Merged.csv", "r") as f:
        f.readline()
        for l in f:
            l = l.rstrip().split(",")
            u = str(l[1])
            t = str(l[2])
            g.add_edge(u, t)
            user.append(u)
            topic.append(t)

    Ngraph_user = bipartite.weighted_projected_graph(g, user)
    nx.write_edgelist(Ngraph_user, i + "Projections.csv",  delimiter = ",")