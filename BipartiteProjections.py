import networkx as nx
from networkx.algorithms import bipartite

g = nx.Graph()
user = []
topic = []

with open("Trimester Bipartite/SepDec17.csv", "r") as f:
    f.readline()
    for l in f:
        l = l.rstrip().split(",")
        u = str(l[0])
        t = str(l[3])
        g.add_edge(u, t)
        user.append(u)
        topic.append(t)


Ngraph_user = bipartite.weighted_projected_graph(g, user)
nx.write_edgelist(Ngraph_user, "Proj_SepDec17.csv")