import networkx as nx
from temporal_edge import TemporalEdge
class TemporalCoAuthorNetwork(object):
    
    def __init__(self):
        """ Initialize the TemporalCoAuthorNetwork """
        self.edges = []
            
    def __repr__(self):
        return str(self.edges)
    
    def addEdge(self,node1,node2,w,t):
        """
            Add an edge between node1 and node2 with weight w at time t
            params:
                node1 : string
                            Name of the node
                node2 : string
                            Name of the node
                w : int
                            The weight
                t : int
                            The timepoint
        """
        node1, node2 = min(node1,node2),max(node1,node2)
        if (node1,node2) not in self.edges:
            edge = TemporalEdge(node1,node2)
            self.edges.append(edge)
        i = self.edges.index((node1,node2))
        self.edges[i].add(w,t)
    
    def networkAtTime(self,t):
        """
            Get a networkx Graph-object representation of the network at time 't'
            params:
                t : int
                        The time at which to show the network
        """
        N = nx.Graph()
        for edge in self.edges:
            w = edge.weight(t)
            if w:
                N.add_edge(*edge.nodes,weight = w)
        return N
