class TemporalEdge(object):
    """ The edges used in the Temporal Network
        Every edge is unique by it's nodes.
    """
    
    def __init__(self,node1,node2):
        """ Initialize the edge without weights
            params:
                node1 : string
                            Name of the node
                node2 : string
                            Name of the node
        """
        self.node1 = node1
        self.node2 = node2
        self.occurences = dict()
    
    def add(self, weight, time):
        """ Adds the weight to the edge at timepoit 'time'
            params:
                weight : int
                            The added weight
                time : int
                            The timepoint
        """
        if time not in self.occurences:
            self.occurences[time] = weight
        else:
             self.occurences[time] += weight
    
    @property
    def nodes(self):
        """ Return a tuple of involved nodes of the edge"""
        return (self.node1,self.node2)
    
    def weight(self,t):
        """ Get the weight of the edge at timePoint 't'
            params: 
                t : int
                        The timepoint
        
        """
        curDict = {time : weight for time,weight in self.occurences.items() if time <= t}
        w = 0
        for (time,weight) in curDict.items():
            w += weight * 0.9**(t - time)
        return w
        
    def __eq__(self,other):
        if isinstance(other,type(self)):
            return other.node1 == self.node1 and other.node2 == self.node2 and other.occurences == self.occurences
        elif isinstance(other,tuple):
            if len(other) == 2:
                return self.node1 == other[0] and self.node2 == other[1]
        return False
    
    def __repr__(self):
        return "%s -> %s %s" % (self.node1,self.node2,self.occurences)