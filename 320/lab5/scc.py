# An implementation, in Python, of Kosaraju's Algorithm for computing
# the Strongly Connected Components (SCCs) of a directed graph.  See
# https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm for more details.
#
# Run this program with the command: python3 scc.py
#
# For CS 320, Principles of Programming Languages, Spring 2019
# Mark P. Jones

def bfs(graph, start):
    visited = []
    queue = [start]

    while queue:
        node = queue.pop()
        for neighbor in graph[node]:
            if neighbor not in visited:
                queue.append(neighbor)    
        visited.append(node)

    return visited

# print(bfs(graph, "b"))

def flipGraph(graph):
    flipped = {}
    for v in graph.keys():
        flipped[v] = set()
    for v in graph.keys():
        for w in graph[v]:     # for each edge v -> w in graph
            flipped[w].add(v)  # add reverse edge w -> v to flipped
    return flipped

def dfs(graph):
    visited  = set()
    finished = []
    def visit(v):
        if v not in visited:
            visited.add(v)
            for w in graph[v]:
                visit(w)
            finished.append(v)
    for v in graph.keys():
        visit(v)
    return finished

def scc(graph):
    #print('calculating strongly connected components of', g)
    vertices = dfs(graph)
    vertices.reverse()
    #print('vertices in reverse order of finishing', vertices)
    flipped  = flipGraph(graph)
    #print('flipped graph is', flipped)

    visited = set()
    def visit(v, scc):
        visited.add(v)
        scc.append(v)
        for w in flipped[v]:
            if w not in visited:
                visit(w, scc)

    sccs = []
    for v in vertices:
        if v not in visited:
            scc = []
            visit(v, scc)
            sccs.append(scc)

    return sccs

def testScc(graph):
    print('strongly connected components of ', graph)
    print('are', scc(graph))

def toDot(filename, graph):
    with open(filename, 'w') as file:
        file.write('digraph G {\n')
        for v in graph.keys():
            for w in graph[v]:
                file.write('  ' + v + " -> " + w + ';\n')
        file.write('}\n')

# Testing: ----------------------------------------------------

graph = {'d' : {'e'},
         'a' : {'b', 'c'},
         'e' : {'f'},
         'c' : {'b'},
         'f' : {'g', 'd'},
         'b' : {'c', 'd'},
         'g' : set()}

other = {'a' : {'b', 'c'},
         'b' : {'d', 'e'},
         'c' : {'e', 'f'},
         'd' : {'a'},
         'e' : {'g', 'h'},
         'f' : {'a'},
         'g' : {'e'},
         'h' : {'e'}}

# To generate a pdf version of a graph, uncomment the following
# call to toDot(), and then, after you have run this program,
# convert the generated graph.dot file into a pdf file using the
# command:   dot -Tpdf graph.dot > graph.pdf
# For (lots) more information about dot, visit graphviz.org.
#
#toDot('graph.dot', graph)
#toDot('flipped.dot', flipGraph(graph))

# Test with the graph from the lecture:
testScc(graph)
print()

# Test with the other graph defined above:
testScc(other)
print()

# A simple graph:  a ---> b ---> c
testScc({'a': {'b'}, 'b':['c'], 'c':[] })
print()


print(bfs(other, "b"))

# Demonstrate a runtime type error:
#print('Be not alarmed; the following will trigger an error!')
#testScc({'a': {'b'}, 'b':3, 'c':[] })

