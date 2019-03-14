### Rundown of the Prim's and Kruskal's algorithm

The development process for implementing Prim's and Kruskal's algorithm was similar to that of assignment two.
Naturally, I built out the graph class first, designing it primarily for use with Prim's algorithm
as its implementation was the minimum requirement. I tried to take Kruskal's algorithm into consideration
when designing the graph class, yet ended up doing some heavy refactoring once I started to work on, and understand
the problem more clearly.

For Prim's algorithm I started by reading chapter 9.1 in _Introduction to the Design and Analysis of Algorithms_
as well as looking back on past notes from class. Neither of those really helped me get a grasp on the implications 
of the implementation. I then Googled around a bit and came across an [article](https://www.researchgate.net/publication/275645125_Weighted_graph_algorithms_with_Python) 
that I thought did a good job of explaining both Prim's and Kruskal's algorithms clearly. Another source I came across 
that _really_ helped me understand the implementation details of again, both [Prim's](https://www-m9.ma.tum.de/graph-algorithms/mst-prim/index_en.html)
 and [Kruskal's](https://www-m9.ma.tum.de/graph-algorithms/mst-kruskal/index_en.html) algorithm's was a site that came from the 
_Technical University of Munchen_ which had pages that walked through each algorithms psuedocode while showing 
a visual representation of how the algorithm was interacting with the graph. 

I chose to use Python as my programming language for a couple reasons. The first and arguably most important to me, is
that I have a lot of experience testing with it. Both writing tests as well as utilizing CI systems to run them 
(I also was able to reuse a lot of the work I had done on assignment two). Another big reason I stuck with it
 was that I'm quite familiar with its built in data types and their use cases. Specifically, `dict`s, `list`s, and `set`s.
 All of which I utilized heavily on this assignment.
 
 
 In fact, before I even began implementing, I sat down in front of a whiteboard and drew what I imagined a graph datastructure
 would look like. As it turns out, implementing it was very natural to do with `dict`s. So that's what I used. A more in depth
 overview of how I used them can be found in the `Graph` classes docstring. 
 
 ### Example output
 
 ```bash
 $ python main.py
 
 Prim's algorithm started traversal at Portland

Moved from Portland -> Gresham
	(travelled 0 so far)
Moved from Portland -> Forest Grove
	(travelled 14 so far)
Moved from Portland -> Newberg
	(travelled 37 so far)
Moved from Newberg -> Mcminnville
	(travelled 60 so far)
Moved from Newberg -> Woodburn
	(travelled 74 so far)
Moved from Woodburn -> Salem
	(travelled 93 so far)
Moved from Salem -> Albany
	(travelled 110 so far)
Moved from Albany -> Corvallis
	(travelled 134 so far)
Moved from Corvallis -> Eugene
	(travelled 145 so far)
Moved from Eugene -> Springfield
	(travelled 185 so far)
Moved from Forest Grove -> Tillamook
	(travelled 189 so far)
Moved from Corvallis -> Newport
	(travelled 241 so far)
Moved from Newport -> Florence
	(travelled 294 so far)
Moved from Florence -> Coos Bay
	(travelled 344 so far)
Moved from Tillamook -> Astoria
	(travelled 392 so far)
Moved from Springfield -> Roseburg
	(travelled 458 so far)
Moved from Roseburg -> Grants Pass
	(travelled 526 so far)
Moved from Grants Pass -> Medford
	(travelled 594 so far)
Moved from Medford -> Ashland
	(travelled 623 so far)
Moved from Ashland -> Klamath Falls
	(travelled 635 so far)
Moved from Gresham -> The Dalles
	(travelled 699 so far)
Moved from The Dalles -> Redmond
	(travelled 772 so far)
Moved from Redmond -> Bend
	(travelled 886 so far)
Moved from The Dalles -> Pendleton
	(travelled 902 so far)
Moved from Pendleton -> La Grande
	(travelled 1027 so far)
Moved from La Grande -> Baker City
	(travelled 1079 so far)
Moved from Baker City -> Ontario
	(travelled 1123 so far)
Moved from Ontario -> Burns
	(travelled 1195 so far)

Total distance traveled using Prim's was 1325

Kruskal's algorithm started traversal at Eugene

Moved from Eugene -> Springfield
	(travelled 0 so far)
Moved from Albany -> Corvallis
	(travelled 4 so far)
Moved from Medford -> Ashland
	(travelled 15 so far)
Moved from Portland -> Gresham
	(travelled 27 so far)
Moved from Newberg -> Mcminnville
	(travelled 41 so far)
Moved from Bend -> Redmond
	(travelled 55 so far)
Moved from Woodburn -> Salem
	(travelled 71 so far)
Moved from Woodburn -> Newberg
	(travelled 88 so far)
Moved from Portland -> Forest Grove
	(travelled 107 so far)
Moved from Portland -> Newberg
	(travelled 130 so far)
Moved from Albany -> Salem
	(travelled 153 so far)
Moved from Grants Pass -> Medford
	(travelled 177 so far)
Moved from Corvallis -> Eugene
	(travelled 206 so far)
Moved from La Grande -> Baker City
	(travelled 246 so far)
Moved from Florence -> Coos Bay
	(travelled 290 so far)
Moved from Newport -> Florence
	(travelled 338 so far)
Moved from La Grande -> Pendleton
	(travelled 388 so far)
Moved from Tillamook -> Forest Grove
	(travelled 440 so far)
Moved from Corvallis -> Newport
	(travelled 492 so far)
Moved from Klamath Falls -> Ashland
	(travelled 545 so far)
Moved from Astoria -> Tillamook
	(travelled 609 so far)
Moved from Grants Pass -> Roseburg
	(travelled 675 so far)
Moved from Roseburg -> Springfield
	(travelled 743 so far)
Moved from Ontario -> Baker City
	(travelled 811 so far)
Moved from The Dalles -> Gresham
	(travelled 883 so far)
Moved from The Dalles -> Redmond
	(travelled 956 so far)
Moved from The Dalles -> Pendleton
	(travelled 1070 so far)
Moved from Ontario -> Burns
	(travelled 1195 so far)

Total distance traveled using Kruskal's was 1325
 ```