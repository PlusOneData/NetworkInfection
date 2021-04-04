Cohort Optimization Formulation
Packages and software: python, NetworkX, Gurobi Optimization Solver, Gurobipy

Background
As a result of the COVID-19 pandemic, many organizations and schools have switched to a virtual environment. Recently, as vaccines have become more available, the idea of gradually returning to the office space or classroom has surfaced. For the highest level of safety and caution with respect to the containment of COVID-19, the shift to in-person interaction requires a thoughtful approach. With the help of a Mixed Integer Programming (MIP) Optimization model, we can formulate the objective function and constraints to determine a safe way of returning to the office through cohort development. 

Input Variables and Parameters
Let graph G = (V, E) describe the network of individuals who work in the same office. In the model, we use a G(n, p) Erdos-Renyi random graph to represent graph G, where the number of nodes, n = 40, and the probability of edge creation, p = .15. In the real world, we would create an edge between two individuals in the graph if they have interacted in person over a defined period. The aim of this cohort model is to reduce unnecessary additional edges introduced to the network after establishing cohorts. 
For this model, we set the parameter, total_cohorts = 3, so that we create three groups that could come in on separate days or separate times of the day. For example, we can assign Group 1 to Monday, Group 2 to Wednesday, and Group 3 to Friday. How each cohort is assigned to unique day and time combination to come into the office is flexible and not addressed in the formulation, but we do generate the cohort groups. The next parameter, min_cohort_size, is the user defined minimum cohort size. We set min_cohort_size = 10 for this formulation to ensure a minimum level of social interaction is achieved. The next parameter, max_cohort_size, is the user defined maximum cohort size. This can also be adjusted to satisfy capacity constraints and social distancing and gathering policy requirements. In this formulation, we set max_cohort_size = 20 due to gathering restrictions. 
Using NetworkX, we can easily generate an adjacency matrix of G, A¬G, where if A¬G = [aij], then aij = 1 if there is an edge between vi and vj and otherwise aij = 0. We will define the cost matrix for this formulation as C¬G, where if CG = [cij], then cij = 1 if there is not an edge between vi and vj and otherwise cij = 0. This cost matrix will be incorporated in the objective function so that extra edges introduced to the solution cohorts will incur a cost. 

 
Decision Variables
For simplicity, in this formulation we will call the set of nodes that represent people in the network as N. The set of cohorts will be referred to as C. 
There are two sets of decision variables for this formulation. First, we will introduce Xptc, the binary variable which determines the creation of an edge between person p, teammate t, in cohort c. 
X_{ptc}\ =\ 1  0 person p and teammate t contain an edge in cohort c  otherwise,∀p∈N,t∈N,c∈C  
	
Next, we introduce Ypc, the binary variable which determines the assignment of person p to cohort c.
Y_{pc}\ =\ 1  0 person p is assigned to cohort c  otherwise,∀p∈N,c∈C



Constraints
Cohort Unique Assignment Constraint
A person p can only be assigned to one cohort:
\sum_{c\ \in\ C\ } Y_{pc}\ =\ 1\ ,\ \ \forall p\in N

Conditional Constraints on Xptc and Ypc
Upper and Lower Bounds to establish relationship between edge Xptc and node Ypc existence:
The upper bound constraint below forces Xptc to zero, unless both Ypc and Ytc have values of 1. We still require a lower bound to ensure that Xptc will take the value of 1 when both Ypc and Ytc have values of 1.
X_{ptc}\ \le\ \frac{Y_{pc}\ +\ Y_{tc}}{2},\ \ \forall p\in N,\ \ t\in N,\ \ c\in C\  
The lower bound constraint below forces Xptc to 1 if both Ypc and Ytc have values of 1. Paired with the upper bound constraint, we now have a well-defined relationship between Xptc and Ypc that can be interpreted as a network of nodes and edges in a defined space.
X_{ptc}\ \geq\ Y_{pc}\ +\ Y_{tc}\ -\ 1,\ \ \forall p\in N,\ \ t\in N,\ \ c\in C\  

Minimum Cohort Size Constraint
Each cohort must achieve the minimum cohort size.
\sum_{p\ \in\ P\ } Y_{pc}\geq min_cohort_size,\forall c\in C

Maximum Cohort Size Constraint
Each cohort must be within the maximum cohort size.

\sum_{p\ \in\ P\ } Y_{pc}\le max_cohort_size,\forall c\in C

Binary Integer Constraints
This MIP model is dependent on Xptc and Ypc taking on binary integer values.
X_{ptc}\in {0, 1},\ \forall p\in N,\ \ t\in N,\ \ c\in C\  
Y_{pc}\in\ {0, 1},\ \ \forall p\in N,\ \ c\in C

Objective Function
The objective function minimizes the cost associated with introducing new edges, which represent in-person interactions. 
minimize\ \sum_{c\ \in\ C}\ \sum_{t\ \in\ P}\ \sum_{\begin{matrix}p\ >\ t,\ \\p\ \in\ P\\\end{matrix}\ }{c_{pt}X}_{ptc},\ \forall p\in N,\ \ t\in N,\ \ c\in C\ 
