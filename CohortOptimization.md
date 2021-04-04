
Graph COI Office Simulation

Cohort Optimization Formulation

Packages and software: python, NetworkX, Gurobi Optimization Solver,
Gurobipy

# Background

As a result of the COVID-19 pandemic, many organizations and schools
have switched to a virtual environment. Recently, as vaccines have
become more available, the idea of gradually returning to the office
space or classroom has surfaced. For the highest level of safety and
caution with respect to the containment of COVID-19, the shift to
in-person interaction requires a thoughtful approach. With the help of a
Mixed Integer Programming (MIP) Optimization model, we can formulate the
objective function and constraints to determine a safe way of returning
to the office through cohort development.

# Input Variables and Parameters

Let graph G = (V, E) describe the network of individuals who work in the same office. In the model, we use a G(n, p) Erdos-Renyi random graph to represent graph G, where the number of nodes, n = 40, and the probability of edge creation, p = .15. In the real world, we would create an edge between two individuals in the graph if they have interacted in person over a defined period. The aim of this cohort model is to reduce unnecessary additional edges introduced to the network after establishing cohorts. <br>

For this model, we set the parameter, total_cohorts = 3, so that we create three groups that could come in on separate days or separate times of the day. For example, we can assign Group 1 to Monday, Group 2 to Wednesday, and Group 3 to Friday. How each cohort is assigned to unique day and time combination to come into the office is flexible and not addressed in the formulation, but we do generate the cohort groups. The next parameter, min_cohort_size, is the user defined minimum cohort size. We set min_cohort_size = 10 for this formulation to ensure a minimum level of social interaction is achieved. The next parameter, max_cohort_size, is the user defined maximum cohort size. This can also be adjusted to satisfy capacity constraints and social distancing and gathering policy requirements. In this formulation, we set max_cohort_size = 20 due to gathering restrictions. <br> 

Using NetworkX, we can easily generate an adjacency matrix of G, A<sub>G</sub>, where if A_G = [a<sub>ij</sub>], then a<sub>ij</sub> = 1 if there is an edge between v<sub>i</sub> and v<sub>j</sub> and otherwise a<sub>ij</sub> = 0. We will define the cost matrix for this formulation as C<sub>G</sub>, where if C<sub>G</sub> = [c<sub>ij</sub>] then c<sub>ij</sub> = 1 if there is not an edge between v<sub>i</sub> and v<sub>j</sub> and otherwise c<sub>ij</sub> = 0. This cost matrix will be incorporated in the objective function so that extra edges introduced to the solution cohorts will incur a cost. <br>


# Decision Variables
For simplicity, in this formulation we will call the set of nodes that represent people in the network as N. The set of cohorts will be referred to as C. 
There are two sets of decision variables for this formulation. <br>

First, we will introduce X<sub>ptc</sub, the binary variable which determines the creation of an edge between person p, teammate t, in cohort c. <br>
	![image](https://user-images.githubusercontent.com/46353487/113512729-f9b21380-9533-11eb-9cf6-b977869cfb1d.png)

Next, we introduce Y<sub>pc</sub, the binary variable which determines the assignment of person p to cohort c. <br>
	![image](https://user-images.githubusercontent.com/46353487/113512760-13ebf180-9534-11eb-8d16-57399f948682.png)


# Constraints
**Cohort Unique Assignment Constraint**
A person p can only be assigned to one cohort:<br>
	![image](https://user-images.githubusercontent.com/46353487/113512794-43026300-9534-11eb-8454-59d17938322a.png)


**Conditional Constraints on Xptc and Ypc** 
Upper and Lower Bounds to establish relationship between edge X<sub>ptc</sub and node Y<sub>pc</sub existence:<br>
The upper bound constraint below forces X<sub>ptc</sub to zero, unless both Y<sub>pc</sub and Y<sub>tc</sub have values of 1. We still require a lower bound to ensure that X<sub>ptc</sub will take the value of 1 when both Y<sub>pc</sub and Y<sub>tc</sub have values of 1.<br>
	![image](https://user-images.githubusercontent.com/46353487/113512801-4c8bcb00-9534-11eb-8325-74f911393058.png)


The lower bound constraint below forces X<sub>ptc</sub to 1 if both Y<sub>pc</sub and Y<sub>tc</sub have values of 1. Paired with the upper bound constraint, we now have a well-defined relationship between X<sub>ptc</sub and Y<sub>pc</sub that can be interpreted as a network of nodes and edges in a defined space.<br>
	![image](https://user-images.githubusercontent.com/46353487/113512812-57466000-9534-11eb-827d-2681fcdf5c38.png)


**Minimum Cohort Size Constraint**
Each cohort must achieve the minimum cohort size.<br>
	![image](https://user-images.githubusercontent.com/46353487/113512961-329eb800-9535-11eb-8e11-1bffe4b24fa5.png)


**Maximum Cohort Size Constraint**
Each cohort must be within the maximum cohort size.<br>
	![image](https://user-images.githubusercontent.com/46353487/113512819-675e3f80-9534-11eb-8d01-d2b00aa7fdc5.png)


**Binary Integer Constraints**
This MIP model is dependent on X<sub>ptc</sub and Y<sub>pc</sub taking on binary integer values.<br>
	![image](https://user-images.githubusercontent.com/46353487/113512825-6f1de400-9534-11eb-98ed-a37a5075d63c.png)


# Objective Function

The objective function minimizes the cost associated with introducing
new edges, which represent in-person interactions.<br>
	![image](https://user-images.githubusercontent.com/46353487/113512827-77761f00-9534-11eb-8cf2-34c31956a5f8.png)

