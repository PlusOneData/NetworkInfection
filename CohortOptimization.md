Graph COI Office Simulation

Cohort Optimization Formulation

Packages and software: python, NetworkX, Gurobi Optimization Solver, Gurobipy

# Background

As a result of the COVID-19 pandemic, many organizations and schools have switched to a virtual environment. Recently, as vaccines have become more available, the idea of gradually returning to the office space or classroom has surfaced. For the highest level of safety and caution with respect to the containment of COVID-19, the shift to in-person interaction requires a thoughtful approach. With the help of a Mixed Integer Programming (MIP) Optimization model, we can formulate the objective function and constraints to determine a safe way of returning to the office through cohort development.

# Input Variables and Parameters

Let graph _G_ = _(V, E)_ describe the network of individuals who work in the same office. In the model, we use a _G(n, p_) Erdos-Renyi random graph to represent graph _G_, where the number of nodes, _n =_ 40, and the probability of edge creation, _p_ = .15. In the real world, we would create an edge between two individuals in the graph if they have interacted in person over a defined period. The aim of this cohort model is to reduce unnecessary additional edges introduced to the network after establishing cohorts.

For this model, we set the parameter_, total\_cohorts_ = 3, so that we create three groups that could come in on separate days or separate times of the day. For example, we can assign Group 1 to Monday, Group 2 to Wednesday, and Group 3 to Friday. How each cohort is assigned to unique day and time combination to come into the office is flexible and not addressed in the formulation, but we do generate the cohort groups. The next parameter, _min\_cohort\_size,_ is the user defined minimum cohort size. We set _min\_cohort\_size_ = 10 for this formulation to ensure a minimum level of social interaction is achieved. The next parameter, max_\_cohort\_size_, is the user defined maximum cohort size. This can also be adjusted to satisfy capacity constraints and social distancing and gathering policy requirements. In this formulation, we set _max\_cohort\_size_ = 20 due to gathering restrictions.

Using NetworkX, we can easily generate an adjacency matrix of _G_, **A­** _ **G** _, where if **A­** _ **G** _ = [aij], then aij = 1 if there is an edge between _v __i_and _v__ j_ and otherwise aij = 0. We will define the cost matrix for this formulation as **C­** _ **G** _, where if **C** _ **G** _ = [cij], then cij = 1 if there is not an edge between _v __i_and _v__ j_ and otherwise cij = 0. This cost matrix will be incorporated in the objective function so that extra edges introduced to the solution cohorts will incur a cost.

# Decision Variables

For simplicity, in this formulation we will call the set of nodes that represent people in the network as _N_. The set of cohorts will be referred to as _C_.

There are two sets of decision variables for this formulation. First, we will introduce _X__ptc_, the binary variable which determines the creation of an edge between person _p_, teammate _t_, in cohort _c_.

Next, we introduce _Y__pc_, the binary variable which determines the assignment of person _p_ to cohort _c._

# Constraints

**Cohort Unique Assignment Constraint**

A person _p_ can only be assigned to one cohort:

**Conditional Constraints on** _ **X** __**ptc** _ **and** _ **Y**__ **pc** _

Upper and Lower Bounds to establish relationship between edge _X __ptc_ and node _Y__ pc_ existence:

The upper bound constraint below forces _X __ptc_ to zero, unless both _Y__ pc_ and _Y __tc_ have values of 1. We still require a lower bound to ensure that _X__ ptc_ will take the value of 1 when both _Y __pc_ and _Y__ tc_ have values of 1.

The lower bound constraint below forces _X __ptc_ to 1 if both _Y__ pc_ and _Y __tc_ have values of 1. Paired with the upper bound constraint, we now have a well-defined relationship between _X__ ptc_ and _Y__pc_ that can be interpreted as a network of nodes and edges in a defined space.

**Minimum Cohort Size Constraint**

Each cohort must achieve the minimum cohort size.

**Maximum Cohort Size Constraint**

Each cohort must be within the maximum cohort size.

**Binary Integer Constraints**

This MIP model is dependent on _X __ptc_ and _Y__ pc_ taking on binary integer values.

{0, 1}

{0, 1}

# Objective Function

The objective function minimizes the cost associated with introducing new edges, which represent in-person interactions.
