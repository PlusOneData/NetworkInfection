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

Let graph *G* = *(V, E)* describe the network of individuals who work in
the same office. In the model, we use a *G(n, p*) Erdos-Renyi random
graph to represent graph *G*, where the number of nodes, *n =* 40, and
the probability of edge creation, *p* = .15. In the real world, we would
create an edge between two individuals in the graph if they have
interacted in person over a defined period. The aim of this cohort model
is to reduce unnecessary additional edges introduced to the network
after establishing cohorts.

For this model, we set the parameter*, total_cohorts* = 3, so that we
create three groups that could come in on separate days or separate
times of the day. For example, we can assign Group 1 to Monday, Group 2
to Wednesday, and Group 3 to Friday. How each cohort is assigned to
unique day and time combination to come into the office is flexible and
not addressed in the formulation, but we do generate the cohort groups.
The next parameter, *min_cohort_size,* is the user defined minimum
cohort size. We set *min_cohort_size* = 10 for this formulation to
ensure a minimum level of social interaction is achieved. The next
parameter, max*\_cohort_size*, is the user defined maximum cohort size.
This can also be adjusted to satisfy capacity constraints and social
distancing and gathering policy requirements. In this formulation, we
set *max_cohort_size* = 20 due to gathering restrictions.

Using NetworkX, we can easily generate an adjacency matrix of *G*,
**A­*~G~***, where if **A­*~G~*** = \[a~ij~\], then a~ij~ = 1 if there
is an edge between *v~i~* and *v~j\ ~*and otherwise a~ij~ = 0. We will
define the cost matrix for this formulation as **C­*~G~***, where if
**C*~G~*** = \[c~ij~\], then c~ij~ = 1 if there is not an edge between
*v~i~* and *v~j~* and otherwise c~ij~ = 0. This cost matrix will be
incorporated in the objective function so that extra edges introduced to
the solution cohorts will incur a cost.

# Decision Variables

For simplicity, in this formulation we will call the set of nodes that
represent people in the network as *N*. The set of cohorts will be
referred to as *C*.

There are two sets of decision variables for this formulation. First, we
will introduce *X~ptc~*, the binary variable which determines the
creation of an edge between person *p*, teammate *t*, in cohort *c*.

> $X_{\text{ptc}}\  = \ \left\{ \begin{matrix}
> 1\ \  \\
> 0\  \\
> \end{matrix}\begin{matrix}
> \text{person}\text{\ p}\text{\ and\ teammate\ }\text{t\ contain\ an\ edge\ in\ cohort\ c\ }\  \\
> \text{otherwise} \\
> \end{matrix} \right.\ ,\text{\ \ }\forall p \in N,\ \ t \in N,\ \text{\ c} \in C\ $

Next, we introduce *Y~pc~*, the binary variable which determines the
assignment of person *p* to cohort *c.*

$$Y_{\text{pc}}\  = \ \left\{ \begin{matrix}
1\ \  \\
0\  \\
\end{matrix}\begin{matrix}
\text{person}\text{\ p}\ \text{is\ a}\text{ssigned\ to\ }\text{cohort\ c\ }\  \\
\text{otherwise} \\
\end{matrix} \right.\ ,\ \ \forall p \in N,\ \ c \in C$$

# Constraints

**Cohort Unique Assignment Constraint**

A person *p* can only be assigned to one cohort:

$$\sum_{\text{c\ } \in \ C\ }^{}Y_{\text{pc}}\  = \ 1\ ,\ \ \forall p \in N$$

**Conditional Constraints on *X~ptc~* and *Y~pc~***

Upper and Lower Bounds to establish relationship between edge *X~ptc~*
and node *Y~pc~* existence:

The upper bound constraint below forces *X~ptc~* to zero, unless both
*Y~pc~* and *Y~tc~* have values of 1. We still require a lower bound to
ensure that *X~ptc~* will take the value of 1 when both *Y~pc~* and
*Y~tc~* have values of 1.

> $X_{\text{ptc}}\  \leq \ \frac{Y_{\text{pc}}\  + \ Y_{\text{tc}}}{2},\text{\ \ }\forall p \in N,\ \ t \in N,\ \text{\ c} \in C\ $

The lower bound constraint below forces *X~ptc~* to 1 if both *Y~pc~*
and *Y~tc~* have values of 1. Paired with the upper bound constraint, we
now have a well-defined relationship between *X~ptc~* and *Y~pc~* that
can be interpreted as a network of nodes and edges in a defined space.

> $X_{\text{ptc}}\  \geq \ Y_{\text{pc}}\  + \ Y_{\text{tc}}\  - \ 1,\ \ \forall p \in N,\ \ t \in N,\ \text{\ c} \in C\ $

**Minimum Cohort Size Constraint**

Each cohort must achieve the minimum cohort size.

$$\sum_{p\  \in \ P\ }^{}Y_{\text{pc}} \geq \text{mi}n\_ c\text{ohor}t\_ si\text{ze},\forall c \in C$$

**Maximum Cohort Size Constraint**

Each cohort must be within the maximum cohort size.

$$\sum_{p\  \in \ P\ }^{}Y_{\text{pc}} \leq m\text{ax}\_ c\text{ohor}t\_ si\text{ze},\forall c \in C$$

**Binary Integer Constraints**

This MIP model is dependent on *X~ptc~* and *Y~pc~* taking on binary
integer values.

> $X_{\text{ptc}} \in$ {0,
> 1}$,\ \forall p \in N,\ \ t \in N,\ \text{\ c} \in C\ $
>
> $Y_{\text{pc}} \in \ ${0, 1}$,\ \ \forall p \in N,\ \ c \in C$

# Objective Function

The objective function minimizes the cost associated with introducing
new edges, which represent in-person interactions.

$$\text{minimize}\ \sum_{c\  \in \ C}^{}\ \sum_{t\  \in \ P}^{}\ \sum_{\begin{matrix}
p\  > \ t,\  \\
p\  \in \ P \\
\end{matrix}\ }^{}{c_{\text{pt}}X}_{\text{ptc}},\ \forall p \in N,\ \ t \in N,\ \text{\ c} \in C\ $$
